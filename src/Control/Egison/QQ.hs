-- | Quasiquotation for rewriting a match clause.

module Control.Egison.QQ
  ( mc
  )
where

import           Control.Egison.Core
import           Control.Monad                  ( (<=<) )
import           Control.Monad.State            ( runState
                                                , get
                                                , modify
                                                )
import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( foldl' )
import           Data.Functor.Foldable          ( Recursive
                                                , Base
                                                , cata
                                                )
import           Language.Haskell.TH            ( Q
                                                , Loc(..)
                                                , Exp(..)
                                                , Pat(..)
                                                , Lit(..)
                                                , Name
                                                , location
                                                , extsEnabled
                                                , mkName
                                                , pprint
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )
import qualified Language.Haskell.TH           as TH
                                                ( Extension(..) )
import           Language.Haskell.Exts.Extension
                                                ( Extension(EnableExtension) )
import           Language.Haskell.Exts.Parser   ( ParseResult(..)
                                                , defaultParseMode
                                                , parseExpWithMode
                                                )
import qualified Language.Haskell.Exts.Extension
                                               as Exts
                                                ( KnownExtension(..) )
import qualified Language.Haskell.Exts.Parser  as Exts
                                                ( ParseMode(..) )
import           Language.Haskell.Meta.Syntax.Translate
                                                ( toExp )
import qualified Language.Egison.Syntax.Pattern
                                               as Pat
                                                ( Expr
                                                , ExprF(..)
                                                )
import qualified Language.Egison.Parser.Pattern
                                               as Pat
                                                ( parseNonGreedy )
import           Language.Egison.Parser.Pattern ( Fixity(..)
                                                , ParseFixity(..)
                                                , Associativity(..)
                                                , Precedence(..)
                                                )
import           Language.Egison.Parser.Pattern.Mode.Haskell.TH
                                                ( ParseMode(..) )

-- | A quasiquoter for rewriting a match clause.
-- This quasiquoter is useful for generating a 'MatchClause' in user-friendly syntax.
-- 
-- === Wildcards
-- 
-- A match clause that contains a wildcard
-- 
-- > [mc| _ => "Matched" |]
-- 
-- is rewritten to
-- 
-- > MatchClause Wildcard
-- >             (\HNil -> "Matched")
-- 
-- === Pattern variables
-- 
-- A match clause that contains a pattern variable
-- 
-- > [mc| $x => x |]
-- 
-- is rewritten to
-- 
-- > MatchClause (PatVar "x")
-- >             (\HCons x HNil -> x)
-- 
-- === Value patterns
-- 
-- A match clause that contains a value pattern
-- 
-- > [mc| cons $x (cons $y (cons #(x + 1) (cons $z nil))) => (x, y, z) |]
-- 
-- is rewritten to
-- 
-- > MatchClause (cons (PatVar "x") (cons (PatVar "y") (cons (ValuePat (\HCons x (HCons (y HNil)) -> x + 1)) (cons (PatVar "z") nil))))
-- >             (\HCons x (HCons (y (HCons z HNil))) -> (x, y, z))
-- 
-- === And-patterns
-- 
-- A match clause that contains an and-pattern
-- 
-- > [mc| (cons _ _) & $x => x |]
-- 
-- is rewritten to
-- 
-- > MatchClause (AndPat (cons Wildcard Wildcard) (PatVar "x"))
-- >             (\HCons x HNil -> x)
-- 
-- === Or-patterns
-- 
-- A match clause that contains an or-pattern
-- 
-- > [mc| nil | (cons _ _) => "Matched" |]
-- 
-- is rewritten to
-- 
-- > MatchClause (OrPat nil (cons Wildcard Wildcard))
-- >             (\HNil -> "Matched")
--
-- === Collection patterns
--
-- A collection pattern
--
-- > [p1, p2, ..., pn]
--
-- is desugared into
--
-- > p1 : p2 : ... : pn : nil
--
-- === Cons patterns
--
-- A pattern with special collection pattern operator @:@
--
-- > p1 : p2
--
-- is parsed as
--
-- > p1 `cons` p2
--
-- === Join patterns
--
-- A pattern with special collection pattern operator @++@
--
-- > p1 ++ p2
--
-- is parsed as
--
-- > p1 `join` p2
mc :: QuasiQuoter
mc = QuasiQuoter { quoteExp  = compile
                 , quotePat  = undefined
                 , quoteType = undefined
                 , quoteDec  = undefined
                 }


listFixities :: [ParseFixity Name String]
listFixities =
  [ ParseFixity (Fixity AssocRight (Precedence 5) (mkName "join")) $ parser "++"
  , ParseFixity (Fixity AssocRight (Precedence 5) (mkName "cons")) $ parser ":"
  ]
 where
  parser symbol content | symbol == content = Right ()
                        | otherwise = Left $ show symbol ++ "is expected"

parseMode :: Q Exts.ParseMode
parseMode = do
  Loc { loc_filename } <- location
  extensions <- mapMaybe (fmap EnableExtension . convertExt) <$> extsEnabled
  pure defaultParseMode { Exts.parseFilename = loc_filename, Exts.extensions }
 where
  convertExt :: TH.Extension -> Maybe Exts.KnownExtension
  convertExt TH.TemplateHaskellQuotes = Just Exts.TemplateHaskell  -- haskell-suite/haskell-src-exts#357
  convertExt ext                      = readMaybe $ show ext

parseExp :: Exts.ParseMode -> String -> Q Exp
parseExp mode content = case parseExpWithMode mode content of
  ParseOk x       -> pure $ toExp x
  ParseFailed _ e -> fail e

compile :: String -> Q Exp
compile content = do
  mode        <- parseMode
  (pat, rest) <- parsePatternExpr mode content
  bodySource  <- takeBody rest
  body        <- parseExp mode bodySource
  pure $ compilePattern pat body
 where
  takeBody ('-' : '>' : xs) = pure xs
  takeBody xs               = fail $ "\"->\" is expected, but found " ++ show xs

parsePatternExpr
  :: Exts.ParseMode -> String -> Q (Pat.Expr Name Name Exp, String)
parsePatternExpr haskellMode content = case Pat.parseNonGreedy mode content of
  Left  e -> fail $ show e
  Right x -> pure x
  where mode = ParseMode { haskellMode, fixities = Just listFixities }

compilePattern :: Pat.Expr Name Name Exp -> Exp -> Exp
compilePattern pat body = AppE
  (AppE (ConE 'Control.Egison.Core.MatchClause) clauseExp)
  bodyExp
 where
  (clauseExp, bsAll) = runState (cataM go pat) []
  bsFun bs = LamE [toHListPat bs]
  bodyExp = bsFun bsAll body
  go Pat.WildcardF     = pure $ ConE 'Control.Egison.Core.Wildcard
  go (Pat.VariableF v) = do
    modify (<> [v])
    pure . AppE (ConE 'Control.Egison.Core.PatVar) . LitE . StringL $ pprint v
  go (Pat.ValueF e) = do
    bs <- get
    pure . AppE (VarE $ mkName "valuePat") $ bsFun bs e
  go (Pat.PredicateF e) = do
    bs <- get
    pure . AppE (ConE 'Control.Egison.Core.PredicatePat) $ bsFun bs e
  go (Pat.AndF e1 e2) =
    pure $ AppE (AppE (ConE 'Control.Egison.Core.AndPat) e1) e2
  go (Pat.OrF e1 e2) =
    pure $ AppE (AppE (ConE 'Control.Egison.Core.OrPat) e1) e2
  go (Pat.NotF e1) = pure $ AppE (ConE 'Control.Egison.Core.NotPat) e1
  go (Pat.TupleF [e1, e2]) = pure $ AppE (AppE (VarE $ mkName "pair") e1) e2
  go (Pat.TupleF _) = error "tuples other than pairs are not supported"
  go (Pat.CollectionF es) = pure $ toNilCons es
  go (Pat.InfixF n e1 e2) = pure . ParensE $ UInfixE e1 (VarE n) e2
  go (Pat.PatternF n es) = pure $ foldl' AppE (VarE n) es

toHListPat :: [Name] -> Pat
toHListPat = foldr go $ ConP 'HNil [] where go x a = ConP 'HCons [VarP x, a]

toNilCons :: [Exp] -> Exp
toNilCons = foldr go . VarE $ mkName "nil"
  where go e = AppE (AppE (VarE $ mkName "cons") e)

cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a)
  -> t
  -> m a
cataM alg = cata (alg <=< sequence)
