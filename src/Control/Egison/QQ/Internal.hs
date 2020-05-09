module Control.Egison.QQ.Internal
  ( parseMode
  , parseExp
  , parsePatternExpr
  , compilePattern
  )
where

import           Control.Egison.Core
import           Control.Monad                  ( (<=<) )
import           Control.Monad.State            ( runStateT
                                                , MonadState(..)
                                                , modify
                                                , lift
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
import qualified Language.Egison.Syntax.Pattern
                                               as Pat
                                                ( Expr
                                                , ExprF(..)
                                                )

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

parsePatternExpr
  :: Exts.ParseMode -> String -> Q (Pat.Expr Name Name Exp, String)
parsePatternExpr haskellMode content = case Pat.parseNonGreedy mode content of
  Left  e -> fail $ show e
  Right x -> pure x
  where mode = ParseMode { haskellMode, fixities = Just listFixities }

compilePattern :: Pat.Expr Name Name Exp -> ([Name] -> Exp) -> Q Exp
compilePattern pat makeBody = do
  (clauseExp, bindings) <- runStateT (cataM go pat) []
  let bodyExp = bsFun bindings $ makeBody bindings
  pure $ AppE (AppE (ConE 'Control.Egison.Core.MatchClause) clauseExp) bodyExp
 where
  bsFun bs = LamE [toHListPat bs]
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
  go (Pat.TupleF _) = lift $ fail "tuples other than pairs are not supported"
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
