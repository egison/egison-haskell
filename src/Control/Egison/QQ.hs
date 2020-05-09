-- | Quasiquotation for rewriting a match clause.

module Control.Egison.QQ
  ( mc
  )
where

import           Language.Haskell.TH            ( Q
                                                , Exp
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )

import           Control.Egison.QQ.Internal     ( parseMode
                                                , parseExp
                                                , parsePatternExpr
                                                , compilePattern
                                                )

-- | A quasiquoter for rewriting a match clause.
-- This quasiquoter is useful for generating a 'MatchClause' in user-friendly syntax.
-- 
-- === Wildcards
-- 
-- A match clause that contains a wildcard
-- 
-- > [mc| _ -> "Matched" |]
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
-- > [mc| $x -> x |]
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
-- > [mc| cons $x (cons $y (cons #(x + 1) (cons $z nil))) -> (x, y, z) |]
-- 
-- is rewritten to
-- 
-- > MatchClause (cons (PatVar "x") (cons (PatVar "y") (cons (valuePat (\HCons x (HCons (y HNil)) -> x + 1)) (cons (PatVar "z") nil))))
-- >             (\HCons x (HCons (y (HCons z HNil))) -> (x, y, z))
-- 
-- === And-patterns
-- 
-- A match clause that contains an and-pattern
-- 
-- > [mc| (cons _ _) & $x -> x |]
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
-- > [mc| nil | (cons _ _) -> "Matched" |]
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

compile :: String -> Q Exp
compile content = do
  mode        <- parseMode
  (pat, rest) <- parsePatternExpr mode content
  bodySource  <- takeBody rest
  body        <- parseExp mode bodySource
  compilePattern pat $ const body
 where
  takeBody ('-' : '>' : xs) = pure xs
  takeBody xs               = fail $ "\"->\" is expected, but found " ++ show xs
