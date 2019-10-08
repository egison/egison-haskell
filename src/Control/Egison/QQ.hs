{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | Quasiquotation for rewriting a match clause.

module Control.Egison.QQ (
  mc,
  ) where

import           Control.Egison.Core
import           Data.List
import           Data.List.Split
import           Data.Map                   (Map)
import           Data.Maybe                 (fromMaybe)
import           Language.Haskell.Meta
import           Language.Haskell.TH        hiding (match)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Regex

-- | A quasiquoter for rewriting a match clause.
-- This quasiquoter is useful for generating a 'MatchClause' in user-friendly syntax.
-- 
-- === Wildcards
-- 
-- A match clause that contains a wildcard
-- 
-- > [mc| _ => "Matched" |]
-- 
-- is rewrited to
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
-- is rewrited to
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
-- is rewrited to
-- 
-- > MatchClause (cons (PatVar "x") (cons (PatVar "y") (cons (ValuePat (\HCons x (HCons (y HNil)) -> x + 1)) (cons (PatVar "z") nil))))
-- >             (\HCons x (HCons (y (HCons z HNil))) -> (x, y, z))
-- 
-- === And-patterns
-- 
-- A match clause that contains an and-pattern
-- 
-- > [mc| (& (cons _ _) $x) => x |]
-- 
-- is rewrited to
-- 
-- > MatchClause (AndPat (cons Wildcard Wildcard) (PatVar "x"))
-- >             (\HCons x HNil -> x)
-- 
-- === Or-patterns
-- 
-- A match clause that contains an or-pattern
-- 
-- > [mc| (| nil (cons _ _)) => "Matched" |]
-- 
-- is rewrited to
-- 
-- > MatchClause (OrPat nil (cons Wildcard Wildcard))
-- >             (\HNil -> "Matched")
mc :: QuasiQuoter
mc = QuasiQuoter { quoteExp = \s -> do
                      let [pat, exp] = splitOn "=>" s
                      e1 <- case parseExp (changeNotPat (changeOrPat (changeAndPat (changeValuePat (changePatVar (changeWildcard pat)))))) of
                              Left _ -> fail "Could not parse pattern expression."
                              Right exp -> return exp
                      e2 <- case parseExp exp of
                                 Left _ -> fail "Could not parse expression."
                                 Right exp -> return exp
                      mcChange e1 e2
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined }

changeWildcard :: String -> String
changeWildcard pat = subRegex (mkRegex " _") pat " Wildcard"

changePatVar :: String -> String
changePatVar pat = subRegex (mkRegex "\\$([a-zA-Z0-9]+)") pat "(PatVar \"\\1\")"

changeValuePat :: String -> String
changeValuePat pat = subRegex (mkRegex "\\#(\\([^)]+\\)|\\[[^)]+\\]|[a-zA-Z0-9]+)") pat "(valuePat \\1)"

changeAndPat :: String -> String
changeAndPat pat = subRegex (mkRegex "\\(\\&") pat "(AndPat"

changeOrPat :: String -> String
changeOrPat pat = subRegex (mkRegex "\\(\\|") pat "(OrPat"

changeNotPat :: String -> String
changeNotPat pat = subRegex (mkRegex "\\(not ") pat "(NotPat "

mcChange :: Exp -> Exp -> Q Exp
mcChange pat expr = do
  let (vars, xs) = extractPatVars [pat] []
  [| (MatchClause $(fst <$> changePat pat (map (`take` vars) xs)) $(changeExp vars expr)) |]

-- extract patvars from pattern
extractPatVars :: [Exp] -> [String] -> ([String], [Int])
extractPatVars [] vars = (vars, [])
extractPatVars (ParensE x:xs) vars = extractPatVars (x:xs) vars
extractPatVars (AppE (ConE name) p:xs) vars
  | nameBase name == "PatVar" = case p of (LitE (StringL s)) -> extractPatVars xs (vars ++ [s])
  | nameBase name == "PredicatePat" = let (vs, ns) = extractPatVars xs vars in (vs, length vars:ns)
  | nameBase name == "LaterPat" =
      let (vs1, ns1) = extractPatVars xs vars in
      let (vs2, ns2) = extractPatVars [p] vs1 in (vs2, ns2 ++ ns1)
  | otherwise = extractPatVars (p:xs) vars
extractPatVars (AppE (VarE name) p:xs) vars
  | nameBase name == "valuePat" = let (vs, ns) = extractPatVars xs vars in (vs, length vars:ns)
  | otherwise = extractPatVars (p:xs) vars
extractPatVars (AppE a b:xs) vars = extractPatVars (a:b:xs) vars
extractPatVars (SigE x typ:xs) vs = extractPatVars (x:xs) vs
extractPatVars (_:xs) vars = extractPatVars xs vars

-- change ValuePat e to \(HCons x HNil) -> e
-- change PredicatePat (\x -> e) to \(HCons x HNil) -> (\x -> e)
changePat :: Exp -> [[String]] -> Q (Exp, [[String]])
changePat e@(AppE (ConE name) p) vs
  | nameBase name == "PredicatePat" = do
      let (vars:varss) = vs
      (, varss) <$> appE (conE 'PredicatePat) (changeExp vars p)
  | otherwise = do
      (e', vs') <- changePat p vs
      (, vs') <$> appE (conE name) (return e')
changePat e@(AppE (VarE name) p) vs
  | nameBase name == "valuePat" = do
      let (vars:varss) = vs
      (, varss) <$> appE (varE name) (changeExp vars p)
  | otherwise = do
      (e', vs') <- changePat p vs
      (, vs') <$> appE (varE name) (return e')
changePat (AppE e1 e2) vs = do
  (e1', vs') <- changePat e1 vs
  (e2', vs'') <- changePat e2 vs'
  (, vs'') <$> appE (return e1') (return e2')
changePat (ParensE x) vs = changePat x vs
changePat (SigE x typ) vs = changePat x vs
changePat e vs = return (e, vs)

-- change e to \(HCons x HNil) -> e
changeExp :: [String] -> Exp -> Q Exp
changeExp vars expr = do
  vars' <- mapM newName vars
  vars'' <- mapM (\s -> newName $ s ++ "'") vars
  return $ LamE [f vars'] expr

-- \[x, y] -> HCons x (HCons y HNil)
f :: [Name] -> Pat
f []     = ConP 'HNil []
f (x:xs) = InfixP (VarP x) 'HCons $ f xs
