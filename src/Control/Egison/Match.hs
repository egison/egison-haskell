{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Control.Egison.Match (
  mc,
  mcChange,
  matchAll,
  match,
  ) where

import           Control.Egison.Core
import           Control.Egison.TH
import           Data.Strings
import           Language.Haskell.Meta
import           Language.Haskell.TH        hiding (match)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Regex

mc :: QuasiQuoter
mc = QuasiQuoter { quoteExp = \s -> do
                      let (pat, exp) = strSplit "=>" s
                      let Right e1 = parseExp $ changeValuePat $ changePatVar pat
                      let Right e2 = parseExp exp
                      mcChange $ return $ TupE [e1, e2]
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined }

changePatVar :: String -> String
changePatVar pat = subRegex (mkRegex "\\$(\\w+)") pat "(PatVar \"\\1\")"

changeValuePat :: String -> String
changeValuePat pat = subRegex (mkRegex "\\#(\\([^)]+\\)|\\w+)") pat "(ValuePat \\1)"

mcChange :: ExpQ -> ExpQ
mcChange e = do
  (TupE [pat, expr]) <- e
  let (vars, xs) = extractPatVars [pat] []
  [| ($(fst <$> changePat pat (map (`take` vars) xs)), $(makeExprQ vars expr)) |]

matchAll :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    map f (processMStatesAll [[MState [MAtom pat matcher tgt] []]]) ++ matches) []

match :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs
