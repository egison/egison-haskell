{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Control.Egison.Match (
  mc,
  matchAll,
  match,
  ) where

import           Control.Egison.Core
import           Control.Egison.TH
import           Language.Haskell.TH        hiding (match)
import           Language.Haskell.TH.Syntax

mc :: ExpQ -> ExpQ
mc e = do
  (ListE [TupE [pat, expr]]) <- e
  let (vars, xs) = extractPatVars [pat] []
  [| [($(fst <$> changePat pat (map (`take` vars) xs)), $(makeExprQ vars expr))] |]

matchAll :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    map f (processMStatesAll [[MState [MAtom pat matcher tgt] []]]) ++ matches) []

match :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs
