{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Control.Egison.Match ( mc ) where

import           Language.Haskell.TH        hiding (match)
import           Language.Haskell.TH.Syntax
import           Control.Egison.Core
import           Control.Egison.TH

mc :: ExpQ -> ExpQ
mc e = do
  (ListE [TupE [pat, expr]]) <- e
  let (vars, xs) = extractPatVars [pat] []
  [| [($(fst <$> changePat pat (map (`take` vars) xs)), $(makeExprQ vars expr))] |]
