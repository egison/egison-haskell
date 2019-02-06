{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module MatchTH2 ( f ) where

import           Language.Haskell.TH        hiding (match)
import           Language.Haskell.TH.Syntax
import           Match
import           MatchTH

f :: ExpQ -> ExpQ
f e = do
  (ListE [TupE [pat, expr]]) <- e
  [| [($(return pat), $(makeExprQ (extractPatVars pat) expr))] |]
