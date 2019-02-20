module Control.Egison.QQ (mc2) where

import           Control.Egison.Match
import           Data.Strings
import           Language.Haskell.Meta
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote


mc2 :: QuasiQuoter
mc2 = QuasiQuoter { quoteExp = \s -> do
                      let (pat, exp) = strSplit "=>" s
                      let Right e1 = parseExp $ changePatString pat
                      let Right e2 = parseExp exp
                      mcChange $ return $ TupE [e1, e2]
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined }

changePatString :: String -> String
changePatString pat = pat
  -- replaceAll "\\$\\S+" "(PatVar \"${1}\")" pat
