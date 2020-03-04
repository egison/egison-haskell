{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Egison hiding (Integer)
import qualified Control.Egison as M

main = do
  putStrLn $ show $ matchAll [1,2,3,5] (Multiset Eql)
                      [[mc| [$x, $y, #(x + 1), $z] -> (x, y, z) |]]
-- [(1,3,5),(2,1,5),(1,5,3),(2,5,1)]
  putStrLn $ show $ matchAll [1,2,3,5] (Multiset Eql)
                      [[mc| [$x, $y, #(x + 1), #(not x)] -> (x, y) |]]
