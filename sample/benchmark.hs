{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs     #-}

import           Control.Egison

main :: IO ()
main = do
  let n = 10
  let ans = matchAll [1..n] (multiset something)
            $ [mc| (consPat $x (consPat $y Wildcard)) => (x, y) |] .*. PNil
  putStrLn $ show ans
