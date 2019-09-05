{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs     #-}

import           Control.Egison

main :: IO ()
main = do
  let n = 10
  let ans = matchAll [1..n] (multiset something)
            $ ps [mc| (consPat $x (consPat $y Wildcard)) => (x, y) |]
  putStrLn $ show ans
