{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison

main :: IO ()
main = do
  let n = 1600
--  let ans = matchAll [1..n] (Multiset Something)
  let ans = matchAllDFS [1..n] (Multiset Something)
              [[mc| (cons $x (cons $y Wildcard)) => (x, y) |]]
  putStrLn $ show ans
