{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison

main :: IO ()
main = do
  let n = 400
  let ans = matchAll [1..n] (multiset something)
--  let ans = matchAllDFS [1..n] (multiset something)
              [[mc| (cons $x (cons $y Wildcard)) => (x, y) |]]
  putStrLn $ show ans
