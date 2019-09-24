{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison
import Data.Numbers.Primes

main :: IO ()
main = do
  let n = 100
  let ans = take n (matchAll primes (list integer)
                     [[mc| join _ (cons $p (cons #(p+2) _)) => (p, p+2) |]])
  putStrLn $ show ans
