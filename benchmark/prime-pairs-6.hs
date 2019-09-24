{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison
import Data.Numbers.Primes

main :: IO ()
main = do
  let n = 100
  let ans = take n (matchAll primes (list integer)
                     [[mc| join _ (cons $p (join _ (cons #(p+6) _))) => (p, p+6) |]])
  putStrLn $ show ans
