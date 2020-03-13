{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison
import Data.Numbers.Primes

main :: IO ()
main = do
  let n = 100
  let ans = take n (matchAll primes (List Integer)
                     [[mc| _ ++ $p : _ ++ #(p+6) : _ -> (p, p+6) |]])
  putStrLn $ show ans
