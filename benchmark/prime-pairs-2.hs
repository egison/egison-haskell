{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import System.Environment
import Control.Egison
import Data.Numbers.Primes

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args)
  let ans = take n (matchAll primes (List Integer)
                     [[mc| join _ (cons $p (cons #(p+2) _)) => (p, p+2) |]])
  putStrLn $ show ans
