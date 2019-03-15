{-# OPTIONS_GHC -F -pgmF eh-preprocessor #-}
import Data.Numbers.Primes
import Control.Egison

main :: IO ()
main = do
  let twinPrimes = matchAll primes (list integer)
                     [ [mc| JoinPat Wildcard (ConsPat $p (ConsPat #(p + 2) Wildcard)) => (p, p + 2) :: (Int, Int) |] ]

  putStrLn $ show $ take 10 twinPrimes
-- [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]