{-# OPTIONS_GHC -fno-full-laziness #-}

import           Control.Egison

import           Data.Numbers.Primes            ( primes )
import           Criterion.Main


primePairs2 :: Int -> [(Int, Int)]
primePairs2 n = take n
  $ matchAll primes (List Integer) [[mc| _ ++ $p : #(p+2) : _ -> (p, p+2) |]]

primePairs6 :: Int -> [(Int, Int)]
primePairs6 n = take n $ matchAll
  primes
  (List Integer)
  [[mc| _ ++ $p : _ ++ #(p+6) : _ -> (p, p+6) |]]

main :: IO ()
main = defaultMain
  [ bgroup
    "(p, p+2) pairs"
    [ bench "12800" $ nf primePairs2 12800
    , bench "25600" $ nf primePairs2 25600
    , bench "51200" $ nf primePairs2 51200
    ]
  , bgroup
    "(p, p+6) pairs"
    [ bench "128" $ nf primePairs6 128
    , bench "256" $ nf primePairs6 256
    , bench "512" $ nf primePairs6 512
    ]
  ]
