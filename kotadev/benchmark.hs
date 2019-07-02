{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Egison


main :: IO ()
main = do
  let n = 1600
  let ans = matchAll [1..n] (multiset something)
              [ [mc| (ConsPat $x (ConsPat $y Wildcard)) => (x, y) |] ] :: [(Int, Int)]
  putStrLn $ show ans
