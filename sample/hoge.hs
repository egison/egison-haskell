{-# LANGUAGE GADTs       #-}
import           Control.Egison

main :: IO ()
main = do
  let ans = matchAll 1 eql
              [ (wildcard, \HNil -> "aha") ]
  let ans2 = matchAll 1 eql
              [ (patVar "x", \(HCons x HNil) -> x) ]
  let ans3 = matchAll [1,2,3] (list eql)
              [ (consPat (patVar "x") (consPat (valuePat $ \(HCons x _) -> x+1) wildcard), \(HCons x _) -> x) ]

  putStrLn $ show ans
  putStrLn $ show ans2
  putStrLn $ show ans3
