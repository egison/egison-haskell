{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}
import           Control.Egison

main :: IO ()
main = do
  let ans = matchAll 1 eql
              [ (Wildcard, \HNil -> "aha") ]
  let ans2 = matchAll 1 eql
              [ (PatVar "x", \(HCons x HNil) -> x) ]
  let ans3 = matchAll [1,2,3] (list eql)
              [ (consPatL (PatVar "x") (consPatL (ValuePat $ \(HCons x _) -> x+1) Wildcard), \(HCons x _) -> x) ]
  let ans4 = matchAll [1,2,3] (list eql)
              [ (joinPatL (consPatL (PatVar "x") Wildcard) Wildcard, \(HCons x _) -> x) ]
  let ans5 = matchAll [1,2,3] (multiset eql)
              [ (consPatM (PatVar "x") Wildcard, \(HCons x _) -> x) ]

  putStrLn $ show ans
  putStrLn $ show ans2
  putStrLn $ show ans3
  putStrLn $ show ans4
  putStrLn $ show ans5
