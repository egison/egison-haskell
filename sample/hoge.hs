{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs                     #-}
import           Control.Egison

main :: IO ()
main = do
  let ans = matchAll (1 :: Int) eql
              [ (wildcard, \(HCons Unit HNil) -> "aha") ]
  let ans2 = matchAll "ehe" eql
              [ (wildcard, \(HCons Unit HNil) -> "aha") ]
  let ans3 = matchAll (1 :: Int) eql
              [ (patVar "x", \(HCons x HNil) -> x) ]
  let ans4 = matchAll ([1,2] :: [Int]) (list eql)
              [ (patVar "x", \(HCons x HNil) -> x) ]
  let ans5 = matchAll (1 :: Int) eql
              [ (valuePat (\HNil -> 1), \(HCons Unit HNil) -> "aha") ]
  let ans6 = matchAll ([1,2,3] :: [Int]) (list eql)
              -- [ (consPat (patVar "x") (consPat (valuePat $ \(HCons x _) -> x+1) wildcard), \(HCons Unit (HCons x (HCons Unit (HCons Unit (HCons Unit HNil))))) -> "aha") ] :: [[Char]]
              [ (consPat (patVar "x") (valuePat $ \(HCons x _) -> [x+1, 3]), \(HCons Unit (HCons x (HCons Unit _))) -> x) ]
  putStrLn $ show ans
  putStrLn $ show ans2
  putStrLn $ show ans3
  putStrLn $ show ans4
  putStrLn $ show ans5
  putStrLn $ show ans6
