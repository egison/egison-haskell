import System.Environment
import Data.List

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

main = do
  args <- getArgs
  let n = read (head args)
  let ans = combinations 2 [1..n]
  putStrLn $ show $ length ans
