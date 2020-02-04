import System.Environment
import Data.List

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

comb2 :: [a] -> [(a,a)]
comb2 xs = [ (y,z) | y:ts <- tails xs
                   , z <- ts ]

main = do
  args <- getArgs
  let n = read (head args)
--  let ans = combinations 2 [1..n]
  let ans2 = comb2 [1..n]
--  putStrLn $ show $ length ans
  putStrLn $ show $ length ans2
--  putStrLn $ show $ ans2
--  putStrLn $ show $ tails [1..n]
--  putStrLn $ show $ (tails []  :: [[Int]])
