{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import System.Environment
import Control.Egison

main :: IO ()
main = do
--  args <- getArgs
--  let n = read (head args)
  n <- getArgs >>= return . read . head
  let ans = matchAllDFS [1..n] (List Something)
              [[mc| _ ++ $x : _ ++ $y : _ -> (x, y) |]]
  putStrLn $ show $ length ans
