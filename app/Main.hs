module Main where

import           Control.Egison.Match (changePatVar, changeValuePat)
import           Data.Strings
import           System.Environment
import           Text.Regex

main :: IO ()
main = do
    args <- getArgs
    case args of
      original:input:output:_ -> runConvert original input output
      input:output:_          -> runConvert input input output
      input:_                 -> runConvert input input "-"
      []                      -> putStrLn "needed [FILE-TO-CONVERT]"

runConvert :: FilePath -> FilePath -> FilePath -> IO ()
runConvert original input output = do
    res <- ("{-# LANGUAGE TemplateHaskell #-}\n" ++) . f <$> readFile input
    if output == "-" then putStrLn res else writeFile output res

f :: String -> String
f input = case matchRegexAll (mkRegex "\\[mc\\| ([^|]+) \\|\\]") input of
            Nothing -> input
            Just (s1, mc, s2, [mcin]) -> s1 ++ "$(mcChange [e| " ++ g mcin ++ " |])" ++ f s2

g :: String -> String
g s = let (pat, expr) = strSplit "=>" s in
        "(" ++ changePatVar (changeValuePat pat) ++ "," ++ expr ++ ")"
