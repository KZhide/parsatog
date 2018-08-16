module Main where

import Lib
import Data.Char

main :: IO ()
main = do
  someFunc
  --content <- readFile "reserved.txt" 
  --putStrLn $ foldr1 ((++).(++",")) (fmap show (words (content ++ fmap toLower content)))
