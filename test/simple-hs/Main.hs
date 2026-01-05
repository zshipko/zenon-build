module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Testing simple Haskell build"
    putStrLn $ "factorial 5 = " ++ show (factorial 5)
    putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10)
