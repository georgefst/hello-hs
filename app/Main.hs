module Main where

import Lib (C (c))

main :: IO ()
main = putStrLn "Hello, Haskell!"
  where
    _ = c @Bool
