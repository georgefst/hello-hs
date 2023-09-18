module Main where

import Lib (C (c))
import Instance ()

main :: IO ()
main = putStrLn "Hello, Haskell!"
  where
    _ = c @Bool
