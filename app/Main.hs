{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedFile)

main :: IO ()
main = print $(embedFile "embed")
