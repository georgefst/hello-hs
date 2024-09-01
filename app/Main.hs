{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

#if __GLASGOW_HASKELL__ >= 908
-- Cabal 3.10 hits here
main = putStrLn "9.8 (or higher)"
#elif __GLASGOW_HASKELL__ >= 906
main = putStrLn "9.6"
#elif __GLASGOW_HASKELL__ >= 904
main = putStrLn "9.4"
#elif __GLASGOW_HASKELL__ >= 902
-- Cabal 3.12 (on Billy) erroneously hits here, even with system GHC renamed
main = putStrLn "9.2"
#else
main = putStrLn "unknown"
#endif
