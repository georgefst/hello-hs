{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

#if __GLASGOW_HASKELL__ >= 910
main = putStrLn "9.10 (or higher)"
#elif __GLASGOW_HASKELL__ >= 908
-- Cabal 3.10 hits here
main = putStrLn "9.8 (good!)"
#elif __GLASGOW_HASKELL__ >= 906
main = putStrLn "9.6"
#elif __GLASGOW_HASKELL__ >= 904
main = putStrLn "9.4"
#elif __GLASGOW_HASKELL__ >= 902
-- Cabal 3.12 (on Billy) erroneously hits here, even with system GHC renamed
-- b055abb58fb414ffebec7d48f142d625907d62ff is the first bad commit
main = putStrLn "9.2"
#else
main = putStrLn "unknown"
#endif
