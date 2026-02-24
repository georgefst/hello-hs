{-# LANGUAGE CPP #-}

module Main (main) where

import Georgefstris (Opts (..), opts)
import Georgefstris.Miso
import Miso
import System.Environment

{- FOURMOLU_DISABLE -}
main :: IO ()
main = do
    random <- opts.random
    let a = startApp defaultEvents $ (app random){styles = [Href "assets/style.css"]}
    getProgName >>= \case
        "<interactive>" -> reload a
        _ -> a
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
{- FOURMOLU_ENABLE -}
