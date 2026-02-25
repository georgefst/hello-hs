{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Word
import Foreign.Store
import Georgefstris (Opts (..), opts)
import Georgefstris.Miso
import Miso
import System.Environment

{- FOURMOLU_DISABLE -}
main :: IO ()
main = do
    random <- opts.random
    -- TODO hardcoding ID is said in docs to be "hideously unsafe"
    -- but we need to use the same one after reload somehow
    -- use GHCIWatch hooks to run this on init instead?
    let foreignStoreId = 0 :: Word32
    model <- maybe (pure $ initialModel random 0) readStore
        -- TODO find better way of allowing the developer to signal that old state should be thrown away (per component)
        -- uncomment this line to reset the state, without restarting REPL
        -- =<< const (pure Nothing)
        -- TODO catch failures here, e.g. for when the model type has changed
        =<< lookupStore foreignStoreId
    -- TODO it'd be simpler if we could write to the store on unmount only, instead of every update
    -- and in theory more efficient, at least for huge models
    -- but the unmount action doesn't get run on reload
    -- and actually, given that it can't do IO directly, we'd need a new action as well...
    let a = startApp defaultEvents $ (app foreignStoreId model){styles = [Href "assets/style.css"]}
    getProgName >>= \case
        "<interactive>" -> reload a
        _ -> a
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
{- FOURMOLU_ENABLE -}
