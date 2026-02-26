{-# LANGUAGE CPP #-}

module Main (main) where

import Foreign.Store
import Georgefstris (Opts (..), opts)
import Georgefstris.Miso
import Miso
import System.Environment

{- FOURMOLU_DISABLE -}
main :: IO ()
main = do
    -- TODO the logic for hot reloading and stylesheet cache busting should in principle be behind `#ifdef INTERACTIVE`
    -- but it doesn't do any real harm, and changing it would lose us HLS support
    -- anyway, it's intended to be temporary while waiting for more principled upstream support
    random <- opts.random
    -- TODO hardcoding ID is said in docs to be "hideously unsafe"
    -- but we need to use the same one after reload somehow
    -- use GHCIWatch hooks to run this on init instead?
    let foreignStoreId = 0
    model <- maybe (pure $ initialModel random 0) readStore
        -- TODO find better way of allowing the developer to signal that old state should be thrown away (per component)
        -- uncomment this line to reset the state, without restarting REPL
        -- =<< (\x -> pure Nothing)
        -- TODO catch failures here, e.g. for when the model type has changed
        =<< lookupStore foreignStoreId
    -- TODO it'd be simpler if we could write to the store on unmount only, instead of every update
    -- and in theory more efficient, at least for huge models
    -- but the unmount action doesn't get run on reload
    -- and actually, given that it can't do IO directly, we'd need a new action as well...
    let a = startApp defaultEvents $ (app foreignStoreId model){styles = [Href "assets/style.css" True]}
    getProgName >>= \case
        "<interactive>" -> reload a
        _ -> a
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#else
fetchCssHash :: IO Word
fetchCssHash = error "native stub"
#endif
{- FOURMOLU_ENABLE -}
