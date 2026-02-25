{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Word
import Foreign.Store
import Georgefstris (Opts (..), opts)
import Georgefstris.Miso
import Miso
import System.Environment
import System.Random.Stateful (globalStdGen, uniformM)

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
    -- TODO this is a hack to ensure the stylesheet is reloaded on GHCI reload
    -- it's not about the HTTP cache - browsers don't even make a new request since the DOM element hasn't changed
    -- and if this is our long-term solution, it should really be a counter or timestamp to avoid potential collisions
    cacheBuster <- ms <$> uniformM @Word globalStdGen
    -- TODO we should find some way to _not_ request a new stylesheet when it hasn't changed
    -- one way would be if GHCIWatch could run different commands depending on what file triggered the reload
    -- for now we can uncomment the line below, though that will only work from the second reload onwards
    -- let cacheBuster = ""
    -- TODO it'd be simpler if we could write to the store on unmount only, instead of every update
    -- and in theory more efficient, at least for huge models
    -- but the unmount action doesn't get run on reload
    -- and actually, given that it can't do IO directly, we'd need a new action as well...
    let a = startApp defaultEvents $ (app foreignStoreId model){styles = [Href $ "assets/style.css#" <> cacheBuster]}
    getProgName >>= \case
        "<interactive>" -> reload a
        _ -> a
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
{- FOURMOLU_ENABLE -}
