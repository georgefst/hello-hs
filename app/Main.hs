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
    -- TODO this is a hack to ensure the stylesheet is reloaded on GHCI reload
    -- it's not about the HTTP cache - browsers don't even make a new request since the DOM element hasn't changed
    -- we use content hashing in an attempt to ensure that we don't unnecessarily refetch and thus cause page flash
    -- it even prevents the flash when the CSS returns to a state it was in previously, as that hash is cached
    -- this does seem a bit fragile though, particularly with printing seemingly being needed to prevent the flash
    -- anyway, it'd be better if the hashing were done server-side with ETags, but that makes browser mode more complex
    cacheBuster <- ms <$> fetchCssHash
    print cacheBuster
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
-- TODO we're hitting compiler errors using these, despite the first one being straight from the GHC users' guide
-- possibly due to using Haskell.nix with the Wasm backend, and our associated hacks
-- same behaviour with `JSString` swapped for `MisoString`
-- so we implement a version with a hardcoded input string instead, and do the hashing in JS so we can just get an `Int`
-- foreign import javascript safe "const r = await fetch($1); return r.text();" fetch' :: JSString -> IO JSString
foreign import javascript safe
  """
  const r = await fetch('assets/style.css');
  const buf = await r.arrayBuffer();
  const hash = await crypto.subtle.digest('SHA-256', buf);
  const view = new DataView(hash);
  return view.getUint32(0);
  """
  fetchCssHash :: IO Word
foreign export javascript "hs_start" main :: IO ()
#endif
{- FOURMOLU_ENABLE -}
