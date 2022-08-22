{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Evdev
import Evdev.Codes
import qualified Evdev.Codes as Key
import Evdev.Uinput
import qualified Evdev.Uinput as U

main :: IO ()
main = do
    d <- U.newDevice "nix test" defaultDeviceOpts{keys = [KeyLeftmeta]}
    threadDelay 100_000
    writeBatch
        d
        [ KeyEvent Key.KeyLeftmeta Pressed
        , KeyEvent Key.KeyLeftmeta Released
        ]
