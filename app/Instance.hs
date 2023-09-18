{-# OPTIONS_GHC -Wno-orphans #-}

module Instance where

import Lib (C (c))

instance C Bool where
    c = const ()
