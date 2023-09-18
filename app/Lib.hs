module Lib where

class C a where
    c :: a -> ()

instance C Bool where
    c = const ()
