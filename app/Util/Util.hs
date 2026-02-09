module Util.Util where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.List.NonEmpty qualified as NE
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Tuple.Extra ((&&&))
import Optics (Lens', lens, (.~), (^.))

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

-- | Analogous to `until`. Note that `\p -> until (not . p)` goes on one step too long.
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f = go
  where
    go x = let y = f x in if p y then go y else x

neUncons :: NE.NonEmpty a -> (a, [a])
neUncons (x NE.:| xs) = (x, xs)

newtype Validation e r = Validation {unwrap :: Either e r} deriving newtype (Eq, Show, Functor)
instance (Semigroup m) => Applicative (Validation m) where
    pure = Validation . pure
    Validation (Left x) <*> Validation (Left y) = Validation $ Left $ x <> y
    Validation f <*> Validation r = Validation $ f <*> r

threadDelay' :: (MonadIO m) => NominalDiffTime -> m ()
threadDelay' = liftIO . threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds

fanout :: Lens' s a -> Lens' s b -> Lens' s (a, b)
fanout l1 l2 = lens ((^. l1) &&& (^. l2)) (flip $ uncurry (.) . bimap (l1 .~) (l2 .~))
