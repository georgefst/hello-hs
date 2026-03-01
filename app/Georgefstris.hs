module Georgefstris where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.Bool
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Either.Extra
import Data.Foldable
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Massiv.Array (Array)
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Monoid.Extra
import Data.Ord (clamp)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic, Generically (Generically))
import Linear (V2 (V2))
import Miso.CSS (Color)
import Miso.CSS qualified as MS
import Miso.JSON (FromJSON, ToJSON)
import Miso.String (ToMisoString)
import System.Random.Stateful hiding (next, random)
import Util
import Util.MisoAesonDeriving
import Util.Shuffle

data Opts = Opts
    { gridWidth :: Int
    , gridHeight :: Int
    , previewLength :: Word
    , ghost :: Bool
    , random :: IO StdGen
    , randomiser :: State StdGen (NonEmpty Piece)
    , topLevel :: Level
    , keyDelays :: KeyAction -> Maybe (NominalDiffTime, NominalDiffTime)
    , tickLength :: NominalDiffTime
    , rate :: Level -> Word
    , colours :: Piece -> Color
    , keymap :: Int -> Maybe KeyAction
    }

opts :: Opts
opts =
    Opts
        { gridWidth = 10
        , gridHeight = 18
        , previewLength = 1
        , ghost = False
        , random = newStdGen
        , randomiser = flip shuffleM StateGenM . (:| enumerate) =<< uniformM StateGenM
        , topLevel
        , keyDelays = \case
            RotateLeft; RotateRight; HardDrop; Pause; Reset -> Nothing
            MoveLeft; MoveRight; SoftDrop; LevelDown; LevelUp -> Just (0.12, 0.02)
        , tickLength = 0.05
        , rate = \l -> fromIntegral $ topLevel + 1 - clamp (startLevel, topLevel) l
        , colours = \case
            O -> hsl' 0 62 54
            I -> hsl' 29 73 58
            S -> hsl' 203 72 75
            Z -> hsl' 147 34 49
            L -> hsl' 217 72 56
            J -> hsl' 275 37 60
            T -> hsl' 46 98 63
        , keymap = \case
            37 -> Just MoveLeft -- left arrow
            39 -> Just MoveRight -- right arrow
            90 -> Just RotateLeft -- z
            88 -> Just RotateRight -- x
            40 -> Just SoftDrop -- down arrow
            32 -> Just HardDrop -- space bar
            173 -> Just LevelDown -- minus
            61 -> Just LevelUp -- plus
            27 -> Just Pause -- esc
            82 -> Just Reset -- r
            _ -> Nothing
        }
  where
    startLevel = Level 1
    topLevel = Level 10

-- TODO work out why Miso's `hsl` always just produces black on canvas
hsl' :: Double -> Double -> Double -> Color
hsl' h s l = uncurryRGB MS.rgb $ fmap (floor @Double . (* 255)) $ hsl h (s / 100) (l / 100)

newtype Level = Level Word
    deriving newtype (Eq, Ord, Show, Enum, Bounded, Num, Real, Integral, ToMisoString)

data KeyAction
    = MoveLeft
    | MoveRight
    | RotateLeft
    | RotateRight
    | SoftDrop
    | HardDrop
    | LevelDown
    | LevelUp
    | Pause
    | Reset
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via Generically KeyAction
    deriving (FromJSON, ToJSON) via (MisoAeson KeyAction)

data Piece = O | I | S | Z | L | J | T
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
instance Uniform Piece where uniformM = uniformEnumM

-- inv: list has length 4, and its xs range from -1 to 2 (a single 2 for `I`), and ys from 0 to 1
shape :: Piece -> NonEmpty (V2 Int)
shape =
    (0 :|) . \case
        O -> [V2 0 1, V2 1 0, V2 1 1]
        I -> [V2 -1 0, V2 1 0, V2 2 0]
        S -> [V2 -1 1, V2 0 1, V2 1 0]
        Z -> [V2 -1 0, V2 0 1, V2 1 1]
        L -> [V2 -1 0, V2 -1 1, V2 1 0]
        J -> [V2 -1 0, V2 1 0, V2 1 1]
        T -> [V2 -1 0, V2 0 1, V2 1 0]

data ActivePiece = ActivePiece
    { piece :: Piece
    , pos :: V2 Int
    , rotation :: Rotation
    }
    deriving stock (Eq, Show, Generic)

newPiece :: Piece -> ActivePiece
newPiece piece = ActivePiece{piece, pos = V2 (opts.gridWidth `div` 2 - 1) 0, rotation = NoRotation}

-- TODO separate module? nothing like this in `linear` or even `diagrams`
-- actually, we could maybe just use `linear`'s matrices...
data Rotation
    = NoRotation
    | Rotation90
    | Rotation180
    | Rotation270
    deriving stock (Eq, Ord, Show, Enum, Bounded)
rotate :: Rotation -> V2 Int -> V2 Int
rotate = flip \(V2 x y) -> \case
    NoRotation -> V2 x y
    Rotation90 -> V2 y -x
    Rotation180 -> V2 -x -y
    Rotation270 -> V2 -y x

data Cell
    = Occupied Piece
    | Ghost Piece
    | Unoccupied
    deriving stock (Eq, Ord, Show)

-- inv: has width and height given by game opts, i.e.
-- (0,0) is the top-left
-- TODO use length-indexed vectors to statically avoid out-of-bounds errors?
-- TODO swap out `A.B` for more efficient representation? or at least use sparse array for empty cells
newtype Grid = Grid (Array A.B A.Ix2 Cell)
    deriving newtype (Eq, Show)
emptyGrid :: Grid
emptyGrid = Grid $ A.replicate A.Seq (A.Sz2 opts.gridWidth opts.gridHeight) Unoccupied
lookupGrid :: Grid -> V2 Int -> Either Bool Cell -- `Left True` means the location is above the top
lookupGrid (Grid g) (V2 x y) = if y < 0 then Left True else maybeToEither False $ g A.!? A.Ix2 x y
deconstructGrid :: (Monad m) => Grid -> (V2 Int -> Cell -> m b) -> m ()
deconstructGrid (Grid g) = A.iforM_ g . \f (A.Ix2 x y) -> f (V2 x y)
addPieceToGrid :: Bool -> ActivePiece -> Grid -> Grid
addPieceToGrid ghost ActivePiece{..} (Grid g) =
    -- TODO can we avoid a copy?
    Grid $ A.withMArrayST_ g \gm -> for_ ((+ pos) . rotate rotation <$> shape piece) \(V2 x y) ->
        A.modify_
            gm
            -- this assumes that the extra piece does not intersect occupied cells - if it does we overwrite
            (const $ pure $ bool Occupied Ghost ghost piece)
            (A.Ix2 x y)
pieceIntersectsGrid :: ActivePiece -> Grid -> Bool
pieceIntersectsGrid ActivePiece{..} (Grid g) =
    getAny $
        A.ifoldMono
            (\(A.Ix2 x y) e -> Any $ any ((\v' -> e /= Unoccupied && V2 x y == v') . (+ pos) . rotate rotation) (shape piece))
            g
removeCompletedLines :: Grid -> (Word, Grid)
removeCompletedLines (Grid g) = second Grid $ fromMaybe (0, g) do
    -- TODO any failure would be a programmer error, so we just use `fromMaybe` to return the original
    -- that does imply there's probably a better way of doing this...
    g' <- foldrM (\i a -> A.compute <$> A.deleteColumnsM i 1 a) g completedRows
    g'' <- A.appendM 1 (A.replicate @A.B A.Seq (A.Sz2 opts.gridWidth $ length completedRows) Unoccupied) g'
    pure (genericLength completedRows, A.compute g'')
  where
    -- TODO it's a bit inefficient to `compute` on every iteration - there should be a better way
    -- TODO shouldn't it be `A.ifoldOuterSlice`, `A.deleteRowsM` etc. rather than columns?
    completedRows = A.ifoldInnerSlice (\i row -> mwhen (A.all (/= Unoccupied) row) [i]) g

-- a monad for operations which produce finite lists of pieces
type RandomPieces = StateT [Piece] (State StdGen)
runRandomPieces :: ([Piece], StdGen) -> RandomPieces a -> (a, ([Piece], StdGen))
runRandomPieces (l, g) f = (\((a, b), c) -> (a, (b, c))) $ runStateGen g $ flip runStateT l . \StateGenM -> f
liftRandomiser :: State StdGen (NonEmpty Piece) -> RandomPieces Piece
liftRandomiser r = do
    (x, xs) <-
        gets uncons
            >>= maybe
                -- generate a new chunk of random pieces
                (neUncons <$> lift r)
                -- we already have pieces computed - take from those
                pure
    put xs
    pure x
