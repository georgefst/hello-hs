module Georgefstris.Miso (app) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, first)
import Data.Bool
import Data.Foldable
import Data.Foldable1 qualified as NE
import Data.Function
import Data.Generics.Product
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid.Extra
import Data.Optics.Operators
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Georgefstris
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Miso hiding (for, new, (--->), (<---), (<--->))
import Miso.CSS qualified as MS
import Miso.Canvas qualified as Canvas
import Miso.Html
import Miso.Html.Property hiding (for_)
import Optics hiding (uncons)
import Optics.State.Operators
import Safe (predDef, predSafe, succDef)
import System.Random.Stateful hiding (next, random)
import Util
import Util.FixedLengthQueue qualified as FLQ
import Util.MisoOptics

data Model = Model
    { pile :: Grid -- the cells fixed in place
    , current :: ActivePiece
    , next :: FLQ.Queue Piece
    , ticks :: Word
    , paused :: Bool
    , level :: Level
    , random :: ([Piece], StdGen)
    , gameOver :: Bool
    , lineCount :: Word
    }
    deriving (Eq, Show, Generic)
initialModel :: StdGen -> Level -> Model
initialModel random0 level =
    Model
        { pile = emptyGrid
        , ticks = 0
        , level
        , gameOver = False
        , lineCount = 0
        , paused = False
        , ..
        }
  where
    ((current, next), random) = runRandomPieces ([], random0) do
        curry (bimap newPiece FLQ.fromList)
            <$> liftRandomiser opts.randomiser
            <*> replicateM (fromIntegral opts.previewLength) (liftRandomiser opts.randomiser)

data Action
    = NoOp (Maybe MisoString)
    | Init
    | Tick
    | KeyAction KeyAction

gridCanvas ::
    Int ->
    Int ->
    [Attribute action] ->
    ((Piece -> Bool -> V2 Int -> Canvas.Canvas ()) -> Canvas.Canvas ()) ->
    View parent action
gridCanvas w h attrs f = Canvas.canvas
    ([width_ $ ms w, height_ $ ms h, cssVar "canvas-width" w, cssVar "canvas-height" h] <> attrs)
    (const $ pure ())
    \() -> do
        -- TODO keep some canvas state rather than always redrawing everything?
        Canvas.clearRect (0, 0, fromIntegral w, fromIntegral h)
        f \p ghost (V2 x y) -> do
            Canvas.fillStyle $ Canvas.ColorArg if ghost then MS.lightgrey else opts.colours p
            Canvas.fillRect (fromIntegral x, fromIntegral y, 1, 1)

grid ::
    (HasType (FLQ.Queue Piece) parent, HasType Level parent, HasType Word parent) =>
    Model -> Component parent Model Action
grid m0 =
    ( component
        m0
        ( \case
            NoOp s -> io_ $ traverse_ consoleLog s
            Init -> subscribe keysPressedTopic KeyAction (const $ NoOp Nothing)
            Tick -> unlessM (use #paused) do
                level <- use #level
                relevantTick <-
                    #ticks %%= \t ->
                        let t' = t + 1
                            b = t' >= opts.rate level
                         in (b, if b then 0 else t')
                notOver <- not <$> use #gameOver
                when (relevantTick && notOver) do
                    success <- tryMove (+ V2 0 1)
                    when (not success) do
                        fixPiece
                        gameOver <- uncurry pieceIntersectsGrid <$> use (fanout #current #pile)
                        #gameOver .= gameOver
            KeyAction MoveLeft -> void $ tryMove (- V2 1 0)
            KeyAction MoveRight -> void $ tryMove (+ V2 1 0)
            KeyAction RotateLeft -> void $ tryRotate \case
                O -> id
                I; S; Z -> bool NoRotation Rotation90 . (== NoRotation)
                L; J; T -> succDef minBound
            KeyAction RotateRight -> void $ tryRotate \case
                O -> id
                I; S; Z -> bool NoRotation Rotation90 . (== NoRotation)
                L; J; T -> predDef maxBound
            KeyAction SoftDrop -> void $ tryMove (+ V2 0 1)
            KeyAction HardDrop -> whileM (tryMove (+ V2 0 1)) >> fixPiece
            KeyAction LevelDown -> #level %= predSafe
            KeyAction LevelUp -> #level %= min opts.topLevel . succDef opts.topLevel
            KeyAction Pause -> #paused %= not
            KeyAction Reset -> do
                m <- get
                put $ initialModel (snd m.random) m.level
        )
        ( \Model{..} ->
            gridCanvas opts.gridWidth opts.gridHeight (mwhen gameOver [class_ "game-over"]) \f ->
                deconstructGrid
                    ( addPieceToGrid False current
                        . applyWhen
                            opts.ghost
                            (addPieceToGrid True (while (pieceFits pile) (#pos %~ (+ V2 0 1)) current))
                        $ pile
                    )
                    \v -> \case
                        Unoccupied -> pure ()
                        Occupied p -> f p False v
                        Ghost p -> f p True v
        )
    )
        { subs =
            [ \sink -> forever do
                sink Tick
                threadDelay' opts.tickLength
            ]
        , mount = Just Init
        , bindings =
            [ typed <--- #next
            , typed <---> #level
            , typed <--- #lineCount
            ]
        }
  where
    fixPiece = do
        Model{current, next} <- get
        #pile %= addPieceToGrid False current
        next' <- #random %%= flip runRandomPieces (liftRandomiser opts.randomiser)
        fanout #current #next .= first newPiece (FLQ.shift next' next)
        removed <- #pile %%= removeCompletedLines
        #lineCount += removed
    tryMove f = tryEdit . (#pos %~ f) =<< use #current
    tryRotate f = tryEdit . (\p -> p & #rotation %~ f p.piece) =<< use #current
    tryEdit p = do
        b <- flip pieceFits p <$> use #pile
        when b $ #current .= p
        pure b
    pieceFits g p =
        either getAll (all (== Unoccupied))
            . (.unwrap)
            . traverse (Validation . first All . lookupGrid g . (+ p.pos) . rotate p.rotation)
            $ shape p.piece

sidebar ::
    (HasType (FLQ.Queue Piece) parent, HasType Level parent, HasType Word parent) =>
    (FLQ.Queue Piece, Level, Word) -> Component parent (FLQ.Queue Piece, Level, Word) ()
sidebar m0 =
    ( component
        m0
        (\() -> pure ())
        ( \(pieces, level, lineCount) ->
            div_
                []
                $ ( FLQ.toList pieces <&> \piece ->
                        div_
                            [class_ "next"]
                            [ let
                                ps = shape piece
                                vMin = V2 (NE.minimum $ (^. lensVL _x) <$> ps) (NE.minimum $ (^. lensVL _y) <$> ps)
                                vmax = V2 (NE.maximum $ (^. lensVL _x) <$> ps) (NE.maximum $ (^. lensVL _y) <$> ps)
                                V2 w h = vmax - vMin + 1
                               in
                                gridCanvas w h [] \f -> for_ ((- vMin) <$> ps) $ f piece False
                            ]
                  )
                    <> [ div_
                            [class_ "level"]
                            [ div_ [] [text "Level:"]
                            , div_ [] [text $ ms level]
                            ]
                       , div_
                            [class_ "line-count"]
                            [ div_ [] [text "Lines cleared:"]
                            , div_ [] [text $ ms lineCount]
                            ]
                       ]
        )
    )
        { bindings =
            [ typed ---> _1
            , typed ---> _2
            , typed ---> _3
            ]
        }

-- TODO even though this component effectively has no view, it seems to be the best way to encapsulate our key stuff
-- we should aim to improve the API of `keyboardSub` and maybe even `Sub` itself in order to make this unnecessary
dummyKeyHandler :: Topic KeyAction -> Component parent (Map KeyAction Integer, Integer) (Either (KeyAction, Bool, Integer) [Int])
dummyKeyHandler keyTopic =
    ( component
        (mempty, 0)
        ( either
            ( \(k, new, i) -> do
                stillPressed <- if new then pure True else gets $ (== Just i) . Map.lookup k . fst
                when stillPressed do
                    io_ $ publish keyTopic k
                    for_ (opts.keyDelays k) \(initialKeyDelay, repeatKeyDelay) -> io $ do
                        liftIO $ threadDelay' if new then initialKeyDelay else repeatKeyDelay
                        pure $ Left (k, False, i)
            )
            \(Set.fromList . mapMaybe opts.keymap -> ks') -> do
                ks <- use _1
                freshlyPressed <-
                    Map.fromList <$> flip mapMaybeM (Set.toList ks') \k ->
                        if Map.member k ks
                            then pure Nothing
                            else Just . (k,) <$> (_2 <<%= succ)
                -- TODO the union is disjoint, and the keys of the result will be precisely the elements of `ks'`...
                -- we could probably somehow take advantage of this to simplify the code
                _1 .= Map.union freshlyPressed (Map.filterWithKey (const . (`elem` ks')) ks)
                void $ flip Map.traverseWithKey freshlyPressed $ flip \i -> issue . Left . (,True,i)
        )
        (\_ -> div_ [] [])
    )
        { subs = [keyboardSub $ Right . IntSet.toList]
        }

app :: StdGen -> Component parent (FLQ.Queue Piece, Level, Word) ()
app random =
    component
        (m.next, m.level, m.lineCount)
        (\() -> pure ())
        ( \_ ->
            div_
                []
                [ div_ [id_ "grid"] ["grid" +> grid m]
                , div_ [id_ "sidebar"] ["sidebar" +> sidebar (m.next, m.level, m.lineCount)]
                , div_ [id_ "dummy-key-handler"] ["dummy-key-handler" +> dummyKeyHandler keysPressedTopic]
                ]
        )
  where
    m = initialModel random 0

keysPressedTopic :: Topic KeyAction
keysPressedTopic = topic "keys-pressed"
