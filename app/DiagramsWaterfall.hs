{-# LANGUAGE TypeFamilies #-}

-- TODO very WIP
-- heavily based on my Gloss backend here, as well as the SVG backend
-- what's quite different is how we render paths, since Waterfall has no explicit notion of lines with width
-- not widely tested - only really on London Haskell logo
-- see various TODOs below

-- | A Diagrams backend for Waterfall CAD. Turns shapes in to prisms.
module DiagramsWaterfall (
    B,
    Waterfall (Waterfall),
    Options (WaterfallOptions, font, prismLength),
) where

import Control.Monad
import Control.Monad.Zip
import Data.Colour qualified
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Tree
import Diagrams.Core.Compile
import Diagrams.Prelude
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Text
import Waterfall qualified as W

-- TODO parameterise this by a monad so that we can have an optional pure renderer, and only need IO for text?
-- i.e. have pretty much everything work for any monad, but then only `instance Renderable (Text Double) (Waterfall IO)`
-- for now we just have a pure renderer which requires a single font to be passed in up front and used everywhere
data Waterfall = Waterfall
type B = Waterfall

-- TODO proper 3D backend, instead of a 2D one which we just end up using to create prisms?
-- unclear how much support Diagrams really has
type instance V Waterfall = V2
type instance N Waterfall = Double

instance Backend Waterfall V2 Double where
    type Result Waterfall V2 Double = W.Solid
    newtype Render Waterfall V2 Double = Render
        { unwrap :: Options Waterfall V2 Double -> Style V2 Double -> Result Waterfall V2 Double
        }
    data Options Waterfall V2 Double = WaterfallOptions
        { font :: W.Font -- TODO temporary crutch - see above about fonts and pure rendering
        , prismLength :: Double
        }
    renderRTree Waterfall opts t =
        fold $ fmap (uncurry (flip prim)) $ mzip t $ collectParents $ fmap (fromMaybe mempty . style) t
      where
        -- TODO don't ignore annotation nodes?
        style = \case
            RStyle s -> Just s
            _ -> Nothing
        prim sty = \case
            RPrim p -> (render Waterfall p).unwrap opts sty
            _ -> mempty

instance Renderable (Path V2 Double) Waterfall where
    render Waterfall path = Render \opts sty -> mconcat do
        trail <- pathTrails path
        guard $ not $ isTrailEmpty $ unLoc trail -- prevents segfault
        concat
            [ maybeToList $ flip (toColouredPrism opts.prismLength) (renderLocTrail trail) <$> getFillColour sty
            , do
                w <- getLineWidth <$> maybeToList (getAttr sty)
                guard $ w > 0 -- main base case
                c <- maybeToList $ getLineColour sty
                -- TODO this probably isn't handling hollow shapes correctly - we'll need `W.difference2D`
                shape <- renderLocTrail <$> pathTrails (expandTrail (w / 2) $ undegenerate trail)
                pure $ toColouredPrism opts.prismLength c shape
            ]
      where
        renderLocTrail trail =
            W.makeShape
                . applyWhen (isLoop $ unLoc trail) W.closeLoop2D
                . foldMap
                    ( \case
                        FLinear (P a) (P b) -> W.line2D a b
                        FCubic (P a) (P b) (P c) (P d) -> W.bezier2D a b c d
                    )
                $ fixTrail trail

-- TODO stop ignoring text alignment, rotations, non-colour styles etc.
instance Renderable (Text Double) Waterfall where
    render Waterfall (Text tt _ s) = Render \opts sty -> mconcat do
        c <- maybeToList $ getFillColour sty
        pure
            . toColouredPrism opts.prismLength c
            . W.translate2D (unP $ papply tt 0)
            . W.scale2D (apply tt 1)
            $ W.text opts.font s

getFillColour :: Style v Double -> Maybe (AlphaColour Double)
getFillColour = textureToColour . getFillTexture <=< getAttr
getLineColour :: Style v Double -> Maybe (AlphaColour Double)
getLineColour = textureToColour . getLineTexture <=< getAttr
textureToColour :: Texture Double -> Maybe (AlphaColour Double)
textureToColour = \case
    SC c -> Just $ someToAlpha c
    -- TODO handle gradients
    LG _; RG _ -> Nothing

toColouredPrism :: Double -> AlphaColour Double -> W.Shape -> W.Solid
toColouredPrism d c =
    W.paintSolid
        ( W.paintWithColour (W.Colour r g b)
            <> W.paintWithOpacity (alphaChannel c)
        )
        . W.prism d
  where
    RGB r g b = toSRGB $ c `Data.Colour.over` black

-- TODO is this equivalent to some standard recursion scheme?
-- we can't use anything like `foldTree` as we need to pass things _down_
-- and `traverse` etc. don't rewind when re-ascending
-- the simpler version would be inefficient (prefixes keep getting re-evaluated):
-- collectParents (Node n ts) = Node n $ map (((n <>) <$>) . collectParents) ts
collectParents :: forall a. (Monoid a) => Tree a -> Tree a
collectParents = go mempty
  where
    go r (Node n ts) = let r' = r <> n in Node r' $ map (go r') ts

-- TODO this is a hacky vibed-up workaround for https://github.com/diagrams/diagrams-lib/issues/268#issuecomment-5209421453

{- | Nudge zero-length Bezier control handles off zero so that
`Diagrams.TwoD.Offset` can derive a tangent direction from them.

`offsetSegment` gets its offset direction from @unitPerp = signorm . perp@,
which for a zero handle is @V2 NaN NaN@; its termination test is a comparison,
and comparisons against NaN are always False, so the subdivision never
terminates. Subdividing cannot rescue it either: halving a cubic whose first
handle is zero leaves the first half's handle zero too.

We substitute a very short handle pointing along the limiting tangent, which
is given by the next derivative. For control points 0, a, b, c:
  B''(0) = 6b - 12a       -- with a == 0, the start direction is b
  B(t)   = t^3 c          -- with a == b == 0, the segment is straight, along c
  B''(1) = 6a - 12b + 6c  -- with b == c, the end direction is a - c

Note @unitPerp@ normalises, so @eps@ has *no* effect on the direction the
offset is computed in -- it is exactly the direction the upstream fix picks.
It only controls how much the curve itself is perturbed, so keep it small; it
need only be large enough that @quadrance@ does not underflow.
-}
undegenerate :: Located (Trail V2 Double) -> Located (Trail V2 Double)
undegenerate =
    mapLoc $
        withTrail
            (wrapLine . onLineSegments (map fixSeg))
            -- glueLine . cutLoop === id, and glueLine re-closes the loop, so
            -- this is safe even though fixSeg perturbs the geometry
            (wrapLoop . glueLine . onLineSegments (map fixSeg) . cutLoop)
  where
    fixSeg = \case
        s@(Cubic c1 c2 (OffsetClosed c3))
            | degenerate c1 && degenerate c2 -> Linear (OffsetClosed c3)
            | degenerate c1 -> Cubic (c2 ^* eps) c2 (OffsetClosed c3)
            | degenerate (c3 ^-^ c2) -> Cubic c1 (c2 ^+^ (c1 ^-^ c3) ^* eps) (OffsetClosed c3)
            | otherwise -> s
        s -> s
    eps = 1e-9
    -- Same reasoning as the upstream patch: ask whether `unitPerp` can actually
    -- produce a direction, rather than picking an arbitrary length threshold.
    -- (`unitPerp` itself isn't exported, hence `signorm . perp` inline.)
    degenerate v = not (all finite (signorm (perp v)))
      where
        finite x = not (isNaN x || isInfinite x)
