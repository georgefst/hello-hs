{-# LANGUAGE TypeFamilies #-}

-- TODO very WIP
-- heavily based on my Gloss backend here, as well as the SVG backend
-- what's quite different is how we render paths, since Waterfall has no explicit notion of lines with width
-- not widely tested - only really on London Haskell logo
-- see various TODOs below
-- esp. last section, pointing out slop

module DiagramsWaterfall (
    B,
    Waterfall (Waterfall),
    Options (WaterfallOptions, font),
) where

import Control.Monad
import Control.Monad.Zip
import Data.Function
import Data.Maybe
import Data.Tree
import Diagrams.Core.Compile
import Diagrams.Prelude
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Text
import Undegenerate (undegenerate)
import Waterfall qualified as W

-- TODO parameterise this by a monad so that we can have an optional pure renderer, and only need IO for text?
-- i.e. have pretty much everything work for any monad, but then only `instance Renderable (Text Double) (Waterfall IO)`
-- for now we just have a pure renderer which requires a single font to be passed in up front and used everywhere
data Waterfall = Waterfall
type B = Waterfall

-- TODO proper 3D backend, instead of a 2D one which we just end up using to create prisms
type instance V Waterfall = V2
type instance N Waterfall = Double

instance Backend Waterfall V2 Double where
    -- coloured shapes - later entries are the ones drawn on top
    -- TODO having colour here is for the benefit of the OBJ backend
    -- it'd be better if shapes did carry colour information, and Waterfall CAD could use this
    -- see https://github.com/joe-warren/opencascade-hs/issues/44
    -- we might also then be able to return a single shape rather than a list
    -- we should also eventually stop ignoring non-colour styles
    type Result Waterfall V2 Double = [(W.Shape, AlphaColour Double)]
    newtype Render Waterfall V2 Double = Render
        { unwrap :: Options Waterfall V2 Double -> Style V2 Double -> Result Waterfall V2 Double
        }
    data Options Waterfall V2 Double = WaterfallOptions
        { font :: W.Font --  TODO temporary crutch - see above about fonts and pure rendering
        }
    renderRTree Waterfall opts t =
        concat $ fmap (uncurry (flip prim)) $ mzip t $ collectParents $ fmap (fromMaybe mempty . style) t
      where
        -- TODO don't ignore annotation nodes?
        style = \case
            RStyle s -> Just s
            _ -> Nothing
        prim sty = \case
            RPrim p -> (render Waterfall p).unwrap opts sty
            _ -> []

instance Renderable (Path V2 Double) Waterfall where
    render Waterfall = \path -> Render \_ sty -> do
        trail <- pathTrails path
        guard $ not $ isTrailEmpty $ unLoc trail -- prevents segfault
        concat
            [ maybeToList $ (renderLocTrail trail,) <$> getFillColour sty
            , do
                w <- getLineWidth <$> maybeToList (getAttr sty)
                guard $ w > 0 -- main base case
                c <- maybeToList $ getLineColour sty
                -- TODO this probably isn't handling hollow shapes correctly - we'll need `W.difference2D`
                shape <- renderLocTrail <$> pathTrails (expandTrail (w / 2) $ undegenerate trail)
                pure (shape, c)
            ]
      where
        renderLocTrail trail = do
            W.makeShape
                . applyWhen (isLoop $ unLoc trail) W.closeLoop2D
                . foldMap
                    ( \case
                        FLinear a b -> W.line2D (unP a) (unP b)
                        FCubic a b c d -> W.bezier2D (unP a) (unP b) (unP c) (unP d)
                    )
                $ fixTrail trail

-- TODO stop ignoring text alignment, rotations, non-colour styles etc.
instance Renderable (Text Double) Waterfall where
    render Waterfall (Text tt _ s) = Render \opts sty -> do
        c <- maybeToList $ getFillColour sty
        pure
            ( W.translate2D (unP $ papply tt 0)
                . W.scale2D (apply tt 1)
                $ W.text opts.font s
            , c
            )

getFillColour :: Style v Double -> Maybe (AlphaColour Double)
getFillColour = textureToColour . getFillTexture <=< getAttr
getLineColour :: Style v Double -> Maybe (AlphaColour Double)
getLineColour = textureToColour . getLineTexture <=< getAttr
textureToColour :: Texture Double -> Maybe (AlphaColour Double)
textureToColour = \case
    SC c -> Just $ someToAlpha c
    -- TODO handle gradients
    LG _; RG _ -> Nothing

-- TODO is this equivalent to some standard recursion scheme?
-- we can't use anything like `foldTree` as we need to pass things _down_
-- and `traverse` etc. don't rewind when re-ascending
-- the simpler version would be inefficient (prefixes keep getting re-evaluated):
-- collectParents (Node n ts) = Node n $ map (((n <>) <$>) . collectParents) ts
collectParents :: forall a. (Monoid a) => Tree a -> Tree a
collectParents = go mempty
  where
    go r (Node n ts) = let r' = r <> n in Node r' $ map (go r') ts
