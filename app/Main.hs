{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import ColouredOBJ
import Data.Tuple.Extra
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude hiding (font)
import Diagrams.TwoD.Offset
import DiagramsGloss
import DiagramsWaterfall
import Graphics.Gloss qualified as Gloss
import LondonHaskell
import Waterfall qualified
import WaterfallDice

main = do
    -- runGloss
    renderSVG "out.svg" (mkSizeSpec $ V2 (Just 1000) Nothing) londonHaskellDiag
    -- font <- Waterfall.fontFromSystem "P22 Johnston Underground" Waterfall.Regular 8 --TODO repeated from diagram
    font <- Waterfall.fontFromPath "../london-haskell-logo/P22 Johnston Underground Regular.ttf" 8
    -- P22 Johnston Underground Regular.ttf
    let regions =
            renderDia
                Waterfall
                WaterfallOptions{font}
                londonHaskellDiag
        -- to3D = Waterfall.sweep $ Waterfall.arcVia3D 0 (V3 0 0 1) (V3 0 1 0.5)
        -- to3D = Waterfall.sweep $ Waterfall.fromPath2D $ Waterfall.line2D 0 (V2 4 10)
        to3D = Waterfall.prism 5
    -- Waterfall.writeSTL 0.01 "out.stl" dice
    -- the whole thing as one solid, which is all an STL can hold
    Waterfall.writeSTL 0.1 "out.stl" . to3D . mconcat $ fst <$> regions
    -- and each region as its own coloured solid
    writeColouredOBJ 0.1 "out.obj" $
        zipWith
            -- extrude upper layers so that overlapping regions don't fight over the same plane
            -- this ensures the view from above matches the SVG
            (\layer -> first $ Waterfall.translate (unit _z ^* (layer * 0.01)) . to3D)
            -- (const id)
            [0 ..]
            regions

runGloss =
    Gloss.animate (Gloss.InWindow "diagrams-gloss" (1000, 1000) (0, 0)) Gloss.white \(realToFrac -> t) ->
        renderDia
            Gloss
            GlossOptions
                { sizeSpec = mkSizeSpec $ V2 (Just 100) Nothing
                , samples = 10
                }
            . rotateBy (t / 2)
            $ londonHaskellDiag

-- https://github.com/diagrams/diagrams-lib/issues/268
-- mainTest = renderSVG "out.svg" (mkSizeSpec2D (Just 500) (Just 500)) $ t <> t' # showOrigin
--   where
--     x2 = r2 (1.0, 1.0) :: V2 Double -- endpoint
--     -- this works well
--     -- [c1, c2] = map r2 [(0.0, 1.0), (1.0, 0.0)]
--     -- this doesn't (and is seemingly what OP meant)
--     [c1,c2] = map r2 [(0.0, 0.0), (1.0, 0.0)]
--     t = fromSegments [bézier3 c1 c2 x2]
--     t' = (strokeLocTrail $ offsetTrail 0.5 t) # lc orange # lw thick
