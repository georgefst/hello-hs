{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import ColouredOBJ
import Data.Tuple.Extra
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude hiding (font)
import Diagrams.TwoD.Offset
import DiagramsWaterfall
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
