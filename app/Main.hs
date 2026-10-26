{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Tuple.Extra
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude hiding (font)
import Diagrams.TwoD.Offset
import DiagramsWaterfall
import LondonHaskell
import Waterfall qualified
import Waterfall.Colour
import WaterfallDice

main = do
    renderSVG "out.svg" (mkSizeSpec $ V2 (Just 1000) Nothing) londonHaskellDiag
    -- font <- Waterfall.fontFromSystem "P22 Johnston Underground" Waterfall.Regular 8
    font <- Waterfall.fontFromPath "../london-haskell-logo/P22 Johnston Underground Regular.ttf" 8 -- alternative if not installed system-wide
    Waterfall.writeOBJ 0.1 "out.obj" $
        renderDia
            Waterfall
            WaterfallOptions{font, prismLength = 1.2}
            londonHaskellDiag
