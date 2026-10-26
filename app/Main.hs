{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Data.Monoid.Extra (getSum, mwhen)
import Linear (V3 (..), unit, _x)
import Waterfall qualified as W

-- rough measurements (mm)
roomWidth = 2720
roomHeight = 3070
alcoveWidth = 600
mattressWidth = 1370
mattressLength = 1900
mattressHeight = 200
headBoardWidth = 70 -- also taken as the width of the padding at the end and sides of bed
headBoardHeight = 700 -- how far the headboard rises above the main frame
alcoveDepth = 250
bedLegHeight = 120
bedLegRadius = 35
bedLegInset = 100
bedMainFrameHeight = 250

-- roomDepth = 2660 -- not counting alcove

-- parameters we have some control over
headroom = 1000
alcoveWardrobeDepth = 600 -- this is a bit of a standard, and only the alcove part is this deep
rightWardrobeWidth = alcoveWidth -- this is maybe implicitly assumed elsewhere
cupboardBehind = True
shelfGap = 100
shelfCurveRadius = 400
cupboardDepth = 100
platformThickness = 30
platformDepth = wardrobeDepth

-- derived
wardrobeHeight = roomHeight
wardrobeCenterHeight = roomHeight - bedHeight + mattressHeight - headBoardHeight - headroom
leftWardrobeWidth = roomWidth - bedWidth - rightWardrobeWidth
bedHeight = mattressHeight + bedMainFrameHeight + bedLegHeight
bedWidth = mattressWidth + headBoardWidth * 2
bedLength = mattressLength + headBoardWidth * 2
wardrobeDepth = alcoveWardrobeDepth - alcoveDepth

bed =
    frame
        <> mattress
  where
    frame =
        (base <> headboard <> legs)
    base =
        W.translate (V3 0 bedLegHeight 0) $
            W.box (V3 bedWidth bedMainFrameHeight bedLength)
    mattress =
        W.translate (V3 headBoardWidth (bedMainFrameHeight + bedLegHeight) headBoardWidth) $
            W.box (V3 mattressWidth mattressHeight mattressLength)
    legs =
        foldMap
            (flip W.translate $ verticalCylinder bedLegRadius bedLegHeight)
            [ V3 x 0 z
            | x <- [bedLegInset, bedWidth - bedLegInset]
            , z <- [bedLegInset, bedLength - bedLegInset]
            ]
    headboard =
        W.translate
            (V3 0 (bedLegHeight + bedMainFrameHeight) 0)
            $ W.box
            $ V3 bedWidth headBoardHeight headBoardWidth

wardrobe =
    W.translate
        (V3 leftWardrobeWidth (roomHeight - wardrobeCenterHeight) 0)
        top
        <> left
        <> W.translate
            (V3 (roomWidth - rightWardrobeWidth) 0 -alcoveDepth)
            right
        <> cupboard
  where
    top = W.box $ V3 bedWidth wardrobeCenterHeight wardrobeDepth
    left = W.box $ V3 leftWardrobeWidth wardrobeHeight wardrobeDepth
    right = W.box $ V3 rightWardrobeWidth wardrobeHeight alcoveWardrobeDepth
    cupboard =
        mwhen cupboardBehind $
            W.translate
                (V3 leftWardrobeWidth 0 0)
                ( body
                    <> platform
                )
      where
        body = W.box (V3 bedWidth bodyHeight cupboardDepth)
        platform =
            W.translate (V3 0 10 0) $
                W.translate (V3 0 bodyHeight 0) $
                    fullSlab
                    --  `W.difference` (cornerCircle 0 <> cornerCircle bedWidth)
          where
            fullSlab = W.box (V3 bedWidth platformThickness platformDepth)
            cornerCircle x =
                W.translate (V3 x 0 platformDepth) $
                    verticalCylinder shelfCurveRadius platformThickness
        bodyHeight = bedHeight - mattressHeight + headBoardHeight + shelfGap

fullSolid =
    W.translate (V3 leftWardrobeWidth 0 (getSum $ mwhen cupboardBehind cupboardDepth)) bed
        <> wardrobe

main :: IO ()
main = do
    W.writeSTL 1 "out.stl" fullSolid

-- writeColouredOBJ
--     1
--     "out.obj"
--     [ (bed, opaque saddlebrown)
--     , (wardrobe, opaque navajowhite)
--     ]

-- utils
verticalCylinder r h = W.scale (V3 r h r) $ W.rotate (unit _x) (-pi / 2) $ W.unitCylinder
