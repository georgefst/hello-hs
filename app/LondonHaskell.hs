-- https://github.com/joe-warren/london-haskell-logo
-- with `lwG` and `fontSizeG` for proper scaling
-- plus extra polymorphism
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module LondonHaskell (londonHaskellDiag) where

import Diagrams.Prelude

stationCircle :: _ => Point V2 Double -> Diagram b
stationCircle = joinedStations . pure

joinedStations :: _ => [Point V2 Double] -> Diagram b
joinedStations points =
    let trail = fromVertices points
    in  atPoints points (repeat (circle 2 # fc white # lwG 0))
            <> (trail # lc white # lwG 1)
            <> atPoints points (repeat (circle 3 # fc black # lwG 0))
            <> trail # lc black # lwG 3

circleLine =
    lc (sRGB24 255 211 0)

victoriaLine =
    lc (sRGB24 0 152 212)

bakerlooLine =
    lc (sRGB24 179 99 5)

centralLine =
    lc (sRGB24 227 32 23)

hammersmithAndCityLine =
    lc (sRGB24 243 169 187)

districtLine =
    lc (sRGB24 0 120 42)

leg = fromSegments
    [straight (r2 (-10, 10))
    , bezier3 zero (r2 (-2, 2)) (r2 (-2, 4))
    , straight (r2 (0, 4))
    ]

horizontalA = straight (r2 (24, 0)) `at` (p2 (6, 6))

horizontalB = straight (r2 (12, 0)) `at` (p2 (18, -6))

upperLine = translate (r2 (0, 1))
lowerLine = translate (r2 (0, -1))

lambdaTube :: _ => Diagram b
lambdaTube = mconcat
    [ joinedStations [zero, p2 (6, 6)]
    , stationCircle $ p2 (12, 0)
    , stationCircle $ p2 (18, (-6))
    , leg # lwG 2 # victoriaLine
    , leg # reflectY # lwG 2 # victoriaLine
    , leg # translate (12 *^ unit _x) # lwG 2 # bakerlooLine
    , leg # reflectY # reflectX # translate (12 *^ unit _x) # lwG 2 # bakerlooLine
    , leg # reflectY # translate (12 *^ unit _x) # lwG 2 # centralLine
    , horizontalA # stroke # lwG 2 # hammersmithAndCityLine # upperLine
    , horizontalA # stroke # lwG 2 # circleLine # lowerLine
    , horizontalB # stroke # lwG 2 # districtLine # lowerLine
    , horizontalB # stroke # lwG 2 # circleLine # upperLine
    ]
    # center

logoName =
    text "London Haskell"
        # font "P22 Johnston Underground"
        # fontSizeG 8
        # fc (sRGB24 0 54 136)


londonHaskellDiag :: _ => Diagram b
londonHaskellDiag =
    vsep 12
        [ lambdaTube
        , logoName
        ]
        # pad 1.5
