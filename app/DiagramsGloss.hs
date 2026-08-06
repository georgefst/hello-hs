-- from unmerged Agate branch `petri-diagrams-2-gloss`
-- needs a new home, though I've kind of gone off Gloss...
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

-- module Diagrams.Backend.Gloss (
module DiagramsGloss (
    B,
    Gloss (Gloss),
    Options (GlossOptions, sizeSpec, samples),
) where

import Data.Colour
import Data.List
import Data.Tree
import Diagrams.Core.Compile
import Diagrams.Core.Transform
import Diagrams.Prelude hiding (over)
import Diagrams.TwoD.Adjust
import Diagrams.TwoD.Text
import Graphics.Gloss qualified as G

data Gloss = Gloss

type B = Gloss

type instance V Gloss = V2
type instance N Gloss = Double

instance Backend Gloss V2 Double where
    newtype Render Gloss V2 Double = R (Options Gloss V2 Double -> G.Picture)
    type Result Gloss V2 Double = G.Picture
    data Options Gloss V2 Double = GlossOptions {sizeSpec :: SizeSpec V2 Double, samples :: Double}
    adjustDia = adjustDia2D $ lens (\(GlossOptions{sizeSpec}) -> sizeSpec) (\opts sizeSpec -> opts{sizeSpec})
    renderRTree Gloss opts@GlossOptions{sizeSpec = ss} =
        G.translate (-(realToFrac ssx / 2)) (-(realToFrac ssy / 2))
            . go
      where
        V2 ssx ssy = specToSize 0 ss
        colourToGloss c = G.makeColor r g b a
          where
            ac = toAlphaColour c
            RGB r g b = realToFrac <$> toSRGB (ac `over` black)
            a = realToFrac $ alphaChannel ac
        go (Node n ts) =
            G.pictures (go <$> ts) & case n of
                RStyle s -> case getFillTexture @Double <$> getAttr s of
                    Just (SC (SomeColor c)) -> G.color $ colourToGloss c
                    Just (LG _) -> id
                    Just (RG _) -> id
                    Nothing -> id
                RAnnot _ -> id
                RPrim p -> (p' opts <>)
                  where
                    R p' = render Gloss p
                REmpty -> id

instance Semigroup (Render Gloss V2 Double) where
    R a <> R b = R $ G.pictures <$> sequence [a, b]
instance Monoid (Render Gloss V2 Double) where
    mempty = R $ pure G.blank

instance Renderable (Path V2 Double) Gloss where
    render Gloss path = R \GlossOptions{samples} -> G.pictures . map (renderTrail samples) . pathTrails $ path
      where
        renderTrail numSamples trail =
            withTrail
                (const G.line)
                (const G.polygon)
                (unLoc trail)
                . concatMap (\seg -> [unp2 $ realToFrac <$> atParam seg (i / numSamples) | i <- [0 .. numSamples]])
                $ fixTrail trail

instance Renderable (Text Double) Gloss where
    render Gloss (Text tt ta s) =
        R
            . pure
            . G.translate (realToFrac tx) (realToFrac ty)
            . G.scale (realToFrac sx) (realToFrac sy)
            . G.scale 0.07 0.07
            . G.translate (-(72 * realToFrac ax * genericLength s)) (-(100 * realToFrac ay))
            $ G.text s
      where
        (ax, ay) = case ta of
            BoxAlignedText x y -> (x, y)
            _ -> (0, 0)
        V2 sx sy = apply tt 1
        P (V2 tx ty) = papply tt 0
