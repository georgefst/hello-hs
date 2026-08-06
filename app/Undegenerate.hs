-- this is a hacky vibed-up workaround for https://github.com/diagrams/diagrams-lib/issues/268#issuecomment-5209421453

module Undegenerate where

import Diagrams.Prelude

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

    -- Same reasoning as the upstream patch: ask whether `unitPerp` can actually
    -- produce a direction, rather than picking an arbitrary length threshold.
    -- (`unitPerp` itself isn't exported, hence `signorm . perp` inline.)
    degenerate v = not (all finite (signorm (perp v)))
    finite x = not (isNaN x || isInfinite x)

    eps = 1e-9
