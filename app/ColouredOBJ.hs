-- pure slop! wait for https://github.com/joe-warren/opencascade-hs/issues/44

{- | Writing a list of coloured `W.Solid`s to a single Wavefront OBJ file, with
an accompanying MTL file giving each solid its colour. Colour is the reason this
exists at all: every writer in "Waterfall.IO" takes one `W.Solid` and no colour,
because OpenCascade only carries colour in an XCAF document, which
opencascade-hs doesn't expose the colour tool for.

So we let OpenCascade mesh each solid on its own, via `W.writeOBJ` to a
temporary file, and merge the results into one file with a @usemtl@ per solid.
Textual surgery on someone else's output isn't elegant, but the alternative is
meshing by hand through @BRepMesh.IncrementalMesh@ and @Poly.Triangulation@, and
this way the geometry comes from the same code path as `W.writeSTL`.

OBJ was picked over glTF only because it's text: the colours are three floats in
a file we can write with `unlines`, and both f3d and Blender honour them.
-}
module ColouredOBJ (writeColouredOBJ) where

import Data.Colour
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import Data.Traversable (for)
import Numeric (showFFloat)
import System.Directory (removeFile)
import System.FilePath (replaceExtension, takeFileName)
import System.IO (readFile')
import Waterfall qualified as W

{- | Mesh each solid to the given tolerance, as `W.writeSTL` would, writing them
all to the one file, and their colours to the @.mtl@ beside it.
-}
writeColouredOBJ :: Double -> FilePath -> [(W.Solid, AlphaColour Double)] -> IO ()
writeColouredOBJ tolerance path layers = do
    meshes <- for (zip [0 ..] layers) \(i, (solid, _)) -> do
        -- alongside the output, rather than in a temporary directory, so that
        -- anything left behind by a failure is obvious
        let tmp = path <> "." <> show @Int i <> ".tmp.obj"
        W.writeOBJ tolerance tmp solid
        obj <- readFile' tmp
        removeFile tmp
        pure $ geometry obj
    writeFile mtlPath . unlines . concat $ zipWith material [0 ..] (snd <$> layers)
    writeFile path . unlines $
        ("mtllib " <> takeFileName mtlPath)
            : concat (zipWith3 group [0 ..] (scanl addCounts (0, 0, 0) (counts <$> meshes)) meshes)
  where
    mtlPath = replaceExtension path "mtl"
    material i c =
        [ "newmtl " <> name i
        , "Kd " <> unwords (num <$> [r, g, b])
        , "d " <> num a -- OBJ's opacity, confusingly
        ]
      where
        a = alphaChannel c
        -- compositing over black premultiplies by the alpha, which @d@ then
        -- applies for a second time, so divide it back out
        RGB r g b = (/ max 1e-9 a) <$> toSRGB (c `over` black)
    group i offsets ls = ("g " <> name i) : ("usemtl " <> name i) : fmap (renumber offsets) ls
    name i = "colour" <> show @Int i
    num x = showFFloat (Just 6) x ""

-- | The lines of an OBJ we care about: vertices, texture coordinates, normals and faces.
geometry :: String -> [String]
geometry = filter ((`elem` ["v", "vt", "vn", "f"]) . keyword) . lines

-- | How many vertices, texture coordinates and normals a mesh declares.
counts :: [String] -> (Int, Int, Int)
counts ls = (count "v", count "vt", count "vn")
  where
    count k = length $ filter ((== k) . keyword) ls

addCounts :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addCounts (a, b, c) (x, y, z) = (a + x, b + y, c + z)

-- | The first word of a line, or @""@ if it hasn't got one.
keyword :: String -> String
keyword = concat . take 1 . words

{- | Vertex indices in an OBJ are one-based and global to the file, so every face
of every solid after the first has to be shifted past the ones before it. A
reference is @v@, @v\/vt@, @v\/\/vn@ or @v\/vt\/vn@; OpenCascade always writes
the last of those, and never uses the negative, relative form that OBJ also
allows.
-}
renumber :: (Int, Int, Int) -> String -> String
renumber (v, vt, vn) l = case words l of
    "f" : refs -> unwords $ "f" : fmap ref refs
    _ -> l
  where
    ref = intercalate "/" . zipWith bump [v, vt, vn] . splitOn "/"
    bump offset i = if null i then i else show (read i + offset :: Int)
