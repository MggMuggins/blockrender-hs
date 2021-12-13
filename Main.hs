module Main where

import Data.List
import Debug.Trace

import Codec.Picture
import Codec.Picture.Png
import Codec.Picture.Types
import Linear hiding (project, trace)

data Vertex a = Vertex {
    verts :: a,
    tex_coords :: V2 Int
}
    deriving (Show)

-- Replace the vertices of a vertex, retaining the texture coordinates
replace_verts :: Vertex a -> b -> Vertex b
replace_verts old new_verts = Vertex {
        verts = new_verts,
        tex_coords = tex_coords old
    }

type Vertex4 = Vertex (V4 Double)

-- Shorthand for constructing Vertex4s
vert :: Double -> Double -> Double -> Int -> Int -> Vertex4
vert x y z a b = Vertex {
    verts = V4 x y z 1.0,
    tex_coords = V2 a b
}

-- Convert a vertex in 3D space to a vertex in 2D space
project_vertex :: M44 Double -> Vertex4 -> Vertex2
project_vertex proj_mtrx vert = let
    V4 x y _ _ = proj_mtrx !* (verts vert)
    in replace_verts vert $ V2 (round x) (round y)

type Vertex2 = Vertex (V2 Int)

vertex2_to_f :: Vertex2 -> V2 Double
vertex2_to_f vertex = fmap (fromIntegral) (verts vertex)

-- Given a comparison vector, return it updated with any new max
vertex2_maxes :: V2 Int -> Vertex2 -> V2 Int
vertex2_maxes (V2 other_x other_y) (Vertex (V2 self_x self_y) _) =
    V2 (max self_x other_x) (max self_y other_y)

-- Given a vector to compare against, return an updated version with any new
-- minimums.
vertex2_mins :: V2 Int -> Vertex2 -> V2 Int
vertex2_mins (V2 other_x other_y) (Vertex (V2 self_x self_y) _) =
    V2 (min self_x other_x) (min self_y other_y)

data Surface a = Surface a a a
    deriving (Show)

instance Functor Surface where
    fmap f (Surface a b c) = Surface (f a) (f b) (f c)

instance Foldable Surface where
    foldr f z (Surface a b c) = f c (f b (f a z))

type Surface3D = Surface Vertex4
type Surface2D = Surface Vertex2

surface2_max :: V2 Int -> Surface2D -> V2 Int
surface2_max init surf = foldl vertex2_maxes init surf

surface2_min :: V2 Int -> Surface2D -> V2 Int
surface2_min init surf = foldl vertex2_mins init surf

-- Take a point and a surface and give the barycentric coordinates of the
-- point relative to the surface
-- To be clear, I have never totally understood how this actually works
surface_barycentric :: V2 Int -> Surface2D -> [Double]
surface_barycentric point surf = let
    p = fmap fromIntegral point
    (Surface a b c) = fmap vertex2_to_f surf

    ab = b - a
    ac = c - a
    ap = p - a

    V2 ax ay = a
    V2 bx by = b
    V2 cx cy = c

    nac = V2 (ay - cy) (cx - ax)
    nab = V2 (ay - by) (bx - ax)

    beta = dot ap nac / dot ab nac
    gamma = dot ap nab / dot ac nab
    alpha = 1.0 - beta - gamma
    in [alpha, beta, gamma]

-- Determine if a 2D surface contains some vertex
surface_has :: V2 Int -> Surface2D -> Bool
surface_has vert = (all (>0.0) . (surface_barycentric vert))

-- Consts for defining the transformation matrix
y_ROT :: Double
y_ROT = -pi / 4

x_ROT :: Double
x_ROT = pi / 6

proj_matrix :: Double -> M44 Double
proj_matrix scale_fact = let
    rot_y = V4
        (V4 (cos y_ROT) 0 (-sin y_ROT) 0)
        (V4 0 1 0 0)
        (V4 (sin y_ROT) 0 (cos y_ROT) 0)
        (V4 0 0 0 1)
    rot_x = V4
        (V4 1 0 0 0)
        (V4 0 (cos x_ROT) (-sin x_ROT) 0)
        (V4 0 (sin x_ROT) (cos x_ROT) 0)
        (V4 0 0 0 1)
    ortho_proj = V4
        (V4 1 0 0 0)
        (V4 0 1 0 0)
        (V4 0 0 0 0)
        (V4 0 0 0 1)
    in scale_fact *!! ortho_proj !*! rot_x !*! rot_y

-- Project some 3D surfaces to 2D space
project :: M44 Double -> [Surface3D] -> [Surface2D]
project proj_mtrx = map (fmap (project_vertex proj_mtrx))

-- Image generator given some surfaces and a texture image
vertexPixels :: Image PixelRGBA8 -> [Surface2D] -> Int -> Int -> PixelRGBA8
vertexPixels img surfaces x y = let
        pixel_coords = V2 x y
    in case find (surface_has pixel_coords) surfaces of
        Just surf -> let
            bary = surface_barycentric pixel_coords surf
            (Surface a b c) = fmap tex_coords surf
            texture_coords = map (fmap fromIntegral) [a, b, c] :: [V2 Double]
            (V2 x y) = foldl1 (+) (map (\(c, b) -> c ^* b) (zip texture_coords bary))
            in pixelAt img (round x) (round y)
        Nothing -> PixelRGBA8 0 0 0 0

data Bounds = Bounds {
    rendermin :: V2 Int,
    rendermax :: V2 Int
} deriving Show

renderBounds :: [Surface2D] -> Bounds
renderBounds surfaces = Bounds {
        rendermin = foldl surface2_min (V2 maxBound maxBound) surfaces,
        rendermax = foldl surface2_max (V2 minBound minBound) surfaces
    }

main :: IO ()
main = let
    surfaces =[
        -- z = 0 plane
        Surface
            (vert 0 0 0 0 0)
            (vert 0 1 0 0 15)
            (vert 1 0 0 15 0),
        Surface
            (vert 1 1 0 15 15)
            (vert 0 1 0 0 15)
            (vert 1 0 0 15 0),
        -- x = 1 plane
        Surface
            (vert 1 0 0 0 0)
            (vert 1 1 0 0 15)
            (vert 1 0 1 15 0),
        Surface
            (vert 1 1 1 15 15)
            (vert 1 1 0 0 15)
            (vert 1 0 1 15 0),
        -- y = 1 plane
        Surface
            (vert 0 1 0 0 0)
            (vert 0 1 1 0 15)
            (vert 1 1 0 15 0),
        Surface
            (vert 1 1 1 15 15)
            (vert 0 1 1 0 15)
            (vert 1 1 0 15 0)
        ]

    matrix = proj_matrix 100
    projected = project matrix surfaces

    test_bary_surf:_ = projected

    in do
        tex <- readPng "rhunCraftingTable_side.png"
        case tex of
            Left err -> print err
            Right tex -> do
                let img = convertRGBA8 tex
                let (Bounds _ (V2 max_x max_y)) = renderBounds projected
                let new_img = generateImage (vertexPixels img projected) max_x max_y
                print max_x
                print max_y
                writePng "out.png" new_img

