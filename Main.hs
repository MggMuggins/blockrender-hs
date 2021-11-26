module Main where

import Codec.Picture.Png
import Codec.Picture.Types
import Linear hiding (project, trace)

import Debug.Trace

{-
data Vertex = Vertex {
    coords :: V4 Float
    -- Deal with these later
    --tex_coords :: V2 Int
}
    deriving (Show)
-}

--data Vertex = Vertex (V4 Float)
--    deriving (Show)

type Vertex4 = V4 Double

-- Shorthand for constructing Vertex4s
vert :: Double -> Double -> Double -> Vertex4
vert x y z = V4 x y z 1.0

-- Convert a vertex in 3D space to a vertex in 2D space
project_vertex :: M44 Double -> Vertex4 -> Vertex2
project_vertex proj_mtrx vert = let
    V4 x y _ _ = proj_mtrx !* vert
    in V2 (round x) (round y)

type Vertex2 = V2 Int

data Surface a = Surface a a a
    deriving (Show)

instance Functor Surface where
    fmap f (Surface a b c) = Surface (f a) (f b) (f c)

type Surface3D = Surface Vertex4
type Surface2D = Surface Vertex2

-- Take a vertex and a surface and give the barycentric coordinates of that
-- vertex relative to the surface
-- To be clear, I have never totally understood how this actually works
surface_barycentric :: Vertex2 -> Surface2D -> [Double]
surface_barycentric point surf = let
    (Surface a b c) = fmap (fmap fromIntegral) surf
    p = fmap fromIntegral point

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
surface_has :: Vertex2 -> Surface2D -> Bool
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

-- Image generator given some surfaces
vertexPixels :: [Surface2D] -> Int -> Int -> PixelRGBA8
vertexPixels surfaces x y =
    if any (surface_has (V2 x y)) surfaces
        then PixelRGBA8 0 0 0 255
        else PixelRGBA8 0 0 0 0

{-
data Bounds =
{   , min_x :: Int
    , max_x :: Int
    , min_y :: Int
    , max_y :: Int
}

bounds :: [Surface2D] -> Bounds
bounds surfaces = Bounds {
        min_x = min . map (\s -> )
    }
-}

main :: IO ()
main = let
    surfaces =[
        Surface
            (vert 0 0 0)
            (vert 0 1 0)
            (vert 1 0 0),
        Surface
            (vert 1 1 0)
            (vert 0 1 0)
            (vert 1 0 0),
        Surface
            (vert 1 0 0)
            (vert 1 1 0)
            (vert 1 0 1),
        Surface
            (vert 1 1 1)
            (vert 1 1 0)
            (vert 1 0 1),
        Surface
            (vert 0 1 0)
            (vert 0 1 1)
            (vert 1 1 0),
        Surface
            (vert 1 1 1)
            (vert 0 1 1)
            (vert 1 1 0)
        ]

    matrix = proj_matrix 30
    projected = project matrix surfaces

    test_bary_surf:_ = projected

    img = generateImage (vertexPixels projected) 50 50
    in do
        print test_bary_surf
        print $ surface_barycentric (V2 1 1) test_bary_surf
        writePng "out.png" img

