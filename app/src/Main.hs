module Main where

import Data.Matrix

import Triangle
import Transformations


newtype FieldOfView = FieldOfView Float -- FOV

data Rectangle = Rectangle Point Float Float


-- The viewport is the width then height, in pixels, to render along the width and height, in units.
data ViewPort = ViewPort Int Int


-- Given a viewport and a vector to cast along, get the list of all rays to cast
getIsometricRays :: ViewPort -> Rectangle -> Vector -> [Ray]
getIsometricRays (ViewPort viewWidth viewHeight) (Rectangle (Point px py pz) unitWidth unitHeight) vector =
    let
        widthIncrement = unitWidth / fromIntegral viewWidth;
        heightIncrement = unitHeight / fromIntegral viewHeight;
        (Point topLeftX topLeftY topLeftZ) = Point (px - unitWidth / 2) (py + unitHeight / 2) pz

        -- the ray cast from each row is found at a point based on both the row and columnIndex index
        -- take the row index a
        getRow rowIndex = map (\columnIndex -> Ray (Point (topLeftX + fromIntegral columnIndex * widthIncrement) (topLeftY - fromIntegral rowIndex * heightIncrement) topLeftZ) vector) [0..viewWidth - 1]
    in
        concatMap getRow [0..viewHeight - 1]


getRectanglePoints :: ViewPort -> Rectangle -> [Point]
getRectanglePoints (ViewPort viewWidth viewHeight) (Rectangle (Point px py pz) unitWidth unitHeight) = 
    let
        widthIncrement = unitWidth / fromIntegral viewWidth;
        heightIncrement = unitHeight / fromIntegral viewHeight;
        (Point topLeftX topLeftY topLeftZ) = Point (px - unitWidth / 2) (py + unitHeight / 2) pz
        getPoint rowIndex columnIndex = Point (topLeftX + fromIntegral columnIndex * widthIncrement) (topLeftY - fromIntegral rowIndex * heightIncrement) topLeftZ
        getRow rowIndex = map (getPoint rowIndex) [0..viewWidth - 1]
    in
        concatMap getRow [0..viewHeight - 1]

getPerspectiveRays :: ViewPort -> Rectangle -> Point -> [Ray]
getPerspectiveRays  viewport rectangle point =
       map (Ray point . vectorFromPoints point) (getRectanglePoints viewport rectangle)

getViewPortWindow :: ViewPort -> FieldOfView -> Rectangle
getViewPortWindow viewport (FieldOfView fov) =
    let
        (ViewPort viewWidth viewHeight) = viewport
        aspectRatio = fromIntegral viewHeight / fromIntegral viewWidth
        widthFromFov = 2 * tan (fov * pi / 360)
    in
        (Rectangle (Point 0 0 1) (widthFromFov / 2) (aspectRatio * widthFromFov))

castFromCamera :: ViewPort -> FieldOfView -> [Ray]
castFromCamera viewport fov = getPerspectiveRays viewport (getViewPortWindow viewport fov) (Point 0 0 0)

castRays :: [Ray] -> [Triangle] -> [Bool]
castRays rays triangles = map (\r -> any (doesRayIntersectTriangle r) triangles) rays

getBoolChar :: Bool -> Char
getBoolChar b = do
    if b
    then 'o'
    else '.'

getTestTriangles :: [Triangle]
getTestTriangles = [
        Triangle (Point (-1) (-1) 5) (Point 0 1 5)  (Point 1 (-1) 5),
        Triangle (Point (-1) (-1) 5) (Point 0 (-1) 9) (Point 1 (-1) 5)
    ]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

-- >>> getRotationMatrix (Degrees (-90)) (Degrees 0) (Degrees 0)
--                      
--   1.0  0.0  0.0  0.0 
--   0.0  0.0 -1.0  0.0 
--   0.0  1.0  0.0  0.0 
--   0.0  0.0  0.0  1.0 
--

-- >>> getTranslationMatrix 0 3 6
--                  
--  1.0 0.0 0.0 0.0 
--  0.0 1.0 0.0 3.0 
--  0.0 0.0 1.0 6.0 
--  0.0 0.0 0.0 1.0 
--

-- >>>  testTranslation `multStd` testRotation
--                      
--   1.0  0.0  0.0  0.0 
--   0.0  0.0 -1.0  3.0 
--   0.0  1.0  0.0  6.0 
--   0.0  0.0  0.0  1.0 
--
-- >>>  rotateRay (Ray (Point 0 0 0) (Vector 0 0 1)) testTransformation
-- Ray (Point 0.0 3.0 6.0) (Vector 0.0 2.0 6.0)
--
renderAsciiViewPort :: Int -> Int -> [Char]
renderAsciiViewPort width height =
    let
        viewport = (ViewPort width height)
        fov = (FieldOfView 120)
        rect = getViewPortWindow viewport fov

        testRotation = getRotationMatrix (Degrees (-90)) (Degrees 0) (Degrees 0)
        testTranslation = getTranslationMatrix 0 3 5
        testTransformation = testTranslation `multStd` testRotation
        
        windowPoints = map (flip rotatePoint testTransformation) (getRectanglePoints viewport rect)
        startPoint = rotatePoint (Point 0 0 0) testTransformation
        rays = map (\p -> rayFromPoints startPoint p) windowPoints

        rendering = castRays rays getTestTriangles
    in
        concatMap (\s -> s ++ "\r\n") (chunks width (map getBoolChar rendering))

testRayIntersection :: Ray -> Triangle -> IO ()
testRayIntersection ray triangle =
    let
        intersection = getIntersectionPoint ray (getPlaneFromTriangle triangle)
        barycentric = getBarycentricPoint intersection triangle
        normal = normalOfTriangle triangle
    in do
        print ("Triangle: " ++ show triangle)
        print ("Normal: " ++ show normal)
        print ("Area: " ++ show (areaOfTriangle triangle normal))
        print ("Intersection Point: " ++ show intersection)
        print ("Barycentric: " ++ show barycentric)
        print ("Does Ray Intersect: " ++ show (doesRayIntersectTriangle ray triangle))

main :: IO ()
main = putStr (renderAsciiViewPort 200 100)
