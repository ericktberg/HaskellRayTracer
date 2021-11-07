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


castFromCamera :: ViewPort -> FieldOfView -> [Ray]
castFromCamera viewport (FieldOfView fov) =
    let
        (ViewPort viewWidth viewHeight) = viewport
        aspectRatio = fromIntegral viewHeight / fromIntegral viewWidth
        widthFromFov = 2 * tan (fov * pi / 360)
    in
        getPerspectiveRays viewport (Rectangle (Point 0 0 1) (widthFromFov / 2) (aspectRatio * widthFromFov)) (Point 0 0 0)

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

renderAsciiViewPort :: Int -> Int -> [Char]
renderAsciiViewPort width height =
    let
        rays = castFromCamera (ViewPort width height) (FieldOfView 120)
        translation = getTranslationMatrix 0 4 6
        rotation = getRotationMatrix (Degrees (90)) (Degrees 0) (Degrees 0)
        transformation = translation `multStd` rotation
        rotatedRays = map (flip rotateRay transformation) rays
        rendering = castRays rotatedRays getTestTriangles
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
