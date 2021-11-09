module Main where

import Data.Matrix
import System.IO

import Triangle
import Transformations
import Point
import Ray
import Camera
import Text.Printf (printf, hPrintf)

getBoolChar :: Bool -> Char
getBoolChar b = do
    if b
    then 'o'
    else '.'

getTestTriangles :: [Triangle]
getTestTriangles = [
        Triangle (Point (-1) (-1) 5) (Point 0 1 5)  (Point 1 (-1) 5),
        Triangle (Point (-1) (-1) 6.5) (Point 0 (-1) 9) (Point 1 (-1) 6.5)
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
        fov = FieldOfView 120
        camera = PerspectiveCam (RenderedRegion width height) fov

        testRotation = getRotationMatrix (Degrees (-90)) (Degrees 0) (Degrees 45)
        testTranslation = getTranslationMatrix 2 3 6
        testTransformation = testTranslation `multStd` testRotation

        rays = calculateRays camera testTransformation

        rendering = castRays rays getTestTriangles
    in
        concatMap (++ "\r\n") (chunks width (map getBoolChar rendering))

main :: IO ()
main = 
    let 
        output = renderAsciiViewPort 200 100
    in
    do 
        outputHandle <- openFile "output.txt" WriteMode
        printf output
        hPrintf outputHandle output
        hClose outputHandle
