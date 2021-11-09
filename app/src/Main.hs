{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Matrix ( multStd )
import System.IO
import Data.Word
import Data.Array.IArray

import Graphics.Pgm

import Triangle
import Transformations
import Point
import Ray
import Camera
import Text.Printf (printf, hPrintf)
import Data.Array.Unboxed (IArray)

data RGB = RGB Float Float Float

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

renderViewPort :: Int -> Int -> [Bool]
renderViewPort width height =
    let
        fov = FieldOfView 120
        camera = PerspectiveCam (RenderedRegion width height) fov

        testRotation = getRotationMatrix (Degrees (-90)) (Degrees (10)) (Degrees (0))
        testTranslation = getTranslationMatrix (-2) 3 6
        testTransformation = testTranslation `multStd` testRotation

        rays = calculateRays camera testTransformation
    in
        castRays rays getTestTriangles

simpleGrayscale :: Bool -> Word16
simpleGrayscale True = 255
simpleGrayscale False = 0

-- |
-- 
-- >>> mapIndices [True, False, False, True] 2 2
-- [((0,0),True),((0,1),False),((1,0),False),((1,1),True)]
--
mapIndices :: Int -> Int -> [a] -> [((Int, Int), a)]
mapIndices height width values = [(\ (i, j) -> ((i, j), values !! (i * width + j))) (i, j) | i <- [0 .. height -1], j <- [0 .. width -1]]

-- 
-- >>> toArray [True, False, False, True] 2 2
-- array ((0,0),(1,1)) [((0,0),True),((0,1),False),((1,0),False),((1,1),True)]
toArray :: IArray Array Bool =>  [Bool] -> Int -> Int -> Array (Int, Int) Bool
toArray values height width = array ((0, 0), (height - 1, width - 1)) (mapIndices height width values) 

--
-- >>> toWordArray 2 2 [True, False, False, True]
-- array ((0,0),(1,1)) [((0,0),255),((0,1),0),((1,0),0),((1,1),255)]
--
toWordArray height width result = amap simpleGrayscale (toArray result height width)

main :: IO ()
main =
    let
        result = renderViewPort 200 100
        asciiOutput = concatMap (++ "\r\n") (chunks 200 (map getBoolChar result))
    in
    do
        -- Write the ASCII data to file
        outputHandle <- openFile "output.txt" WriteMode
        printf asciiOutput
        hPrintf outputHandle asciiOutput
        hClose outputHandle

        arrayToFile "test.pgm" (toWordArray 100 200 result)
        
