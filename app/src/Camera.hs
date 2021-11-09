module Camera where

import Data.Matrix

import Point
import Vector
import Ray
import Transformations

data Rectangle = Rectangle Point Float Float
newtype FieldOfView = FieldOfView Float -- FOV

data RenderedRegion = RenderedRegion Int Int

data Camera = PerspectiveCam RenderedRegion FieldOfView | IsometricCam RenderedRegion Float

getRectanglePoints :: RenderedRegion -> Rectangle -> [Point]
getRectanglePoints (RenderedRegion viewWidth viewHeight) (Rectangle (Point px py pz) unitWidth unitHeight) =
    let
        widthIncrement = unitWidth / fromIntegral viewWidth;
        heightIncrement = unitHeight / fromIntegral viewHeight;
        (Point topLeftX topLeftY topLeftZ) = Point (px - unitWidth / 2) (py + unitHeight / 2) pz
        getPoint rowIndex columnIndex = Point (topLeftX + fromIntegral columnIndex * widthIncrement) (topLeftY - fromIntegral rowIndex * heightIncrement) topLeftZ
        getRow rowIndex = map (getPoint rowIndex) [0..viewWidth - 1]
    in
        concatMap getRow [0..viewHeight - 1]


-- |
-- Get the world-scale window that will be used by a perspective camera to generate rays from
--
getRenderWindow :: RenderedRegion -> FieldOfView -> Rectangle
getRenderWindow (RenderedRegion viewWidth viewHeight) (FieldOfView fov) =
    let
        aspectRatio = fromIntegral viewHeight / fromIntegral viewWidth
        widthFromFov = 2 * tan (fov * pi / 360)
    in
        Rectangle (Point 0 0 1) (widthFromFov / 2) (aspectRatio * widthFromFov)

-- |
-- Compute the array of rays associated with a camera.
calculateRays :: Camera -> Matrix Float -> [Ray]
calculateRays (IsometricCam (RenderedRegion width height) scaleFactor) transform =
    let
        unitWidth = fromIntegral width * scaleFactor
        unitHeight = fromIntegral height * scaleFactor

        widthIncrement = unitWidth / fromIntegral width;
        heightIncrement = unitHeight / fromIntegral height;
        (Point topLeftX topLeftY topLeftZ) = Point (unitWidth / 2) (unitHeight / 2) 0
        vector = Vector 0 0 1

        -- the ray cast from each row is found at a point based on both the row and columnIndex index
        -- take the row index a
        getRow rowIndex = map (\columnIndex -> Ray (Point (topLeftX + fromIntegral columnIndex * widthIncrement) (topLeftY - fromIntegral rowIndex * heightIncrement) topLeftZ) vector) [0..width - 1]
    in
        concatMap getRow [0..height - 1]

calculateRays (PerspectiveCam region fov) transform =
    let
        rect = getRenderWindow region fov
        windowPoints =  map (`rotatePoint` transform) (getRectanglePoints region rect)
        startPoint = rotatePoint (Point 0 0 0) transform
    in
        map (rayFromPoints  startPoint) windowPoints

