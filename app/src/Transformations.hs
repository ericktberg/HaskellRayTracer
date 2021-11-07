module Transformations where

import Data.Matrix

import Triangle

newtype Radians = Radians Float deriving Show
newtype Degrees = Degrees Float deriving Show

degreesToRadians :: Degrees -> Radians
degreesToRadians (Degrees value) = Radians (value * pi / 180)

radiansToDegrees :: Radians -> Degrees
radiansToDegrees (Radians value) = Degrees (value * 180 / pi)


-- |
-- Creates a rotation matrix around the x-axis
-- Because of floating point error, values that should be zero from sin and cos are not.
-- We coerce any value less than 10^-7 to 0
--
-- >>> xRotationMatrixRadians (degreesToRadians $ Degrees 45)
--                                                  
--          1.0         0.0         0.0         0.0 
--          0.0  0.70710677  0.70710677         0.0 
--          0.0 -0.70710677  0.70710677         0.0 
--          0.0         0.0         0.0         1.0 
--                                                  
--
xRotationMatrixRadians :: Radians -> Matrix Float
xRotationMatrixRadians (Radians angle) = fmap (coerceZero 0.0000001) $ matrix 4 4 indices
    where 
        indices (1,1) = 1
        indices (2, 2) = cos angle
        indices (2, 3) = sin angle
        indices (3, 2) = -sin angle
        indices (3, 3) = cos angle
        indices (4, 4) = 1
        indices _ = 0

-- |
-- Creates a rotation matrix around the y-axis
-- Because of floating point error, values that should be zero from sin and cos are not.
-- We coerce any value less than 10^-7 to 0
--
-- >>> yAxisRotationMatrixRadians (degreesToRadians $ Degrees 60)
--                                                  
--   0.49999997         0.0  0.86602545         0.0 
--          0.0         1.0         0.0         0.0 
--  -0.86602545         0.0  0.49999997         0.0 
--          0.0         0.0         0.0         1.0 
--         
yAxisRotationMatrixRadians :: Radians -> Matrix Float
yAxisRotationMatrixRadians (Radians angle) = fmap (coerceZero 0.0000001) $ matrix 4 4 indices
    where 
        indices (2,2) = 1
        indices (1, 1) = cos angle
        indices (1, 3) = sin angle
        indices (3, 1) = -sin angle
        indices (3, 3) = cos angle
        indices (4, 4) = 1
        indices _ = 0

coerceZero :: Float -> Float -> Float
coerceZero epsilon value    | value == 0 = 0
                            | (abs value) < epsilon = 0
                            | value /= 0 = value

-- |
-- Creates a rotation matrix around the z-axis
-- Because of floating point error, values that should be zero from sin and cos are not.
-- We coerce any value less than 10^-7 to 0
--
-- >>> zAxisRotationMatrixRadians (degreesToRadians $ Degrees 90)
--                      
--   0.0  1.0  0.0  0.0 
--  -1.0  0.0  0.0  0.0 
--   0.0  0.0  1.0  0.0 
--   0.0  0.0  0.0  1.0 
--
zAxisRotationMatrixRadians :: Radians -> Matrix Float
zAxisRotationMatrixRadians (Radians angle) = fmap (coerceZero 0.0000001) $ matrix 4 4 indices
    where 
        indices (3,3) = 1
        indices (1, 1) = cos angle
        indices (1, 2) = sin angle
        indices (2, 1) = -sin angle
        indices (2, 2) = cos angle
        indices (4, 4) = 1
        indices _ = 0

getRotationMatrix :: Degrees -> Degrees -> Degrees -> Matrix Float
getRotationMatrix xAxis yAxis zAxis = 
    (xRotationMatrixRadians (degreesToRadians xAxis)) `multStd` 
    (yAxisRotationMatrixRadians (degreesToRadians yAxis)) `multStd`
    (zAxisRotationMatrixRadians (degreesToRadians zAxis))

getTranslationMatrix :: Float -> Float -> Float -> Matrix Float
getTranslationMatrix x y z = matrix 4 4 indices
    where
        indices (1, 1) = 1
        indices (2, 2) = 1
        indices (3, 3) = 1
        indices (4, 4) = 1
        indices (1, 4) = x
        indices (2, 4) = y
        indices (3, 4) = z
        indices _ = 0

scalarsToMatrix :: Float -> Float -> Float -> Matrix Float
scalarsToMatrix x y z = matrix 4 1 indices
    where
        indices (1, 1) = x
        indices (2, 1) = y
        indices (3, 1) = z
        indices (4, 1) = 1

vectorToMatrix :: Vector -> Matrix Float
vectorToMatrix (Vector x y z) = scalarsToMatrix x y z

pointToMatrix :: Point -> Matrix Float
pointToMatrix (Point x y z) = scalarsToMatrix x y z

matrixToVector :: Matrix Float -> Vector
matrixToVector vectorMatrix = Vector (getElem 1 1 vectorMatrix) (getElem 2 1 vectorMatrix) (getElem 3 1 vectorMatrix)

matrixToPoint :: Matrix Float -> Point
matrixToPoint vectorMatrix = Point (getElem 1 1 vectorMatrix) (getElem 2 1 vectorMatrix) (getElem 3 1 vectorMatrix)

rotateVector :: Vector -> Matrix Float -> Vector
rotateVector vector rotationMatrix = matrixToVector $ multStd rotationMatrix (vectorToMatrix vector)

rotatePoint :: Point -> Matrix Float -> Point
rotatePoint point rotationMatrix = matrixToPoint $ multStd rotationMatrix (pointToMatrix point)

rotateRay :: Ray -> Matrix Float -> Ray
rotateRay (Ray point vector) rotationMatrix = Ray (rotatePoint point rotationMatrix) (rotateVector vector rotationMatrix)