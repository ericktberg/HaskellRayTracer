module Vector where

import Point

data Vector = Vector Float Float Float deriving Show

-- the cross product AxB is an vector orthogonal to both vectors
-- compute this using the determinant method
cross :: Vector -> Vector -> Vector
cross (Vector ax ay az) (Vector bx by bz) = Vector (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

dotProduct :: Vector -> Vector -> Float
dotProduct (Vector ax ay az) (Vector bx by bz) = ax * bx + ay * by + az * bz

normalize :: Vector -> Vector
normalize vector =
    let
        (Vector vx vy vz) = vector
        magnitude = vectorLength vector
    in
        Vector (vx / magnitude) (vy / magnitude) (vz / magnitude)

vectorFromPoints :: Point -> Point -> Vector
vectorFromPoints (Point p1x p1y p1z) (Point p2x p2y p2z) = Vector (p2x - p1x) (p2y - p1y) (p2z - p1z)

vectorLength :: Vector -> Float
vectorLength (Vector vx vy vz) = sqrt (vx^2 + vy^2 + vz^2)
