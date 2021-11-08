module Ray where

import Point
import Vector
import Triangle

data Ray = Ray Point Vector deriving Show

rayFromPoints p1 p2 = Ray p1 (vectorFromPoints p1 p2)

getIntersectionPoint :: Ray -> Plane -> Point
getIntersectionPoint (Ray (Point px py pz) (Vector vx vy vz)) (ScalarPlane a b c d) =
    let
        t = - (a * px + b * py + c * pz + d) / (a * vx + b * vy + c * vz)
    in
        Point (px + vx * t) (py + vy * t) (pz + vz * t)


doesRayIntersectTriangle :: Ray -> Triangle -> Bool
doesRayIntersectTriangle ray triangle = isPointInTriangle (getIntersectionPoint ray (getPlaneFromTriangle triangle)) triangle
