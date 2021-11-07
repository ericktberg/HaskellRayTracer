module Triangle where

data Point = Point Float Float Float deriving Show
data Vector = Vector Float Float Float deriving Show
data Triangle = Triangle Point Point Point deriving Show
data Ray = Ray Point Vector deriving Show
-- Definition of scalar plane: ax + by + cz + d = 0
data Plane = ScalarPlane Float Float Float Float deriving Show

-- |
-- Gets a unit-length vector that defines the normal of a triangle
-- 
-- >>> normalOfTriangle (Triangle (Point 2 2 0) (Point 4 5 3) (Point 3 2 1))
-- Vector 0.6882472 0.22941573 (-0.6882472)
--
normalOfTriangle :: Triangle -> Vector
normalOfTriangle (Triangle a b c) = normalize (cross (vectorFromPoints a b) (vectorFromPoints a c))

-- |
-- Gets a plane definition in format ax + by + cz + d = 0
--
-- >>> getPlaneFromTriangle (Triangle (Point 0 1 0) (Point 1 1 1) (Point 0 0 3))
-- ScalarPlane 1.0 (-3.0) (-1.0) 3.0
--
getPlaneFromTriangle :: Triangle -> Plane
getPlaneFromTriangle (Triangle p q r) =
    let
        (Vector a b c) = cross (vectorFromPoints p q) (vectorFromPoints p r)
        (Point px0 py0 pz0) = p
        d = - (a * px0 + b * py0 + c * pz0)
    in
        ScalarPlane a b c d

getIntersectionPoint :: Ray -> Plane -> Point
getIntersectionPoint (Ray (Point px py pz) (Vector vx vy vz)) (ScalarPlane a b c d) =
    let
        t = - (a * px + b * py + c * pz + d) / (a * vx + b * vy + c * vz)
    in
        Point (px + vx * t) (py + vy * t) (pz + vz * t)

        
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

getBarycentricPoint :: Point -> Triangle -> Point
getBarycentricPoint p triangle = let
        -- calculate the barycentric coordinates of the point
        -- u, v, and w are defined using the relationship between a subtriangle formed with the given point and the main triangle
        -- Properties: 
        --     u + v + w = 1
        (Triangle a b c) = triangle
        normal = normalOfTriangle triangle

        abcArea = areaOfTriangle triangle normal


        capArea = areaOfTriangle (Triangle c a p) normal
        u = capArea / abcArea

        abpArea = areaOfTriangle (Triangle a b p) normal
        v = abpArea / abcArea

        bcpArea = areaOfTriangle (Triangle b c p) normal
        w = bcpArea / abcArea
    in Point u v w

isPointInTriangle :: Point -> Triangle -> Bool
isPointInTriangle p triangle =
    let
        (Point u v w) = getBarycentricPoint p triangle
    in (u >= 0 && u <= 1) && (v >= 0 && v <= 1) && (w >= 0 && w <= 1)

areaOfTriangle :: Triangle -> Vector -> Float
areaOfTriangle (Triangle p q r) normal = dotProduct (cross (vectorFromPoints p q) (vectorFromPoints p r)) normal / 2

doesRayIntersectTriangle :: Ray -> Triangle -> Bool
doesRayIntersectTriangle ray triangle = isPointInTriangle (getIntersectionPoint ray (getPlaneFromTriangle triangle)) triangle
