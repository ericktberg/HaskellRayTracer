module Main where

data Point = Point Float Float Float deriving Show

data Rectangle = Rectangle
 Point -- the location at center
 Float -- Width in "Units"
 Float -- Height in "Units"

data Vector = Vector Float Float Float deriving Show

data Triangle = Triangle Point Point Point deriving Show

data Ray = Ray Point Vector deriving Show

-- The viewport is the width then height, in pixels, to render along the width and height, in units.
data ViewPort = ViewPort Int Int

-- Definition of scalar plane: ax + by + cz + d = 0
data Plane = ScalarPlane Float Float Float Float deriving Show

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

normalOfTriangle :: Triangle -> Vector
normalOfTriangle (Triangle a b c) = normalize (cross (vectorFromPoints a b) (vectorFromPoints a c))

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

castRays :: [Ray] -> [Triangle] -> [Bool]
castRays rays triangles = map (\r -> any (doesRayIntersectTriangle r) triangles) rays

getBoolChar :: Bool -> Char
getBoolChar b = do
    if b
    then 'o'
    else '.'

getTestTriangles :: [Triangle]
getTestTriangles = [
        Triangle (Point (-2) (-1) 5) (Point 0 2 5)  (Point 1 (-2) 5),
        Triangle (Point 0 0 3) (Point 2 (-1) 4) (Point 1.5 (-1.5) 6)
    ]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

renderAsciiViewPort :: Int -> Int -> [Char]
renderAsciiViewPort width height = concatMap (\s -> s ++ "\r\n") (chunks width (map getBoolChar (castRays (getIsometricRays (ViewPort width height) (Rectangle (Point 0 0 0) 5 5) (Vector 0 0 1)) getTestTriangles)))

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
