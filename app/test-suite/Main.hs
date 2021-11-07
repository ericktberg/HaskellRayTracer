import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import Data.Matrix
import Transformations
import Triangle


rotateOriginStays :: Assertion
rotateOriginStays =
    let
        p0 = (Point 0 0 0)
        rotation = getRotationMatrix (Degrees 45) (Degrees 45) (Degrees 45)
        rotated = rotation `multStd` (pointToMatrix p0)

    in
        assertEqual "Origin matrix was transformed" p0 rotated

main :: IO ()
main = defaultMainWithOpts
       [testCase "rotate-origin" rotateOriginStays]
       mempty