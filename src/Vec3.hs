module Vec3 where

data Vec3 a =
    Vec3    { getX :: a -- Left, right
            , getY :: a -- Up, Down
            , getZ :: a -- Front, Back
            }
    deriving (Show, Eq)

vec :: (Num a) => a -> a -> a -> Vec3 a
vec x y z = Vec3 { getX = x, getY = y, getZ = z }

(.+) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(.+) v1 v2 =
    Vec3    
        { getX = getX v1 + getX v2
        , getY = getY v1 + getY v2
        , getZ = getZ v1 + getZ v2
        }
infixl 7 .+

(.*) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(.*) v1 v2 =
    Vec3    
        { getX = getX v1 * getX v2
        , getY = getY v1 * getY v2
        , getZ = getZ v1 * getZ v2
        }
infixl 7 .*

scalarMult :: (Num a) => Vec3 a -> a -> Vec3 a
scalarMult v s =
    Vec3 
        { getX = getX v * s
        , getY = getY v * s
        , getZ = getZ v * s
        }

getLength :: (Floating a) => Vec3 a -> a
getLength v = sqrt $ sum $ map (\f -> f v ^ 2) [getX, getY, getZ]

normalize :: (Floating a) => Vec3 a -> Vec3 a
normalize = scalarMult <$> id <*> ((1/) . getLength)
