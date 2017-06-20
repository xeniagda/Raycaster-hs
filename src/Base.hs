module Base where

import Vec3
import System.Random

data World a =
    World
        { getCameraPos :: Vec3 a
        , getCameraRot :: (a, a) -- (Pan upwards, Tilt)
        , getFOV :: a
        , getShapes :: [Shape a]
        }

data Shape a =
    Shape
        { intersects :: Vec3 a -> Vec3 a -> Bool -- Ray start -> Ray end -> intersects?
        , getBounceDir :: Vec3 a -> Maybe (Vec3 a) -- Ray -> New ray
        , getColorMix :: [a] -> [a]
        }

-- Make a pseudo-random StdGen from a Vector, useful for random numbers in pure contexts
makeRng :: Vec3 Float -> StdGen
makeRng v = mkStdGen $ fromIntegral $ floor (getX v * 1697465 + getY v * 9405860 + getZ v * 3856862)


-- Stolen from https://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/src/Data-Random-Normal.html
-- Couldn't get it to install properly :(


-- Normal distribution approximation
-- ---------------------------------
-- | Box-Muller method for generating two normally distributed
-- independent random values from two uniformly distributed
-- independent random values.
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = 
    (r * cos t, r * sin t)
        where r = sqrt (-2 * log u1)
              t = 2 * pi * u2

-- Convert a list of uniformly distributed random values into a
-- list of normally distributed random values. The Box-Muller
-- algorithms converts values two at a time, so if the input list
-- has an uneven number of element the last one will be discarded.
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
boxMullers _          = []

-- Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is analogous to 'Random.randoms'.
normals :: (RandomGen g, Random a, Floating a) => g -> [a]
normals = boxMullers . randoms
