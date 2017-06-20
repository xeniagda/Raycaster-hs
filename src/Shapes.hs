module Shapes where

import Base
import Vec3
import System.Random

data Material a =
    Material
        { getMix :: [Float] -> [Float]
        , getMatBounceDir :: Vec3 a -> Maybe (Vec3 a) -- Ray -> New ray
        }

diffuseBounce :: Float -> Vec3 Float -> Vec3 Float
diffuseBounce scatter v =
    let r = makeRng v
        [x, y, z] = take 3 $ normals r
    in v .* (vec (x * scatter + 1) (y * scatter + 1) (z * scatter + 1))

diffuse :: Float -> [Float] -> WallDir -> Material Float
diffuse scatter color dir =
    Material
        { getMix =
            \col ->
                let avg = sum col / 3
                in 
                    zipWith (\c ref ->
                        let reflect = (ref + avg * 3) / 4 * c / 256
                        in (reflect * 3 + ref) / 4
                    ) color col
        , getMatBounceDir =
            \v -> Just $
                let bounce = 
                        case dir of
                            X -> vec (-1) 1 1
                            Y -> vec 1 (-1) 1
                            Z -> vec 1 1 (-1)
                in diffuseBounce scatter v .* bounce
        }

emission :: Float -> [Float] -> WallDir -> Material Float
emission strength color dir =
    Material
        { getMix =
            \col -> [255, 255, 255]
        , getMatBounceDir =
            \v -> Nothing
        }

data WallDir = X | Y | Z

makeSquare :: Vec3 Float -> Float -> WallDir -> (WallDir -> Material Float) -> Shape Float
makeSquare pos size dir material =
    Shape
        { intersects = \v1 v2 ->
            let [getInter, get1, get2] =
                    case dir of
                        X -> [getX, getY, getZ]
                        Y -> [getY, getX, getZ]
                        Z -> [getZ, getX, getY]
            in
                (getInter v1 < getInter pos) /= (getInter v2 < getInter pos) -- Goes through
                && get1 v1 > get1 pos - size && get1 v1 < get1 pos + size
                && get2 v1 > get2 pos - size && get2 v1 < get2 pos + size
        , getBounceDir = getMatBounceDir $ material dir
        , getColorMix = getMix $ material dir
        }

makeCirc :: Vec3 Float -> Float -> WallDir -> (WallDir -> Material Float) -> Shape Float
makeCirc pos size dir material =
    Shape
        { intersects = \v1 v2 ->
            let [getInter, get1, get2] =
                    case dir of
                        X -> [getX, getY, getZ]
                        Y -> [getY, getX, getZ]
                        Z -> [getZ, getX, getY]
                d1 = get1 v1 - get1 pos
                d2 = get2 v1 - get2 pos
            in
                (getInter v1 < getInter pos) /= (getInter v2 < getInter pos) -- Goes through
                && d1 ^ 2 + d2 ^ 2 < size ^ 2
        , getBounceDir = getMatBounceDir $ material dir
        , getColorMix = getMix $ material dir
        }

makeFloor :: Float -> (WallDir -> Material Float) -> Shape Float
makeFloor height material =
    Shape
        { intersects = \v1 v2 ->
            (getY v1 < height) /= (getY v2 < height)
        , getBounceDir = getMatBounceDir $ material Y
        , getColorMix = getMix $ material Y
        }
