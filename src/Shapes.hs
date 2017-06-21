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
            \col -> color
        , getMatBounceDir =
            \v -> Nothing
        }

data WallDir = X | Y | Z

getDirs :: (Num a) => WallDir -> ((Vec3 a -> a), (Vec3 a -> a), (Vec3 a -> a))
getDirs dir =
    case dir of
        X -> (getX, getY, getZ)
        Y -> (getY, negate . getX, getZ)
        Z -> (getZ, getY, negate . getX)

rotate from to pos =
    let (a, b, c) = getDirs from
    in case to of
        X -> vec (a pos) (b pos) (c pos)
        Y -> vec (negate $ b pos) (a pos) (c pos)
        Z -> vec (negate $ c pos) (b pos) (a pos)

makeSquare :: Vec3 Float -> Float -> WallDir -> (WallDir -> Material Float) -> Shape Float
makeSquare pos size dir material =
    Shape
        { intersects = \v1 v2 ->
            let (getInter, get1, get2) = getDirs dir
                inter = getIntersection dir pos v1 v2
            in
                (getInter v1 < getInter pos) /= (getInter v2 < getInter pos) -- Goes through
                && get1 inter > get1 pos - size && get1 inter < get1 pos + size
                && get2 inter > get2 pos - size && get2 inter < get2 pos + size
        , getBounceDir = getMatBounceDir $ material dir
        , getColorMix = getMix $ material dir
        }

makeCirc :: Vec3 Float -> Float -> WallDir -> (WallDir -> Material Float) -> Shape Float
makeCirc pos size dir material =
    Shape
        { intersects = \v1 v2 ->
            let (getInter, get1, get2) = getDirs dir
                inter = getIntersection dir pos v1 v2
                d1 = get1 inter - get1 pos
                d2 = get2 inter - get2 pos
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

getIntersection dir pos v1 v2 =
    rotate X dir $ getInterX (getX $ rotate dir X pos) (rotate dir X v1) (rotate dir X v2)

getInterX x v1 v2 =
    let y = (x - getX v1) * (getY v1 - getY v2) / (getX v1 - getX v2) + getY v1
        z = (x - getX v1) * (getZ v1 - getZ v2) / (getX v1 - getX v2) + getZ v1
    in vec x y z
