module Raycaster where

import System.Random

import Vec3
import Base
import Shapes


black = [0, 0, 0]
darkGray = [96, 96, 96]
gray = [128, 128, 128]
lightGray = [192, 192, 192]
lightGray' = [160, 160, 160]
white = [255, 255, 255]

defaultWorld :: Props -> World Float
defaultWorld props =
    World
        { getCameraPos = vec 0 3 (-2)
        , getCameraRot = (-pi/8, pi/8)
        , getFOV = getFOVdeg props / 180 * pi * 2
        , getShapes =
            [ makeFloor  0                       $ diffuse  1 lightGray -- Ground
            , makeFloor  5                       $ emission 10 [255, 230, 200]    -- Light source
            , makeSquare (vec (-1) 1 4)    1   X $ diffuse  1 [255, 0, 0] -- Right
            , makeSquare (vec (-2) 2 4)    1   Y $ diffuse  1 [255, 255, 255] -- Up
            , makeSquare (vec (-2) 1 3)    1   Z $ diffuse  1 [0, 255, 0] -- Front
            , makeSquare (vec (-3) 1 4)    1   X $ diffuse  1 [255, 128, 0] -- Left
            , makeSquare (vec (-2) 2 4)    1   Y $ diffuse  1 [255, 255, 0] -- Down
            , makeSquare (vec (-2) 1 5)    1   Z $ diffuse  1 [0, 128, 255] -- Back
            , makeCirc   (vec (-1) 1 7)    3   Z $ diffuse  0 white -- Mirror
            , makeCirc   (vec (-1) 1 7.01) 3.2 Z $ diffuse  1 black -- Outline
            ]
        }

getRayDirection :: (Floating a) => World a -> (a, a) -> Vec3 a
getRayDirection world (x, y) =
    let xRad = x * getFOV world + (fst $ getCameraRot world)
        yRad = y * getFOV world + (snd $ getCameraRot world)
        vx = sin xRad
        vy = negate $ sin yRad
        vz = cos xRad * cos yRad
    in normalize $ vec vx vy vz

rayCast ::     StdGen       -- Random
            -> (Int, Int)   -- Amount of bounces, Amount of steps max
            -> Props
            -> World Float  -- World
            -> Vec3 Float   -- Ray position
            -> Vec3 Float   -- Ray direction
            -> [Float]      -- Color, 0-255
rayCast _ (0, _) _ _ _ _ = gray -- Sky
rayCast _ (_, 0) _ _ _ _ = gray -- Sky

rayCast rng (bounces, steps) props world rayStart dir =
    let rayEnd = rayStart .+ dir
        collidingShapes = filter (\s -> intersects s rayStart rayEnd) $ getShapes world
    in case collidingShapes of
        [] -> rayCast rng (bounces, steps - 1) props world rayEnd $ dir `scalarMult` getEmptyIncrease props
        (wall:_) ->
            if getLength dir < getMinCollide props
                then
                    let (rng', r) = split rng
                        bounceDir = getBounceDir wall r dir
                    in case bounceDir of
                        Just dir ->
                            let bounceCol = rayCast rng' (bounces - 1, steps) props world rayStart $ normalize dir
                            in getColorMix wall bounceCol
                        Nothing -> getColorMix wall [0, 0, 0]
                else
                    rayCast rng (bounces, steps - 1) props world rayStart $ dir `scalarMult` getCollideShrink props


getColor :: StdGen -> Props -> World Float -> (Float, Float) -> [Float]
getColor rng props world pos =
    rayCast
        rng
        (getMaxBounces props, getMaxSteps props)
        props
        world
        (getCameraPos world)
        (getRayDirection world pos)

