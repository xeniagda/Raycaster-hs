module Raycaster where

import Vec3
import Base
import Shapes


black = [0, 0, 0]
darkGray = [96, 96, 96]
gray = [128, 128, 128]
lightGray = [192, 192, 192]
lightGray' = [160, 160, 160]
white = [255, 255, 255]

defaultWorld :: World Float
defaultWorld =
    World
        { getCameraPos = vec 0 3 (-2)
        , getCameraRot = (-pi/8, pi/8)
        , getFOV = pi / 4 -- 22.5°
        , getShapes = 
            [ makeFloor  0                       $ diffuse  1 lightGray -- Ground
            , makeFloor  5                       $ emission 10 [255, 230, 200]    -- Light source
            , makeSquare (vec (-1) 1 4)    1   X $ diffuse  1 lightGray'
            , makeSquare (vec (-2) 2 4)    1   Y $ diffuse  1 lightGray'
            , makeSquare (vec (-2) 1 3)    1   Z $ diffuse  1 lightGray'
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

rayCast ::     (Int, Int)          -- Amount of bounces, Amount of steps max
            -> World Float  -- World
            -> Vec3 Float   -- Ray position
            -> Vec3 Float   -- Ray direction
            -> [Float]      -- Color, 0-255
rayCast (0, _) _ _ _ = gray -- Sky
rayCast (_, 0) _ _ _ = gray -- Sky

rayCast (bounces, steps) world rayStart dir =
    let rayEnd = rayStart .+ dir
        collidingShapes = filter (\s -> intersects s rayStart rayEnd) $ getShapes world
    in case collidingShapes of
        [] -> rayCast (bounces, steps - 1) world rayEnd $ dir `scalarMult` 2
        (wall:_) ->
            if getLength dir < 0.005
                then
                    let bounceDir = (getBounceDir wall dir)
                    in case bounceDir of
                        Just dir -> 
                            let bounceCol = rayCast (bounces - 1, steps) world rayStart $ normalize dir
                            in getColorMix wall bounceCol
                        Nothing -> getColorMix wall [0, 0, 0]
                else
                    rayCast (bounces, steps - 1) world rayStart $ dir `scalarMult` 0.5
            

getColor :: World Float -> (Float, Float) -> [Float]
getColor world pos =
    rayCast
        (3, 60)
        world
        (getCameraPos world)
        (getRayDirection defaultWorld pos `scalarMult` 1)

