module Lib where

import System.IO
import System.Random
import qualified Data.ByteString as BS
import Data.Char

import Raycaster

import Vec3
import Base

strToByteString :: String -> BS.ByteString
strToByteString = BS.pack . map (fromIntegral . ord)

generatePPM :: (Integral a) => [[[a]]] -> BS.ByteString -- Image is (width * height * 3 colors)
generatePPM image =
    let width = length $ image !! 0
        height = length image
        header = strToByteString $ "P6 " ++ show width ++ " " ++ show height ++ " 255\n"
        content = BS.pack $ map (fromIntegral . max 0 . min 255) $ concat $ concat image
    in header `BS.append` content

pixToSpace :: (Fractional a) => (a, a) -> (a, a) -> (a, a)
pixToSpace (width, height) (x, y) =
    let avg = (width + height) / 2
    in
        ( x / avg * 2 - 1
        , y / avg * 2 - 1)

render :: (Integral a) => (Float, Float) -> (Float, Float) -> [a]
render (width, height) (x, y) =
    map
        (floor . realToFrac)
        $ getColor defaultWorld
            $ pixToSpace (width, height) (x, y)

start :: IO ()
start = do

    let (width, height) = (fromInteger 400, fromInteger 300)

    let amountOfSamples = 20

    putStrLn $ "Generating a " ++ show (floor width) ++ "x" ++ show (floor height) ++ " image with " ++ show amountOfSamples ++ " samples..."
    let image =
            [
                [
                let xRng = makeRng (vec x y 0)
                    yRng = makeRng (vec x y 1)
                    xs = take amountOfSamples $ randomRs (0, 1) xRng
                    ys = take amountOfSamples $ randomRs (0, 1) yRng
                    renderedSamples =
                        zipWith
                            (\x' y' -> render (width, height) (x + x', y + y'))
                            xs ys
                    avgs = map (\i -> fromIntegral $ sum (map (!!i) renderedSamples) `quot` (length renderedSamples)) [0,1,2]
                in avgs
                | x <- [0..width-1]
                ]
            | y <- [0..height-1]
            ]
        bs = generatePPM image

    file <- openFile "Out.ppm" WriteMode
    BS.hPutStr file bs
    hClose file

    putStrLn "Done."
