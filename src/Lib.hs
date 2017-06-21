module Lib where

import System.IO
import System.Random
import Data.Time.Clock
import Data.Char

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.DeepSeq

import qualified Data.ByteString as BS

import Raycaster

import Vec3
import Base


(width, height) = (fromInteger 600, fromInteger 400)

amountOfSamples = 40

oneSecond = 1000 * 1000

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


    samples <- newTVarIO [] :: (Num a) => IO (TVar [[[[a]]]]) -- Samples x width x height x colors

    putStrLn $ "Generating a " ++ show (floor width) ++ "x" ++ show (floor height) ++ " image with " ++ show amountOfSamples ++ " samples..."
    
    sequence_ [forkIO $ oneSample samples i | i <- [0..amountOfSamples-1]]

    start <- getCurrentTime

    let wait lastDone = do
            amount <- fmap length $ readTVarIO samples
            if fromIntegral amount == fromIntegral amountOfSamples -- For some reason amountOfSamples is an Integer
                then return ()
                else do
                    if amount /= lastDone
                        then putStrLn $ "Done " ++ show amount ++ " / " ++ show amountOfSamples ++ " samples."
                        else return ()
                    threadDelay (oneSecond `quot` 100)
                    wait amount
    wait (-1)

    end <- getCurrentTime

    let diff = diffUTCTime end start

    putStrLn $ "Generating samples took " ++ show diff ++ " which is " ++ show (diff / fromIntegral amountOfSamples) ++ " per sample."

    putStrLn "Combining samples..."

    allSamples <- readTVarIO samples

    let image =
            [
                [
                let pixels = map (\sample -> sample !! (floor y) !! (floor x)) allSamples
                    cols = [ (sum $ map (!! i) pixels) `quot` (fromIntegral $ length pixels) | i <- [0..2]]
                in cols
                | x <- [0..width-1]
                ]
            | y <- [0..height-1]
            ]

    image `deepseq` putStrLn "Saving..."

    let bs = generatePPM image

    file <- openFile "Out.ppm" WriteMode
    BS.hPutStr file bs
    hClose file
    
    putStrLn "All Done!"

oneSample :: TVar [[[[Integer]]]] -> Integer -> IO ()
oneSample samples threadNr = 
    atomically (
            let image =
                    [
                        [
                        let rng = makeRng (vec x y $ fromIntegral threadNr)
                            (x', rng') = randomR (0, 1) rng
                            (y', _) = randomR (0, 1) rng'
                        in map (toInteger . fromIntegral) $ render (width, height) (x + x', y + y')
                        | x <- [0..width-1]
                        ]
                    | y <- [0..height-1]
                    ]
            in image `deepseq` modifyTVar samples (image:)
        )
