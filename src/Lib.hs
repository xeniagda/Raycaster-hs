module Lib where

import System.IO
import System.Random
import System.Environment
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
import Args


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

render :: (Integral a) => StdGen -> Props -> World Float -> (Float, Float) -> (Float, Float) -> [a]
render rng props world (width, height) (x, y) =
    map
        (floor . realToFrac)
        $ getColor rng props world
            $ pixToSpace (width, height) (x, y)

start :: IO ()
start = do
    props <- fmap (parse defaultProps) getArgs

    let world = defaultWorld props
        (width, height) = getSize props

    samples <- newTVarIO [] :: (Num a) => IO (TVar [[[[a]]]]) -- Samples x width x height x colors

    putStrLn $ "Generating a " ++ show width ++ "x" ++ show height ++ " image with " ++ show (getSamples props) ++ " samples..."
    if isVerbose props
        then putStrLn $ "Settings: " ++ show props
        else return ()

    sequence_ [forkIO $ oneSample samples props world i | i <- [0..getSamples props-1]]

    start <- getCurrentTime

    let wait lastDone = do
            amount <- fmap length $ readTVarIO samples
            if fromIntegral amount == fromIntegral (getSamples props) -- For some reason amountOfSamples is an Integer
                then return ()
                else do
                    if amount /= lastDone
                        then putStrLn $ "Done " ++ show amount ++ " / " ++ show (getSamples props) ++ " samples."
                        else return ()
                    threadDelay (oneSecond `quot` 100)
                    wait amount
    wait (-1)

    end <- getCurrentTime

    let diff = diffUTCTime end start

    putStrLn $ "Generating samples took " ++ show diff ++ " which is " ++ show (diff / fromIntegral (getSamples props)) ++ " per sample."

    putStrLn "Combining samples..."

    allSamples <- readTVarIO samples

    let image =
            [
                [
                let pixels = map (\sample -> sample !! y !! x) allSamples
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

oneSample :: (Integral a) => TVar [[[[Integer]]]] -> Props -> World Float -> a -> IO ()
oneSample samples props world threadNr = do
    let generators current =
            let (a, b) = split current
            in a : generators b
        (width, height) = getSize props

    tSeed <- fmap (fst . random . (!! fromIntegral threadNr) . generators) getStdGen :: IO Int

    image <- sequence
            [ sequence
                [
                    do
                        seed <- randomIO :: IO Int

                        let gen = mkStdGen $ seed + tSeed
                            (r1, r2) = split gen
                            [x', y'] = take 2 $ randomRs (0, 1) r1 :: [Float] -- Go fuck yourself
                        return $ map (toInteger . fromIntegral)
                            $ render
                                r2
                                props
                                world
                                ( fromIntegral width
                                , fromIntegral height
                                )
                                ( x' + fromIntegral x
                                , y' + fromIntegral y
                                )
                | x <- [0..width-1]
                ]
            | y <- [0..height-1]
            ]
    atomically $
        image `deepseq`
            modifyTVar samples (image:)

