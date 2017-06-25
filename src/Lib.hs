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

import Progress

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
        maxProgress = fromIntegral (height + 100) * fromIntegral (getSamples props)

    samples <- newTVarIO [] :: (Num a) => IO (TVar [[[[a]]]]) -- Samples x width x height x colors
    progress <- newTVarIO (Sampling 0 $ fromIntegral $ (height + 100) * getSamples props)


    putStrLn $ "Generating " ++ getOutput props ++ " " ++ show width ++ "x" ++ show height ++ " with " ++ show (getSamples props) ++ " samples..."
    if isVerbose props
        then putStrLn $ "Settings: " ++ show props
        else return ()


    start <- getCurrentTime

    sequence_ [forkIO $ oneSample samples progress props world i | i <- [0..getSamples props-1]]

    forkIO $ showProgress progress

    let waitForSamples = do
            threadDelay (oneSecond `quot` 100)
            prog <- readTVarIO progress
            case prog of
                Sampling a b ->
                    if a == b
                        then return ()
                        else waitForSamples
                _ -> waitForSamples

    waitForSamples

    atomically $ writeTVar progress (Combining 0 height)


    allSamples <- readTVarIO samples
    image <- sequence
            [ do
                let row =
                        [
                        let pixels = map (\sample -> sample !! (fromIntegral y) !! (fromIntegral x)) allSamples
                            cols = [ (sum $ map (!! i) pixels) `quot` (fromIntegral $ length pixels) | i <- [0..2]]
                        in cols
                        | x <- [0..width-1]
                        ]
                row `deepseq` return ()
                atomically $ modifyTVar progress inc
                return row
            | y <- [0..height-1]
            ]

    image `deepseq` return ()

    let bs = generatePPM image

    file <- openFile (getOutput props) WriteMode
    BS.hPutStr file bs
    hClose file

    atomically $ writeTVar progress $ Done False

    let waitForAllDone = do
            threadDelay (oneSecond `quot` 100)
            prog <- readTVarIO progress
            case prog of
                Done True -> return ()
                _ -> waitForSamples

    waitForAllDone


oneSample :: (Integral a, Num b) => TVar [[[[Integer]]]] -> TVar (Progress b) -> Props -> World Float -> a -> IO ()
oneSample samples progress props world threadNr = do
    let generators current =
            let (a, b) = split current
            in a : generators b
        (width, height) = getSize props

    tSeed <- fmap (fst . random . (!! fromIntegral threadNr) . generators) getStdGen :: IO Int

    image <- sequence
            [ do
                row <- sequence
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
                atomically $ modifyTVar progress inc
                return row
            | y <- [0..height-1]
            ]

    atomically $
        image `deepseq`
            modifyTVar samples (image:)

    atomically $ modifyTVar progress (\(Sampling a b) -> Sampling (100+a) b)


{-
 - [=====     ] 45%, 1000/2200
 -}
-- makeProgresBar :: (Fractional a) => a -> Int -> String
makeProgresBar progress width =
    let prog = getProgress progress
        bar = replicate ((fromIntegral $ floor $ width * prog) - 1) '=' ++ ">"
        space = replicate (fromIntegral $ (ceiling $ width * (1 - prog))) ' '
    in "[" ++ bar ++ space ++ "] " ++ show (floor $ prog * 100) ++ "%  " ++ progressStage progress ++ "  [" ++ show progress ++ "]"


showProgress progress = do
    let wait lastProgress = do
            prog <- readTVarIO progress

            if prog == Done False
                then do
                    putStrLn $ "\x1B[1A\x1B[0K" ++ makeProgresBar prog 40
                else do
                    if prog /= lastProgress
                        then putStrLn $ "\x1B[1A\x1B[0K" ++ makeProgresBar prog 40
                        else return ()
                    threadDelay (oneSecond `quot` 100)
                    wait prog

    putStrLn "\n"
    wait (Sampling 0 1)

    prog <- readTVarIO progress
    putStrLn $ "\x1B[1A\x1B[0KDone!"

    atomically $ writeTVar progress $ Done True
