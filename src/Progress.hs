module Progress where


data Progress a  -- Done / Total
    = Sampling a a
    | Combining a a
    | Done Bool
    deriving (Show, Eq)

getProgress :: (Integral a, Fractional b) => Progress a -> b
getProgress (Sampling a b) = fromIntegral a / ((fromIntegral b) * 2)
getProgress (Combining a b) = fromIntegral a / ((fromIntegral b) * 2) + 0.5
getProgress (Done _) = 1

inc :: (Num a) => Progress a -> Progress a
inc (Sampling a b) = Sampling (a + 1) b
inc (Combining a b) = Combining (a + 1) b
inc (Done _) = Done True

progressStage :: Progress a -> String
progressStage (Sampling _ _) = "Sampling"
progressStage (Combining _ _) = "Combining"
progressStage (Done _) = "Done"
