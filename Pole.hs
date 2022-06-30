module Module.Pole where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import System.IO (hFlush, stdout)

data LogTiang
    = LogTiang
        { tiangId :: Int
        , sto :: [Char]
        , latitude :: Double
        , longitude :: Double
        , material :: [Char]
        , distance :: Double
        , valid :: Bool
        }
    | UnknownTiang
    deriving (Show, Eq)

runProgram :: [LogTiang] -> Int -> IO ()
runProgram tiangs tiangId = do
    tiangs <- fmap parseTiang (readFile "log/tiangs.log")
    -- putStrLn "tiangId: "
    -- tiangId <- getLine
    putStrLn "\n\n\n=============== Here what you selected ==============="
    putStrLn $ "youve selected tiangID 3"
    updatedTiangs <- takeTiang tiangs 3
    parseLogTiang updatedTiangs

extractTiang :: Maybe LogTiang -> LogTiang
extractTiang (Just a) = a
extractTiang Nothing = UnknownTiang

takeTiang :: [LogTiang] -> Int -> IO [LogTiang]
takeTiang tiangList choice = do
    let tiangExist = find (\tiang -> (tiangId tiang) == choice) tiangList
        replaceTiang :: [LogTiang] -> LogTiang -> [LogTiang]
        replaceTiang [] chosenTiang = []
        replaceTiang (tiang : rest) chosenTiang
            | tiang == chosenTiang = [tiang{valid = True}] ++ replaceTiang rest chosenTiang
            | otherwise = [tiang] ++ replaceTiang rest chosenTiang
    
    let updatedLogTiangList = if (extractTiang tiangExist) == UnknownTiang 
                            then tiangList 
                            else replaceTiang tiangList (extractTiang tiangExist)

    if (extractTiang tiangExist) == UnknownTiang
        then putStrLn "Tiang not found. Please check your TiangID"
        else putStrLn "TiangID validated successfully!"
    return updatedLogTiangList



selectTiang :: [LogTiang] -> Int -> Maybe LogTiang
selectTiang [] _ = Nothing
selectTiang tiangList tiangid = find (\tiang -> (tiangId tiang) == tiangid) tiangList
    --     extractTiang :: Maybe LogTiang -> LogTiang
    --     extractTiang (Just a) = a
    --     extractTiang Nothing = UnknownTiang
    -- extractTiang tiangExist
    -- if (extractTiang tiangExist) == UnknownTiang then False else True
        -- then show "Tiang not found. Please check your TiangID"
        -- else show "\nID: " ++ show (tiangId tiang)
        --     ++ "\nArea: "
        --     ++ sto tiang
        --     ++ "\nLatitude: "
        --     ++ show (latitude tiang)
        --     ++ "\nLongitude: "
        --     ++ show (longitude tiang)
        --     ++ "\nmaterial: "
        --     ++ material tiang
        --     ++ "\nisValid: "
        --     ++ show (valid tiang)
        --     ++ "\n"
        --     ++ replicate 29 '-'
        --     ++ "\n"

-- putStrLn show(sto logTiang1++" "++show(latitude logTiang1)
parseTiang :: String -> [LogTiang]
parseTiang rawContent = map parseSingleTiang (lines rawContent)

parseSingleTiang :: String -> LogTiang
parseSingleTiang str = case words str of
    (i : s : la : lo : m) -> makeTiang i s la lo m
    _ -> UnknownTiang

makeTiang :: String -> String -> String -> String -> [String] -> LogTiang
makeTiang tiangId sto latitude longitude material =
    LogTiang
        { tiangId = read tiangId
        , sto = sto
        , latitude = read latitude
        , longitude = read longitude
        , material = unwords material
        , distance = 0.0
        , valid = False
        }

parseLogTiang :: [LogTiang] -> IO ()
parseLogTiang logTiangList = do
    let convertToLog :: [LogTiang] -> String
        convertToLog [] = ""
        convertToLog (logTiang : rest) =
            show (tiangId logTiang)
                ++ "\t"
                ++ sto logTiang
                ++ "\t"
                ++ show (latitude logTiang)
                ++ "\t"
                ++ show (longitude logTiang)
                ++ "\t"
                ++ material logTiang
                ++ "\t"
                ++ show (valid logTiang)
                ++ "\n"
                ++ convertToLog rest
    let parseLogTiang = init $ convertToLog logTiangList -- using init to remove the last \n at the end of the .log
    writeFile "log/tiang.log" parseLogTiang

type Point = (Double, Double)
type Distance = Double

coordistance :: Point -> Point -> Distance
coordistance x y =
  let (lat1,lon1) = getRadianPair x
      (lat2,lon2) = getRadianPair y
      deltaLat    = lat2 - lat1
      deltaLon    = lon2 - lon1
      a = (sin (deltaLat / 2))^(2::Int) + cos lat1 * cos lat2 * (sin (deltaLon / 2))^(2::Int)
      c = 2 * atan2 (a**0.5) ((1-a)**0.5)
  in radiusOfEarth * c

-- get latitude and longituide in Radians as Double's
getRadianPair :: Point -> (Double,Double)
getRadianPair p = (toRadians (pntLat p), toRadians (pntLon p))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

-- |radius of the earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6378700

addVector :: Vector -> Point -> Point
addVector (d,h) p =
                  p { pntLon = toDegrees lon2
                    , pntLat = toDegrees lat2
                        }

type Heading = Double

-- |Speed is hard coded as meters per second
type Speed = Double
type Vector = (Distance, Heading)

toDegrees :: Double -> Double
toDegrees = (*) (180 / pi)