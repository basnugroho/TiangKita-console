module Module.Tiang where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)
import Geo.Computations
import Control.Monad (when)

data LogTiang
    = LogTiang
        { tiangId :: Int
        , sto :: [Char]
        , latitude :: Double
        , longitude :: Double
        , material :: [Char]
        , radiuscheck :: Double
        , valid :: Bool
        }
    | UnknownTiang
    deriving (Show, Eq)

addNewTiang :: [LogTiang] -> [Char] -> Double -> Double -> [Char] -> IO [LogTiang]
addNewTiang oldLogTiangList sto latitude longitude material = do
    let lastId =
            if null oldLogTiangList
                then 0
                else tiangId $ last oldLogTiangList
        newId = lastId + 1
        newLogTiang =
            LogTiang
                { tiangId = newId
                , sto = sto
                , latitude = latitude
                , longitude = longitude
                , material = material
                , radiuscheck = 0
                , valid = False
                }
    let newLogTiangList = oldLogTiangList ++ [newLogTiang]
    return newLogTiangList

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
                ++ "\n"
                ++ convertToLog rest
    let parseLogTiang = init $ convertToLog logTiangList -- using init to remove the last \n at the end of the .log
    when (length parseLogTiang > 0) $
        writeFile "log/tiangs.log" parseLogTiang

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
        , radiuscheck = 0
        , valid = False
        }

selectTiang :: [LogTiang] -> Int -> Maybe LogTiang
selectTiang [] _ = Nothing
selectTiang tiangList tiangid = find (\tiang -> (tiangId tiang) == tiangid) tiangList

extractTiang :: Maybe LogTiang -> LogTiang
extractTiang (Just a) = a
extractTiang Nothing = UnknownTiang

updateTiang :: [LogTiang] -> Int -> IO [LogTiang]
updateTiang tiangList choice = do
    let tiangExist = find (\tiang -> (tiangId tiang) == choice) tiangList
        replaceTiang :: [LogTiang] -> LogTiang -> [LogTiang]
        replaceTiang [] chosenTiang = []
        replaceTiang (tiang : rest) chosenTiang
            | tiang == chosenTiang = [tiang{valid = True}] ++ replaceTiang rest chosenTiang
            | otherwise = [tiang] ++ replaceTiang rest chosenTiang
    
    let updatedLogTiangList = if (extractTiang tiangExist) == UnknownTiang 
                            then tiangList 
                            else replaceTiang tiangList (extractTiang tiangExist)
    putStrLn "updated"
    return updatedLogTiangList


findTiangNearby :: [LogTiang] -> Point -> Double -> [LogTiang]
findTiangNearby [] _ _ = []
findTiangNearby (tiang : rest) inputPoin dist = 
    let tiangPoint = (Point (latitude tiang) (longitude tiang) Nothing Nothing) in
    if (distance (inputPoin) (tiangPoint) < dist)
        then tiang{radiuscheck = distance (inputPoin) (tiangPoint)} : findTiangNearby (rest) inputPoin dist 
        else findTiangNearby rest inputPoin dist

findTiangEven :: [LogTiang] -> [LogTiang]
findTiangEven [] = []
findTiangEven (tiang : rest)  = 
    if (mod (tiangId tiang) 2 == 0)
        then tiang : findTiangEven (rest)
        else findTiangEven rest

showTiangNearby :: [LogTiang] -> String
showTiangNearby [] = replicate 58 '='
showTiangNearby (tiang : rest) = 
    "ID: " ++ show (tiangId tiang)
        ++ "\nArea: "
        ++ sto tiang
        ++ "\nLatitude: "
        ++ show (latitude tiang)
        ++ "\nLongitude: "
        ++ show (longitude tiang)
        ++ "\nmaterial: "
        ++ material tiang
        ++ "\ncalculated distance: "
        ++ show (radiuscheck tiang) ++ " meter"
        ++ "\nisValid: "
        ++ show (valid tiang)
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showTiangNearby rest