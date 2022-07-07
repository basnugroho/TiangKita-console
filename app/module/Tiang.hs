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
        , radiuscheck :: Double
        , isvalid :: Bool
        , designator :: [Char]
        }
    | UnknownTiang
    deriving (Show, Eq)

addNewTiang :: [LogTiang] -> [Char] -> Double -> Double -> [Char] -> IO [LogTiang]
addNewTiang oldLogTiangList sto latitude longitude designator = do
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
                , radiuscheck = 9999
                , isvalid = False
                , designator = designator
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
                ++ show (radiuscheck logTiang)
                ++ "\t"
                ++ show (isvalid logTiang)
                ++ "\t"
                ++ designator logTiang
                ++ "\n"
                ++ convertToLog rest
    let parseLogTiang = init $ convertToLog logTiangList -- using init to remove the last \n at the end of the .log
    when (length parseLogTiang > 0) $
        writeFile "log/tiangs.log" parseLogTiang

parseTiangValidated :: [LogTiang] -> IO ()
parseTiangValidated logTiangList = do
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
                ++ show (radiuscheck logTiang)
                ++ "\t"
                ++ show (True)
                ++ "\t"
                ++ designator logTiang
                ++ "\n"
                ++ convertToLog rest
    let parseTiangValidated = init $ convertToLog logTiangList -- using init to remove the last \n at the end of the .log
    when (length parseTiangValidated > 0) $
        writeFile "log/tiangs_validated.log" parseTiangValidated

------- everything start here
parseTiang :: String -> [LogTiang]
parseTiang rawContent = map parseSingleTiang (lines rawContent)

parseSingleTiang :: String -> LogTiang
parseSingleTiang str = case words str of
    (i : s : la : lo : r : v : d) -> makeTiang i s la lo r v d
    _ -> UnknownTiang

makeTiang :: String -> String -> String -> String -> String -> String -> [String] -> LogTiang
makeTiang tiangId sto latitude longitude radiuscheck isvalid desginator =
    LogTiang
        { tiangId = read tiangId
        , sto = sto
        , latitude = read latitude
        , longitude = read longitude
        , radiuscheck = read radiuscheck
        , isvalid = read isvalid
        , designator = unwords desginator
        }
--------

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
            | tiang == chosenTiang = [tiang{isvalid = True}] ++ replaceTiang rest chosenTiang
            | otherwise = [tiang] ++ replaceTiang rest chosenTiang
    
    let updatedLogTiangList = if (extractTiang tiangExist) == UnknownTiang 
            then tiangList 
            else replaceTiang tiangList (extractTiang tiangExist)
    
    if (extractTiang tiangExist) == UnknownTiang
        then putStrLn "Tiang not found. Please check your TiangID"
        else
            putStrLn "Successfully update Tiang"
            
    return updatedLogTiangList


findTiangNearby :: [LogTiang] -> Point -> Double -> [LogTiang]
findTiangNearby [] _ _ = []
findTiangNearby (tiang : rest) inputPoin dist = 
    let tiangPoint = (Point (latitude tiang) (longitude tiang) Nothing Nothing) in
    if (distance (inputPoin) (tiangPoint) < dist)
        then tiang{radiuscheck = distance (inputPoin) (tiangPoint)} : findTiangNearby (rest) inputPoin dist 
        else findTiangNearby rest inputPoin dist

showTiangNearby :: [LogTiang] -> String
showTiangNearby [] = replicate 58 '='
showTiangNearby (tiang : rest) = 
    "\nID: " ++ show (tiangId tiang)
        ++ "\nSTO: "
        ++ sto tiang
        ++ "\nLatitude: "
        ++ show (latitude tiang)
        ++ "\nLongitude: "
        ++ show (longitude tiang)
        ++ "\ncalculated distance: "
        ++ show (radiuscheck tiang) ++ " meter"
        ++ "\nisValid: "
        ++ show (isvalid tiang)
        ++ "\nDesignator: "
        ++ designator tiang
        ++ "\n\n"
        ++ showTiangNearby rest

generateDesignator :: Int -> String
generateDesignator option
    | option == 1 = "PU-S7.0-140"
    | option == 2 = "PU-S9.0-141"
    | option == 3 = "PU-C7.0-140"
    | option == 4 = "PU-C9.0-140"
    | otherwise = "PU-S12.0-140"