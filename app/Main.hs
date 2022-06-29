{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Tiang (LogTiang (UnknownTiang), addNewTiang, tiangId, sto, latitude, longitude, material, distance, valid, parseLogTiang, parseTiang)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

import Data.Aeson
import Network.Wreq
import Data.Text (Text)
import GHC.Generics

data TelkomAreaResponse = TelkomAreaResponse {
    regional :: Text,
    witel :: Text,
    name :: Text,
    description :: Text,
    status :: Text
    }
    deriving (Generic)
    
instance FromJSON TelkomAreaResponse

getTelkomArea :: String -> String -> IO ()
getTelkomArea lat lon = do
    let url = "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lon=" ++ lon ++ "&lat=" ++ lat
    rsp <- get url
    print rsp

runProgram :: [LogTiang] -> [LogMessage] -> IO ()
runProgram tiangs messages = do
    putStrLn "\n\n\n=============== TiangKita Validator Console ==============="
    putStrLn $ replicate 59 '='
    -- putStrLn $ showItem items
    putStrLn "(a) Login  (b) Telkom Area (c) Show Tiang Nearby  (d) Validate Tiang Eksisting  (e) Submit New Tiang  (f) Exit"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn $ "Login"
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "b" -> do
            putStrLn $ "Get Telkom Area (only works on Telkom Intranet)"
            putStrLn "insert latitude (e.g: -6.175232396788355):"
            lat <- getLine   
            putStrLn "insert longitude (e.g: 106.82712061061278):"
            lon <- getLine   
            getTelkomArea lat lon
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "c" -> do
            putStrLn $ showTiangNearby tiangs
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "d" -> do
            putStrLn $ "Enter Tiang ID to be validated:"
            -- Insert tiangID
            putStr "Insert Tiang ID: "
            -- hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStrLn "insert latitude (e.g: -6.175232396788355):"
            lat <- getLine   
            putStrLn "insert longitude (e.g: 106.82712061061278):"
            lon <- getLine   

            -- parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram tiangs messages
        "e" -> do
            putStrLn $ "You're about to submit New Tiang, please supply the data"
            sto <- prompt "STO (JGR, MYR, KBR): "
            putStrLn "insert latitude (e.g: -6.175232396788355):"
            lat <- getLine   
            let double_lat = read(lat)
            putStrLn "insert longitude (e.g: 106.82712061061278):"
            lon <- getLine  
            let double_lon = read(lon)
            material <- prompt "material (Steel, Concrete): "
            newTiangs <- addNewTiang tiangs sto double_lat double_lon material
            parseLogTiang newTiangs
            logMessage <- makeLogMessage (last newTiangs) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added New Tiang! Press enter to continue."
            runProgram newTiangs messages
        "f" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram tiangs messages

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
        ++ "\nisValid: "
        ++ show (valid tiang)
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showTiangNearby rest

main :: IO ()
main = do
    tiangs <- fmap parseTiang (readFile "log/tiangs.log")
    runProgram tiangs []