{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, EmptyDataDecls, BangPatterns, TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |A basic GPS library with calculations for distance and speed along
-- with helper functions for filtering/smoothing trails.  All distances
-- are in meters and time is in seconds.  Speed is thus meters/second
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT, maybeReadDouble)
import Module.Tiang (LogTiang (UnknownTiang), addNewTiang, tiangId, sto, latitude, longitude, 
                        material, distance, valid, parseLogTiang, parseTiang, selectTiang, extractTiang)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import Module.User
import System.IO (hFlush, stdout)
import HaskellSay (haskellSay)

import Data.Aeson
import Network.Wreq
import Data.Text (Text)
import GHC.Generics

import Geo.Computations
import Data.Time
import Data.List
import Data.Ord
import Data.Fixed
import Control.Applicative
import Control.Monad
import Text.Read

import Crypto.BCrypt

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
    rsp <- Network.Wreq.get url
    print rsp

runProgram :: [LogTiang] -> [LogMessage] -> IO ()
runProgram tiangs messages = do
    haskellSay "Hello, Welcome to TiangKita Validator Console!"
    putStrLn "\n\n\n=============== TiangKita Validator Console ==============="
    putStrLn $ replicate 59 '='
    -- putStrLn $ showItem items
    putStrLn "(a) Login  (b) Telkom Area (c) Show Tiang Nearby  (d) Validate Tiang Eksisting  (e) Submit New Tiang  (f) Exit"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn "\n\n\n=============== Login ==============="
            putStrLn $ "username:"
            username <- getLine
            putStrLn $ "password:"
            password <- getLine
            -- handle <- openFile "log/users.log" ReadMode
            -- users <- fmap parseUser $ (hGetContents handle)     
            -- hClose handle        
            users <- fmap parseUser (readFile "log/users.log")
            let maybeUser = selectUser users username password
            case maybeUser of
                (Just a) -> do
                    usersOnline <- changeState users (Module.User.username a) "online"
                    parseLogUser usersOnline
                    putStrLn $ "Login Success! Welcome " ++ (Module.User.name a) 
                Nothing -> putStrLn "Ups! Wrong credential"
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
            putStrLn $ "Enter Tiang ID: "
            -- hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            let maybeTiang = selectTiang tiangs choice

            case maybeTiang of
                Nothing -> do
                    putStrLn "Tiang not found. Please check your TiangID again!"
                    makeLogMessage UnknownTiang "ERR"
                (Just a) -> do
                    let tiangExisting = extractTiang maybeTiang
                    putStrLn $ "choosen tiang -> ID: " ++ show(tiangId tiangExisting)
                                ++ ", STO: " ++ show(sto tiangExisting)
                                ++ ", lat: " ++ show(latitude tiangExisting)
                                ++ ", lon: " ++ show(longitude tiangExisting)
                                ++ ", material: " ++ show(material tiangExisting)

                    -- check lat lon
                    putStrLn "insert latitude (e.g: -6.175232396788355):"
                    lat <- getLine -- need failsafe
                    safeLat <- do
                        let safeLat = maybeReadDouble lat
                        case safeLat of
                            (Just a) -> return a
                            (Nothing) -> do
                                putStrLn "wrong latitude format input!"
                                makeLogMessage tiangExisting "INVALID"
                                return 0.0
                    -- case safeLat of
                    --     0.0 -> do runProgram tiangs messages
                    
                    putStrLn "insert longitude (e.g: 106.82712061061278):"
                    lon <- getLine -- need failsafe
                    safeLon <- do
                        let safeLon = maybeReadDouble lon
                        case safeLon of
                            (Just a) -> return a
                            (Nothing) -> do
                                makeLogMessage tiangExisting "INVALID"
                                putStrLn "wrong longitude format input!"
                                return 0.0
                    -- case safeLon of
                    --     0.0 -> do runProgram tiangs messages
                    
                    let inputPoin = Point (safeLat) (safeLon) Nothing Nothing
                    let tiangExistingPoin = Point (latitude tiangExisting) (longitude tiangExisting) Nothing Nothing
                    let coordistance =  Geo.Computations.distance inputPoin tiangExistingPoin

                    putStrLn $ "calculated distance: " ++ show(coordistance) ++ " meter"
                    makeLogMessage tiangExisting "VALID"
            emptyPrompt <- prompt "\nPress enter to continue."
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
            users <- fmap parseUser (readFile "log/users.log")
            exitUser <- changeStateToOffline users
            parseLogUser exitUser
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

-- Lat Long

main :: IO ()
main = do
    tiangs <- fmap parseTiang (readFile "log/tiangs.log")
    runProgram tiangs []




