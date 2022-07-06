{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, EmptyDataDecls, BangPatterns, TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT, maybeReadDouble)
import Module.Tiang (LogTiang, LogTiang (UnknownTiang), addNewTiang, tiangId, sto, latitude, longitude, 
                        material, valid, parseLogTiang, parseTiang, selectTiang, extractTiang, findTiangNearby, 
                        showTiangNearby, updateTiang, makeTiang)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import Module.User
import Module.Request (TelkomArea, Place, parseTelkomArea, regional, name, witel, description, 
                        formatted_address, showTelkomArea, showPlace, plus_code, request)
import Module.Password
-- import Module.HTTP
import System.IO
import HaskellSay (haskellSay)
import Geo.Computations
import Data.Aeson
import Data.Fixed
import Data.List
import Data.Ord
import Data.Time
import Network.HTTP.Conduit (simpleHttp)
import Text.Read
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as T
import GHC.Generics

import System.Environment  
import System.IO  
import System.IO.Error 

import Network.HTTP.Simple
import Control.Concurrent     (forkIO, newEmptyMVar, putMVar, takeMVar,
                               threadDelay)
import Control.Exception.Safe
import Data.Typeable          (Typeable, cast)

urlByte :: String -> IO B.ByteString
urlByte url = simpleHttp url

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
            putStrLn $ "Note: Get Telkom Area Data with internet will cause EXCEPTION"
            -- repetitive: need to refactor
            putStrLn "\ninsert latitude (e.g: -6.175232396788355):"
            lat <- getLine -- need failsafe
            safeLat <- do
                let safeLat = maybeReadDouble lat
                case safeLat of
                    (Just a) -> return a
                    (Nothing) -> return 0.0
            
            putStrLn "\ninsert longitude (e.g: 106.82712061061278):"
            lon <- getLine 
            safeLon <- do
                let safeLon = maybeReadDouble lon
                case safeLon of
                    (Just a) -> return a
                    (Nothing) -> return 0.0
            -- try
            if safeLat /= 0.0 && safeLon /= 0.0 then do
                putStrLn "Getting Telkom Area..."
                eres <- tryAny $ httpLbs "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lat=-6.175232396788355&lon=106.82712061061278"
                case eres of
                    Left e -> print e
                    Right lbs -> do
                            -- telkom area
                            let byteUrl = urlByte $ "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lat=" ++ lat ++ "&lon=" ++ lon
                            d <- (eitherDecode <$> byteUrl) :: IO (Either String [TelkomArea])
                            case d of
                                Left err -> putStrLn err
                                Right ps -> do
                                    parseTelkomArea ps
                                    putStrLn "Nice! your IP is allowed"
                                    putStrLn $ showTelkomArea (head ps)
                empty <- prompt "Press enter to get public location info"
                let byteUrl = urlByte $ "https://maps.googleapis.com/maps/api/geocode/json?latlng=" ++ lat ++ ","++ lon ++ "&key=AIzaSyDvqKPOVZlBgYF2t_5odBPOuzzxvBtJL8I"
                d <- (eitherDecode <$> byteUrl) :: IO (Either String Place)
                case d of
                    Left err -> putStrLn err
                    Right ps -> do
                        putStrLn $ "Nearby: " ++ showPlace (plus_code ps)
                else do putStrLn "Ups! Wrong input detected. Please try again!"
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "c" -> do
            -- repetitive: need to refactor
            putStrLn "enter latitude (e.g: -6.175232396788355):"
            lat <- getLine -- need failsafe
            safeLat <- do
                let safeLat = maybeReadDouble lat
                case safeLat of
                    (Just a) -> return a
                    (Nothing) -> do
                        putStrLn "wrong latitude format input!"
                        return 0.0
            
            putStrLn "enter longitude (e.g: 106.82712061061278):"
            lon <- getLine
            safeLon <- do
                let safeLon = maybeReadDouble lon
                case safeLon of
                    (Just a) -> return a
                    (Nothing) -> do
                        putStrLn "wrong longitude format input!"
                        return 0.0

            putStrLn "enter radius (in meter): "
            rad <- getLine
            safeRadius <- do
                let safeRadius = maybeReadDouble rad
                case safeRadius of
                    (Just a) -> return a
                    (Nothing) -> do
                        putStrLn "wrong radius format input!"
                        return 0.0

            if safeLat /= 0 && safeLon /= 0.0 && safeRadius /= 0.0 then do
                let inputPoin = Point (safeLat) (safeLon) Nothing Nothing
                let tiangsNearby = findTiangNearby tiangs inputPoin safeRadius
                putStrLn $ "Tiang(s) in radius: " ++ show safeRadius ++ " meter"
                putStrLn $ showTiangNearby tiangsNearby
                putStrLn $ "there are " ++ show (length tiangsNearby) ++ " Tiang(s) in radius " ++ show (safeRadius) ++ " meter"
            else do putStrLn "Ups! Wrong input detected. Please try again!"
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "d" -> do
            putStrLn $ "Enter Tiang ID: "

            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            let maybeTiang = selectTiang tiangs choice

            case maybeTiang of
                Nothing -> do
                    putStrLn "Tiang not found. Please check your TiangID again!"
                (Just a) -> do
                    let tiangExisting = extractTiang maybeTiang
                    putStrLn $ "\nchoosen tiang -> ID: " ++ show(tiangId tiangExisting)
                                ++ ", STO: " ++ show(sto tiangExisting)
                                ++ ", lat: " ++ show(latitude tiangExisting)
                                ++ ", lon: " ++ show(longitude tiangExisting)
                                ++ ", material: " ++ show(material tiangExisting)

                    -- repetitive: need to refactor
                    putStrLn "\nenter latitude:"
                    lat <- getLine -- need failsafe
                    safeLat <- do
                        let safeLat = maybeReadDouble lat
                        case safeLat of
                            (Just a) -> return a
                            (Nothing) -> return 0.0
                    
                    putStrLn "\ninsert longitude:"
                    lon <- getLine -- need failsafe
                    safeLon <- do
                        let safeLon = maybeReadDouble lon
                        case safeLon of
                            (Just a) -> return a
                            (Nothing) -> return 0.0
                    
                    let inputPoin = Point (safeLat) (safeLon) Nothing Nothing
                    let tiangExistingPoin = Point (latitude tiangExisting) (longitude tiangExisting) Nothing Nothing
                    let coordistance =  distance inputPoin tiangExistingPoin

                    if safeLat /= 0 && safeLon /= 0.0 then do
                        putStrLn $ "calculated distance: " ++ show(coordistance) ++ " (in meter)"
                        if coordistance <= 20.0 
                            then do
                                -- let calonTiang = makeTiang $ show (choice) (sto tiangExisting) lat lon []
                                updatedTiangs <- updateTiang tiangs choice
                                parseLogTiang updatedTiangs -- write to file

                                let updatedTiangMess = extractTiang $ selectTiang tiangs choice
                                logMessage <- makeLogMessage updatedTiangMess "VALID"
                                parseLogMessage logMessage
                                putStrLn $ "Tiang ID: " ++ show (choice) ++ " is VALIDATED"
                            else do
                                let updatedTiangMess = extractTiang $ selectTiang tiangs choice
                                logMessage <- makeLogMessage updatedTiangMess "INVALID"
                                parseLogMessage logMessage
                                putStrLn $ "Tiang ID: " ++ show (choice) ++ " VALIDATION NOT MATCH"
                    else do putStrLn "Ups! Wrong input detected. Please try again!"
            emptyPrompt <- prompt "\nPress enter to continue."
            runProgram tiangs messages
        "e" -> do
            putStrLn $ "You're about to enter NEW Tiang:"
            sto <- prompt "e.g: STO (JGR, MYR, KBR): "

            -- repetitive: need to refactor
            putStrLn "\nenter latitude:"
            lat <- getLine
            safeLat <- do
                let safeLat = maybeReadDouble lat
                case safeLat of
                    (Just a) -> return a
                    (Nothing) -> return 0.0
            
            putStrLn "\ninsert longitude:"
            lon <- getLine
            safeLon <- do
                let safeLon = maybeReadDouble lon
                case safeLon of
                    (Just a) -> return a
                    (Nothing) -> return 0.0

            material <- prompt "material (Steel, Concrete): "
            newTiangs <- addNewTiang tiangs sto safeLat safeLon material
            parseLogTiang newTiangs
            logMessage <- makeLogMessage (last newTiangs) "NEW"
            parseLogMessage logMessage
            putStrLn "Successfully added New Tiang!"
            empty <- prompt "Press enter to go back"
            runProgram newTiangs messages
        "f" -> do
            putStrLn "Exiting program..."
            users <- fmap parseUser (readFile "log/users.log")
            let loggedinUser = isLoggedIn users
            case loggedinUser of
                (Just a) -> do
                    putStrLn $ "Goodbye! " ++ (Module.User.name a) 
                Nothing -> putStrLn "Goodbye!"
            exitUser <- changeStateToOffline users
            parseLogUser exitUser
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram tiangs messages

main :: IO ()
main = do
    tiangs <- parseTiang <$> (readFile "log/tiangs.log")
    runProgram tiangs []