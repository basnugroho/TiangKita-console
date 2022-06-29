import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)

import Module.Tiang (LogTiang (UnknownTiang), addNewTiang, tiangId, sto, latitude, longitude, material, distance, valid, parseLogTiang, parseTiang)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)

runProgram :: [LogTiang] -> [LogMessage] -> IO ()
runProgram tiangs messages = do
    putStrLn "\n\n\n=============== TiangKita Validator Console ==============="
    putStrLn $ replicate 59 '='
    -- putStrLn $ showItem items
    putStrLn "(a) Login  (b) Show Tiang Nearby  (c) Validate Tiang Eksisting  (d) Submit New Tiang  (e) Exit"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn $ "Login"
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "b" -> do
            putStrLn $ showTiangNearby tiangs
            empty <- prompt "Press enter to go back"
            runProgram tiangs messages
        "c" -> do
            putStrLn $ "Enter Tiang ID to be validated:"
            -- Insert ItemID
            -- putStr "Insert Tiang ID: "
            -- hFlush stdout
            -- choice <- do
            --     result <- runMaybeT maybeReadInt
            --     case result of
            --         (Just a) -> return a
            --         Nothing -> return 0
            
            -- newRestockedTiang <- restockTiang tiangs choice amount
            -- parseLogItem newRestockedItems
            -- let changedItem = find (\item -> itemId item == choice) newRestockedItems
            --     extractItem :: Maybe LogItem -> LogItem
            --     extractItem (Just a) = a
            --     extractItem Nothing = UnknownItem

            -- let extractedItem = extractItem changedItem

            -- empty <- prompt "Press enter to go back"
            -- runProgram tiangs messages
            -- -- Insert Amount
            -- putStr "Please specify your latitude: "
            -- hFlush stdout
            -- amount <- do
            --     result <- runMaybeT maybeReadInt
            --     case result of
            --         (Just a) -> return a
            --         Nothing -> return 0
            putStrLn $ "ups this feature still under development :)"
            runProgram tiangs messages
        "d" -> do
            putStrLn $ "You're about to submit New Tiang, please supply the data"
            sto <- prompt "STO (JGR, MYR, KBR): "
            let latitude = 7.41949837429793
            let longitude = 112.66640638796179
            material <- prompt "material (Steel, Concrete): "
            newTiangs <- addNewTiang tiangs sto latitude longitude material
            logMessage <- makeLogMessage (last newTiangs) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added New Tiang! Press enter to continue."
            runProgram newTiangs messages
        "e" -> do
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