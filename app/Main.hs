import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)

import Module.Tiang (LogTiang (UnknownTiang))

runProgram :: IO ()
runProgram = do
    putStrLn "\n\n\n=============== TiangKita Validator Console ==============="
    putStrLn $ replicate 57 '='
    -- putStrLn $ showItem items
    putStrLn "(a) Login  (b) Get Telkom Area  (c) Validate Eksisting Tiang  (d) Found New Tiang  (e) Log Out"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn $ "Enter Token"
            empty <- prompt "Press enter to go back"
            runProgram
        "b" -> do
            putStrLn $ "Enter your location (longitude, latitude)"
            empty <- prompt "Press enter to go back"
            runProgram
        "c" -> do
            putStrLn $ "Enter your Eksisting Tiang"
            empty <- prompt "Press enter to go back"
            runProgram
        "d" -> do
            putStrLn $ "Enter your New Tiang"
            empty <- prompt "Press enter to go back"
            runProgram
        "e" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram

main :: IO ()
main = do
    runProgram