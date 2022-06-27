module Module.Message where

import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Module.Tiang

data LogMessage
    = LogMessage
        { tiangID :: Int
        , area :: [Char]
        , lat :: Double
        , long :: Double
        , status :: Status
        , timestamp :: Int
        }
    | Unknown
    deriving (Show)

data Status = IN | OUT | NEW | ERR deriving (Show, Read)

secondSinceEpoch :: UTCTime -> Int
secondSinceEpoch =
    floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

makeLogMessage :: LogTiang -> String -> IO LogMessage
makeLogMessage tiang status = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
        message =
            if tiang == UnknownTiang
                then
                    LogMessage
                        { tiangID = 0
                        , area = ""
                        , lat = 0
                        , long = 0
                        , status = ERR
                        , timestamp = currentTime
                        }
                else
                    LogMessage
                        { tiangID = tiangId tiang
                        , area = sto tiang
                        , lat = latitude tiang
                        , long = longitude tiang
                        , status = read status :: Status
                        , timestamp = currentTime
                        }
    return message

parseLogMessage :: LogMessage -> IO ()
parseLogMessage message = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
    let parsedLogMessage =
            "TiangID: "
                ++ show (tiangID message)
                ++ " | Status: "
                ++ show (status message)
                ++ " | STO: "
                ++ show (area message)
                ++ " | latitude: "
                ++ show (lat message)
                ++ " | longitude: "
                ++ show (long message)
                ++ " | Timestamp: "
                ++ show (currentTime)
                ++ "\n"
    appendFile "log/messages.log" parsedLogMessage
