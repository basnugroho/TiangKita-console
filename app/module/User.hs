module Module.User where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
-- import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)
import Control.Monad (when)

data User
    = User
        { userId :: Int
        , username :: String
        , name :: String
        , password :: String
        , state :: String
        }
    | UnknownUser
    deriving (Show, Eq)

selectUser :: [User] -> String -> Maybe User
selectUser [] _ = Nothing
selectUser users uname = find (\user -> (username user) == uname) users

parseUser :: String -> [User]
parseUser rawContent = map parseSingleUser (lines rawContent)

parseSingleUser :: String -> User
parseSingleUser str = case words str of
    (i : un : n : p : s) -> makeUser i un n p s
    _ -> UnknownUser

makeUser :: String -> String -> String -> String -> [String] -> User
makeUser userId username name password state =
    User
        { userId = read userId
        , username = username
        , name = name
        , password = password
        , state = unwords state
        }

extractUser :: Maybe User -> User
extractUser (Just a) = a
extractUser Nothing = UnknownUser

isLoggedIn :: [User] -> Maybe User
isLoggedIn users = find (\user -> (state user) == "online") users

isAuthorized :: [User] -> Bool
isAuthorized users = do
    let maybeLogged =  find (\user -> (state user) == "online") users
    case maybeLogged of
        (Just a) -> True
        Nothing -> False

changeState :: [User] -> String -> String -> IO [User]
changeState users uname state = do
    let userExist = find (\user -> (username user) == uname) users
        updateState :: [User] -> User -> [User]
        updateState [] chosenUser = []
        updateState (user : rest) chosenUser
            | user == chosenUser = [user{state = state}] ++ updateState rest chosenUser
            | otherwise = [user] ++ updateState rest chosenUser
    
    let updatdUsers = if (extractUser userExist) == UnknownUser 
                            then users 
                            else updateState users (extractUser userExist)
    return updatdUsers

changeStateToOffline :: [User] -> IO [User]
changeStateToOffline users = do
    let userExist = find (\user -> (state user) == "online") users
        updateState :: [User] -> User -> [User]
        updateState [] chosenUser = []
        updateState (user : rest) chosenUser
            | user == chosenUser = [user{state = "offline"}] ++ updateState rest chosenUser
            | otherwise = [user] ++ updateState rest chosenUser
    
    let updatdUsers = if (extractUser userExist) == UnknownUser 
                            then users 
                            else updateState users (extractUser userExist)
    return updatdUsers

parseLogUser :: [User] -> IO ()
parseLogUser users = do
    let convertToLog :: [User] -> String
        convertToLog [] = ""
        convertToLog (user : rest) =
            show (userId user)
                ++ " "
                ++ username user
                ++ " "
                ++ name user
                ++ " "
                ++ (password user)
                ++ " "
                ++ (state user)
                ++ "\n"
                ++ convertToLog rest
    let parseLogUser = init $ convertToLog users -- using init to remove the last \n at the end of the .log
    when (length parseLogUser > 0) $
        writeFile "log/users.log" parseLogUser