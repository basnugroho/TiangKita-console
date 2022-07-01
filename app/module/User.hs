module Module.User where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
-- import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

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

selectUser :: [User] -> String -> String -> Maybe User
selectUser [] _ _ = Nothing
selectUser users uname  pass = find (\user -> (username user) == uname && (password user) == pass) users

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

    if (extractUser userExist) == UnknownUser
        then putStrLn "Tiang not found. Please check your TiangID"
        else putStrLn "TiangID validated successfully!"
    return updatdUsers