module Module.Password where

import Crypto.BCrypt
import Data.ByteString.Char8
import System.IO
import Control.Exception

p = Data.ByteString.Char8.pack
encryptPass :: String -> IO (Maybe ByteString)
encryptPass pass =  hashPasswordUsingPolicy slowerBcryptHashingPolicy (p pass)

verifPass :: String -> String -> Bool
verifPass actPass inputPass = validatePassword (p actPass) (p inputPass)

getPassword :: IO String
getPassword = do
  Prelude.putStr "Password: "
  hFlush stdout
  pass <- withEcho False Prelude.getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action