module Module.Password where

import Crypto.BCrypt
import Data.ByteString.Char8

p = Data.ByteString.Char8.pack
encryptPass :: String -> IO (Maybe ByteString)
encryptPass pass =  hashPasswordUsingPolicy slowerBcryptHashingPolicy (p pass)

-- extractPass :: IO (Maybe ByteString) -> String
-- extractPass (Just a) = a
-- extractPass Nothing = ""

verifPass :: String -> String -> Bool
verifPass inputPass actPass = validatePassword (p inputPass) (p actPass)