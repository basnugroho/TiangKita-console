{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Module.HTTP where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Char (toLower)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import RIO
import Prelude (print, putStrLn, read)

data TelkomArea = TelkomArea 
    {
        regional :: !Text,
        witel :: !Text,
        name :: !Text,
        description :: !Text,
        status :: !Text
    }  
    | UnknownTelkomArea
    deriving (Eq, Show, Generic)

instance FromJSON TelkomArea

data User = User
  { userId :: !Int,
    userName :: !Text,
    userUsername :: !Text,
    userEmail :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = JSON.genericParseJSON $ jsonOptions "user"

getTelkomAreaContent :: String -> String -> IO LByteString
getTelkomAreaContent lat lon = do
  manager <- newTlsManager
  request <- HTTP.parseRequest $ "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lat" ++ lat ++ "&lon=" ++ lon
  HTTP.responseBody <$> HTTP.httpLbs request manager

getPlaceContent :: String -> String -> IO LByteString
getPlaceContent lat lon = do
  manager <- newTlsManager
  request <- HTTP.parseRequest $ "https://maps.googleapis.com/maps/api/geocode/json?latlng=" ++ lat ++ ","++ lon ++ "&key=AIzaSyDvqKPOVZlBgYF2t_5odBPOuzzxvBtJL8I"
  HTTP.responseBody <$> HTTP.httpLbs request manager

getUsersContent :: IO LByteString
getUsersContent = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  HTTP.responseBody <$> HTTP.httpLbs request manager

getUsers :: IO (Either String [User])
getUsers = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  (HTTP.responseBody >>> JSON.eitherDecode) <$> HTTP.httpLbs request manager

runMain :: IO ()
runMain = do
  maybeUsers <- getUsers
  case maybeUsers of
    Right users -> do
      -- `map @User name users` works to disambiguate this if we have duplicate record fields
      let userNames = map userName users
      print userNames
    Left e -> error e
  putStrLn "Hello, World!"

jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
      lowercaseFirstCharacter (c : rest) = toLower c : rest
      lowercaseFirstCharacter [] = []
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter}