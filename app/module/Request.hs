{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Module.Request where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

data Place = Place {
        plus_code :: !PlaceCode
        -- results :: [AdressComponents]
    }
    deriving (Show, Generic)
instance FromJSON Place

data PlaceCode = PlaceCode {
        compound_code :: !Text
        -- global_code :: !Text
    }
    deriving (Show, Generic)
instance FromJSON PlaceCode

data AdressComponents = AdressComponents {
        formatted_address :: !Text,
        place_id :: !Text,
        types :: [Text]
    }
    deriving (Show, Generic)
instance FromJSON AdressComponents

data TelkomArea 
    = TelkomArea 
        {
            regional :: !Text,
            witel :: !Text,
            name :: !Text,
            description :: !Text
        } 
        deriving (Eq, Show, Generic)

instance FromJSON TelkomArea

-- getJSON :: String -> IO B.ByteString
-- getJSON url = simpleHttp url

-- getTelkomArea :: String -> String -> Either String [TelkomArea]
-- getTelkomArea lat lon = do
--     let url = simpleHttp $ "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lat" ++ lat ++ "&lon=" ++ lon
--     eitherDecode <$> url :: Either String [TelkomArea]
--     case d of
--         Left err -> putStrLn err
--         Right ps -> print ps

-- getGooglePlace :: String -> String -> IO (Either String [Place])
-- getGooglePlace lat lon = do
--     let url = simpleHttp $ "https://maps.googleapis.com/maps/api/geocode/json?latlng=" ++ lat ++ ","++ lon ++ "&key=AIzaSyDvqKPOVZlBgYF2t_5odBPOuzzxvBtJL8I"
--     eitherDecode <$> url :: IO (Either String [Place])

-- tlkmAreaRequestBuild :: BC.ByteString -> BC.ByteString -> Request
-- tlkmAreaRequestBuild host method = setRequestMethod method
--                                   $ setRequestHost host
--                                   $ setRequestSecure True
--                                   $ defaultRequest

-- tlkmAreaRequest :: String -> Request
-- tlkmAreaRequest url = tlkmAreaRequestBuild url "GET"


-- getTelkomArea :: String -> String -> IO LByteString
-- getTelkomArea lat lon = do
--     -- let url = "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lat=" ++ lat ++ "&lon=" ++ lon
--     let url = "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lat=-6.175232396788355&lon=106.82712061061278"
--     manager <- newTlsManager
--     request <- HTTP.parseRequest url
--     HTTP.responseBody <$> HTTP.httpLbs request manager


-- getTelkomArea :: String -> String -> String -> IO ()
-- getTelkomArea lat lon key = do
--     let latitude = lat
--     let longitude = lon
--     let keyApi = key
--     -- let url = "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lon=" ++ lon ++ "&lat=" ++ lat
--     let url = "https://maps.googleapis.com/maps/api/geocode/json?latlng="++latitude++","++longitude++"&key="++keyApi
--     print url
--     r <- Network.Wreq.get url
--     print r