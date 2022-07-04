module Module.Request where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

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

parseTelkomArea :: [TelkomArea] -> IO ()
parseTelkomArea telkomArealist = do
    let convertToLog :: [TelkomArea] -> String
        convertToLog [] = ""
        convertToLog (x : xs) =
            show (regional x)
                ++ ", "
                ++ show (witel x)
                ++ ", "
                ++ show (name x)
                ++ ", "
                ++ show (description x)
                ++ "\n"
    let parseTelkomArea = Prelude.init $ convertToLog telkomArealist -- using init to remove the last \n at the end of the .log
    writeFile "log/telkom_area.log" parseTelkomArea

showTelkomArea :: TelkomArea -> String
showTelkomArea x = do
    let convertToLog :: TelkomArea -> String
        convertToLog telkomArea =
                "Telkom Area: "
                ++ show (regional x)
                ++ ", Witel: "
                ++ show (witel x)
                ++ ", STO: "
                ++ show (description x)
                ++ show " ["
                ++ show (name x)
                ++ show "]"
                ++ "\n"
    Prelude.init $ convertToLog x

showPlace :: PlaceCode -> String
showPlace x = do
    let convertToLog :: PlaceCode -> String
        convertToLog x = show $ Prelude.unwords (Prelude.tail $ Prelude.words $ show (compound_code x))
    Prelude.init $ convertToLog x

-- getTelkomArea :: String -> String -> String -> IO ()
-- getTelkomArea lat lon key = do
--     let latitude = lat
--     let longitude = lon
--     -- let keyApi = key
--     let url = "https://api-emas.telkom.co.id:9093/api/area/findByLocation?lon=" ++ lon ++ "&lat=" ++ lat
--     -- let url = "https://maps.googleapis.com/maps/api/geocode/json?latlng="++latitude++","++longitude++"&key="++keyApi
--     print url
--     r <- Network.Wreq.get url
--     print r

testHost :: BC.ByteString
testHost = "https://api-emas.telkom.co.id:9093/"

apiPath :: BC.ByteString
apiPath = "api/area/findByLocation?lon=-6.175232396788355&lat=106.82712061061278"

buildRequest :: BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443
                                  $ defaultRequest

request :: Request
request = buildRequest testHost "GET" apiPath