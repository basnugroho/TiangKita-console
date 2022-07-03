{-# LANGUAGE OverloadedStrings #-}

module Module.GoogleMap where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.IO as T (getLine, putStr)
import Graphics.Gloss (Display (..), display, white)
import Graphics.Gloss.Juicy (fromDynamicImage)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Google.Geocoding (Address (..), geocode, GeocodingResponse (..),
  Geometry (..), Key (..), LatLng (..), Result (..), Status (..))
import Web.Google.Maps.Static (Center (..), Location (..), Size (..),
  staticmap, Zoom (..))
import System.IO (hFlush, stdout)

-- main :: IO ()
-- main = do
--   putStrLn $ "A test of the Google Geocoding API.\nNB: The use of " ++
--     "the API services is subject to the Google Maps Platform Terms of " ++
--     "Serivce at https://cloud.google.com/maps-platform/terms/.\n"
--   txt <- input "Enter full address: "
--   mgr <- newManager tlsManagerSettings
--   let apiKey = Key "<REPLACE_THIS_WITH_YOUR_ACTUAL_GOOGLE_API_KEY>"
--   result <- geocode mgr apiKey (Just $ Address txt) Nothing Nothing
--     Nothing Nothing
--   case result of
--     Right response -> do
--       let s = status response
--       case s of
--         OK -> do
--           let latlng = location $ geometry $ head $ results response
--               center = Center $ Coord latlng
--           print center
--           displayMap mgr apiKey center
--         _  -> putStrLn $ "Error! Status: " ++ show s
--     _ -> putStrLn $ "Error! Result:\n" ++ show result

-- input :: Text -> IO Text
-- input msg = T.putStr msg >> hFlush stdout >> T.getLine

-- displayMap :: Manager -> Key -> Center -> IO ()
-- displayMap mgr apiKey center = do
--   let zoom = Just $ Zoom 17
--       w    = 400
--       h    = 400
--       size = Size w h
--   result <- staticmap mgr apiKey Nothing (Just center) zoom size Nothing
--     Nothing [] Nothing [] [] Nothing
--   case result of
--     Right response -> do
--       let picture = fromJust $ fromDynamicImage response
--           title   = "Test Google Geocoding API"
--           window  = InWindow title (w, h) (10, 10)
--       display window white picture
--     Left err -> putStrLn $ "Error while displaying map: " ++ show err