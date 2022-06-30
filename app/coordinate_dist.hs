
import Data.Morton


newtype LatLong =
    -- | Underlying reperesentation and source of ordering for indexing
    LatLongZ Morton
    deriving (Eq, Ord)

two32f :: Double
two32f = 2 ^ (32 :: Int)


makeLatLong :: Double -> Double -> LatLong
makeLatLong theta phi = LatLongZ (MortonPair theta' phi') where
    theta' = clampLat . floor $ (theta + 90) / 360 * two32f
    phi' = wrapLong . floor $ (phi + 180) / 360 * two32f
    clampLat (x::Int) = fromIntegral (max 0 (min 0x7fffffff x))
    wrapLong (x::Int) = fromIntegral (x `mod` 0x100000000)