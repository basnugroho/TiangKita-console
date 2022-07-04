module Helper where

import Control.Monad (ap)
import Data.Char (isDigit)
import System.IO (hFlush, stdout)
import Text.Read
-- import Control.Monad.Trans
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Writer
-- import Data.Monoid

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
    -- return :: a -> MaybeT m a
    return = MaybeT . return . Just

    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    MaybeT mma >>= f = MaybeT $ do
        ma <- mma
        case ma of
            Nothing -> return Nothing
            Just a -> runMaybeT $ f a

instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (MaybeT m) where
    fmap f x = pure f <*> x

liftMaybeT :: (Monad m) => m a -> MaybeT m a
liftMaybeT ma = MaybeT (fmap Just ma)

maybeReadInt :: MaybeT IO Int
maybeReadInt = do
    s <- liftMaybeT getLine
    if all isDigit s
        then return $ read s
        else MaybeT $ return Nothing


maybeReadDouble :: String -> Maybe Double
maybeReadDouble input = readMaybe input :: Maybe Double

isEmail :: String -> Bool
isEmail [] = False
isEmail input = '@' `elem` input

-- getEmail :: IOMaybe String
-- getEmail = do
--     input <- getLine
--     if isEmail input
--         then returnIOM input
--     else return Nothing