module Module.Tiang where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

data LogTiang
    = LogTiang
        { id :: Int
        , coordinates :: (Double, Double)
        , sto :: [Char]
        }
    | UnknownTiang
    deriving (Show, Eq)