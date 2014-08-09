module Text.CombinedLog.Types where

import qualified Data.Text as T
import Data.Time.LocalTime

data Event = Event
    { remote :: T.Text
    , logName :: Maybe T.Text
    , authUser :: Maybe T.Text
    , timestamp :: ZonedTime
    , request :: T.Text
    , status :: Int
    , bytes :: Int
    , referer :: Maybe T.Text
    , userAgent :: Maybe T.Text
    } deriving (Show, Eq)

instance Eq ZonedTime where
    (ZonedTime t1 z1) == (ZonedTime t2 z2) = (t1 == t2) && (z1 == z2)
