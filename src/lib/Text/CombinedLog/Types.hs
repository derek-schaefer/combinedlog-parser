module Text.CombinedLog.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime

data Event = Event
    { remote :: Text
    , logName :: Maybe Text
    , authUser :: Maybe Text
    , timestamp :: ZonedTime
    , request :: Text
    , status :: Int
    , bytes :: Int
    , referer :: Maybe Text
    , userAgent :: Maybe Text
    } deriving (Show, Eq)

instance Eq ZonedTime where
    (ZonedTime t1 z1) == (ZonedTime t2 z2) = (t1 == t2) && (z1 == z2)
