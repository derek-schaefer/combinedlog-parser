module Text.CombinedLog.Types where

import qualified Data.ByteString.Char8 as B
import Data.Time.LocalTime

data Event = Event
    { remote :: B.ByteString
    , logName :: Maybe B.ByteString
    , authUser :: Maybe B.ByteString
    , timestamp :: ZonedTime
    , request :: B.ByteString
    , status :: Int
    , bytes :: Int
    , referer :: Maybe B.ByteString
    , userAgent :: Maybe B.ByteString
    } deriving (Show, Eq)

instance Eq ZonedTime where
    (ZonedTime t1 z1) == (ZonedTime t2 z2) = (t1 == t2) && (z1 == z2)
