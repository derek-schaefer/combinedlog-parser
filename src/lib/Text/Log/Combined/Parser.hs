module Text.Log.Combined.Parser
    ( readEvent
    , parseEvent
    , parseReferer
    , parseUserAgent
    ) where

import Text.Log.Combined.Types
import qualified Text.Log.Common.Parser as CP

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

readEvent :: B.ByteString -> Maybe Event
readEvent src = case ee of { Right e -> Just e; _ -> Nothing }
    where ee = parseOnly parseEvent src

parseEvent :: Parser Event
parseEvent = Event
  <$> CP.parseRemote
  <*> CP.parseLogName
  <*> CP.parseAuthUser
  <*> CP.parseTimestamp
  <*> CP.parseRequest
  <*> CP.parseStatus
  <*> CP.parseBytes
  <*> parseReferer
  <*> parseUserAgent

parseReferer :: Parser (Maybe B.ByteString)
parseReferer = char ' ' *> CP.parseOptionalQuoted <|> pure Nothing

parseUserAgent :: Parser (Maybe B.ByteString)
parseUserAgent = char ' ' *> CP.parseOptionalQuoted <|> pure Nothing
