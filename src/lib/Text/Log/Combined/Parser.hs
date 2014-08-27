module Text.Log.Combined.Parser
    ( readEvent
    ) where

import Text.Log.Combined.Types

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Time
import System.Locale

import qualified Data.ByteString.Char8 as B

readEvent :: B.ByteString -> Maybe Event
readEvent src = case ee of { Right e -> Just e; _ -> Nothing }
    where ee = parseOnly parseEvent src

parseEvent :: Parser Event
parseEvent = Event
  <$> parseRemote
  <*> parseLogName
  <*> parseAuthUser
  <*> parseTimestamp
  <*> parseRequest
  <*> parseStatus
  <*> parseBytes
  <*> parseReferer
  <*> parseUserAgent

parseRemote :: Parser B.ByteString
parseRemote = takeTill (== ' ') <* char ' '

parseLogName :: Parser (Maybe B.ByteString)
parseLogName = parseOptional ' ' <* char ' '

parseAuthUser :: Parser (Maybe B.ByteString)
parseAuthUser = parseOptional ' ' <* char ' '

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  time <- char '[' *> takeTill (== ']') <* string "] "
  let time' = parseTime defaultTimeLocale timestampFormat (B.unpack time)
  maybe (fail "invalid timestamp") (return . zonedTimeToUTC) time'

parseRequest :: Parser B.ByteString
parseRequest = parseQuoted <* char ' '

parseStatus :: Parser Int
parseStatus = decimal <* char ' '

parseBytes :: Parser Int
parseBytes = decimal

parseReferer :: Parser (Maybe B.ByteString)
parseReferer = char ' ' *> parseOptionalQuoted <|> pure Nothing

parseUserAgent :: Parser (Maybe B.ByteString)
parseUserAgent = char ' ' *> parseOptionalQuoted <|> pure Nothing

timestampFormat :: String
timestampFormat = "%d/%b/%Y:%H:%M:%S %z"

parseOptional :: Char -> Parser (Maybe B.ByteString)
parseOptional end = char '-' *> pure Nothing <|> Just <$> takeTill (== end)

parseOptionalQuoted :: Parser (Maybe B.ByteString)
parseOptionalQuoted = do
  opt <- char '"' *> parseOptional '"' <* char '"'
  return $ opt >>= \s -> case s of { "-" -> Nothing; _ -> opt }

parseQuoted :: Parser B.ByteString
parseQuoted = parseOptionalQuoted >>= \opt -> return $ maybe B.empty id opt
