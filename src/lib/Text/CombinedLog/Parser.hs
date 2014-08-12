{-# LANGUAGE OverloadedStrings #-}

module Text.CombinedLog.Parser where

import Text.CombinedLog.Types

import Control.Applicative
import Data.Attoparsec.Text
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

readEvent :: Text -> Maybe Event
readEvent src = either (\_ -> Nothing) Just ee
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

parseRemote :: Parser Text
parseRemote = takeTill (== ' ') <* char ' '

parseLogName :: Parser (Maybe Text)
parseLogName = (char '-' *> pure Nothing <|> Just <$> takeTill (== ' ')) <* char ' '

parseAuthUser :: Parser (Maybe Text)
parseAuthUser = (char '-' *> pure Nothing <|> Just <$> takeTill (== ' ')) <* char ' '

parseTimestamp :: Parser ZonedTime
parseTimestamp = do
  time <- char '[' *> takeTill (== ']') <* string "] "
  let time' = parseTime defaultTimeLocale timestampFormat (T.unpack time)
  maybe (fail "invalid timestamp") return time'

parseRequest :: Parser Text
parseRequest = parseQuoted >>= \txt ->
  return $ case txt of { Just t -> t; Nothing -> T.empty }

parseStatus :: Parser Int
parseStatus = decimal <* char ' '

parseBytes :: Parser Int
parseBytes = decimal

parseReferer :: Parser (Maybe Text)
parseReferer = parseQuoted

parseUserAgent :: Parser (Maybe Text)
parseUserAgent = parseQuoted

timestampFormat :: String
timestampFormat = "%d/%b/%Y:%H:%M:%S %z"

parseQuoted :: Parser (Maybe Text)
parseQuoted = do
  txt <- Just <$> (string " \"" *> takeTill (== '"') <* char '"') <|> pure Nothing
  return $ txt >>= \t -> case t of { "-" -> Nothing; _ -> txt }
