{-# LANGUAGE OverloadedStrings #-}

module Text.CombinedLog.Parser where

import Text.CombinedLog.Types

import Control.Applicative
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

readEvent :: T.Text -> Maybe Event
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

parseRemote :: Parser T.Text
parseRemote = takeTill (== ' ') <* char ' '

parseLogName :: Parser (Maybe T.Text)
parseLogName = (char '-' *> pure Nothing <|> Just <$> takeTill (== ' ')) <* char ' '

parseAuthUser :: Parser (Maybe T.Text)
parseAuthUser = (char '-' *> pure Nothing <|> Just <$> takeTill (== ' ')) <* char ' '

parseTimestamp :: Parser ZonedTime
parseTimestamp = do
  time <- char '[' *> takeTill (== ']') <* string "] "
  let time' = parseTime defaultTimeLocale timestampFormat (T.unpack time)
  maybe (fail "invalid timestamp") return time'

parseRequest :: Parser T.Text
parseRequest = char '"' *> takeTill (== '"') <* string "\" "

parseStatus :: Parser Int
parseStatus = decimal <* char ' '

parseBytes :: Parser Int
parseBytes = decimal

parseReferer :: Parser (Maybe T.Text)
parseReferer = string " \"-\"" *> pure Nothing <|>
               Just <$> (string " \"" *> takeTill (== '"') <* char '"') <|>
               pure Nothing

parseUserAgent :: Parser (Maybe T.Text)
parseUserAgent = string " \"-\"" *> pure Nothing <|>
                 Just <$> (string " \"" *> takeTill (== '"') <* char '"') <|>
                 pure Nothing

timestampFormat :: String
timestampFormat = "%d/%b/%Y:%H:%M:%S %z"
