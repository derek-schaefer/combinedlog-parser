module Main where

import Text.CombinedLog

import qualified Data.Text as T
import System.Environment

main :: IO ()
main = parseLines readEvent

parseLines :: (T.Text -> Maybe Event) -> IO ()
parseLines reader = do
  src <- getLine
  case src of
    [] -> return ()
    _  -> do
      case (reader $ T.pack src) of
        Nothing -> putStrLn ""
        Just e  -> do
             putStrLn $ show e
             putStrLn ""
      parseLines reader
