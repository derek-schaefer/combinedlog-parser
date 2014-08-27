module Main where

import Text.Log.Combined

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = parseLines readEvent

parseLines :: (B.ByteString -> Maybe Event) -> IO ()
parseLines reader = do
  src <- getLine
  case src of
    [] -> return ()
    _  -> do
      case (reader $ B.pack src) of
        Nothing -> putStrLn ""
        Just e  -> do
             putStrLn $ show e
             putStrLn ""
      parseLines reader
