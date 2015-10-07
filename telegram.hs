#!/usr/bin/env stack
-- stack --resolver lts-3.8 --install-ghc runghc --package wreq --package directory

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Lens
import Control.Monad (when, forM_, void)
import Data.Aeson
import Data.Aeson.Lens
import Data.Char (isSpace)
import Data.List (sort, group)
import Data.Monoid ((<>))
import Data.Text (pack)
import Network.Wreq
import System.Directory (getHomeDirectory)
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.IO

exitWithErrorMsg :: String -> IO a
exitWithErrorMsg msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

newtype Token = Token { getToken :: String } deriving (Show)

configFile :: FilePath
configFile = "~/.telegram-token"

replaceHomeDirectory :: FilePath -> IO FilePath
replaceHomeDirectory "~" = getHomeDirectory
replaceHomeDirectory ('~':'/':path) =
  (</> path) <$> getHomeDirectory
replaceHomeDirectory path = return path

strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace

readConfig :: IO Token
readConfig = do
  path <- replaceHomeDirectory configFile
  catch (Token . strip <$> readFile path)
        (\(e :: IOException) -> exitWithErrorMsg errMsg)
  where errMsg = "Could not read config file '" <> configFile <> "'!"

usage :: IO ()
usage =
  exitWithErrorMsg $
    "Usage: `telegram your message goes here`\n\n" <>
    "This uses the Telegram API to send a message to\n" <>
    "all users having a conversation with the bot.\n" <>
    "You need to get an API token first and put it in\n" <>
    "  " <> configFile <> "\n" <>
    "You can get such a token from the Botfather\n" <>
    "(see https://core.telegram.org/bots#botfather)"

telegramApi :: Token -> String -> Options -> IO Value
telegramApi (Token token) method opts = do
  let url = "https://api.telegram.org/bot" <> token <> "/" <> method
      opts' = opts & header "Accept" .~ ["application/json"]
  res <- getWith opts' url
  json <- asValue res
  case json ^? responseBody . key "result" of
    Just result -> return result
    Nothing ->
      exitWithErrorMsg $
        "API call to '" <> url <> "' returned \n\n" <>
        show json

sendMessage :: Token -> String -> IO ()
sendMessage token msg = do
  let tgApi = telegramApi token
  updatesJson <- tgApi "getUpdates" defaults
  let uniq = concat . map (take 1) . group . sort
      userIds = uniq $ updatesJson ^.. values . key "message" . key "from" . key "id" . _Integer
  forM_ userIds $ \userId -> do
    void $ tgApi "sendMessage" $
      defaults & param "chat_id" .~ [pack (show userId)]
               & param "text" .~ [pack msg]
    putStrLn $ "Sent message to " <> show userId

main :: IO ()
main = do
  args <- getArgs
  when (null args || "--help" `elem` args) usage
  token <- readConfig
  let msg = unwords args
  sendMessage token msg