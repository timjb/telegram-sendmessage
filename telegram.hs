#!/usr/bin/env stack
-- stack --resolver lts-8.6 --install-ghc runghc --package telegram-api --package text --package directory --package filepath --package http-client --package http-client-tls

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (when, forM_)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Web.Telegram.API.Bot as TG

exitWithErrorMsg :: String -> IO a
exitWithErrorMsg msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

configFile :: FilePath
configFile = "~/.telegram-token"

replaceHomeDirectory :: FilePath -> IO FilePath
replaceHomeDirectory "~" = getHomeDirectory
replaceHomeDirectory ('~':'/':path) =
  (</> path) <$> getHomeDirectory
replaceHomeDirectory path = return path

ensureIsPrefixOf :: T.Text -> T.Text -> T.Text
ensureIsPrefixOf prefix str =
  if prefix `T.isPrefixOf` str then
    str
  else
    prefix <> str

readConfig :: IO TG.Token
readConfig = do
  path <- replaceHomeDirectory configFile
  catch (TG.Token . ensureIsPrefixOf "bot" . T.strip <$> T.readFile path)
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

sendMessage :: TG.Token -> String -> IO ()
sendMessage token msg = do
  manager <- newManager tlsManagerSettings
  updatesResponse <- TG.getUpdates token Nothing Nothing Nothing manager
  case updatesResponse of
    Left err ->
      exitWithErrorMsg ("ERROR: Could not call getUpdates!\n\n" ++ show err)
    Right (TG.Response { TG.result = updates }) ->
      forM_ userIds $ \userId ->
        let chatId = TG.ChatId (fromIntegral userId)
            request = TG.sendMessageRequest chatId (T.pack msg)
        in TG.sendMessage token request manager
      where
        userIds =
          map (TG.chat_id . TG.chat) $
            mapMaybe TG.message updates

main :: IO ()
main = do
  args <- getArgs
  when (null args || "--help" `elem` args) usage
  token <- readConfig
  let msg = unwords args
  sendMessage token msg
