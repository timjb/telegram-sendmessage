#!/usr/bin/env stack
-- stack --resolver lts-6.8 --install-ghc runghc --package wreq --package directory

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad (when, forM_, void)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (pack, strip)
import qualified Data.Text.IO as T (readFile)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getHomeDirectory)
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.IO
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

readConfig :: IO TG.Token
readConfig = do
  path <- replaceHomeDirectory configFile
  catch (TG.Token . strip <$> T.readFile path)
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
    Left err -> exitWithErrorMsg $ show err
    Right res ->
      forM_ userIds $ \userId ->
        let request = TG.sendMessageRequest (pack $ show userId) (pack msg)
        in TG.sendMessage token request manager
      where
        userIds =
          map (TG.chat_id . TG.chat) $
            mapMaybe TG.message $
              TG.update_result res
  -- let tgApi = telegramApi token
  -- updatesJson <- tgApi "getUpdates" defaults
  -- let uniq = concat . map (take 1) . group . sort
  --     userIds = uniq $ updatesJson ^.. values . key "message" . key "from" . key "id" . _Integer
  -- forM_ userIds $ \userId -> do
  --   void $ tgApi "sendMessage" $
  --     defaults & param "chat_id" .~ [pack (show userId)]
  --              & param "text" .~ [pack msg]
  --   putStrLn $ "Sent message to " <> show userId


main :: IO ()
main = do
  args <- getArgs
  when (null args || "--help" `elem` args) usage
  token <- readConfig
  let msg = unwords args
  sendMessage token msg
