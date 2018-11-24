{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mdium.Cmd.Run where

import           RIO

import           Data.Extensible
import           GHC.IO.Encoding    (utf8)
import qualified Mdium.API          as API
import           Mdium.Cmd.Options
import           Mdium.Env
import           System.Environment (getEnv)
import           System.IO          (hSetEncoding)

run :: MonadUnliftIO m => RIO Env () -> Options -> m ()
run cmd opts = do
  liftIO $ hSetEncoding stdout utf8
  token   <- liftIO $ getEnv "MEDIUM_TOKEN"
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: #token  @= fromString token
           <: nil
    runRIO env cmd

postStory :: [FilePath] -> Text -> RIO Env ()
postStory paths title = do
  logDebug "Run cmd: post story"
  case listToMaybe paths of
    Just path -> postStory' path
    Nothing   -> logError "please input filepath"
  where
    postStory' path = do
      content <- readPostContent path
      token   <- asks (view #token)
      params  <- constructPostParams title content
      user    <- API.getMe token
      logDebug $ display ("get: " <> tshow user)

      story <- API.postStory token user params
      logDebug $ display ("post: " <> tshow story)
      logInfo  $ display ("post success, browse to: " <> story ^. #url)

postStroyWithPublicationId :: Text -> [FilePath] -> Text -> RIO Env ()
postStroyWithPublicationId pid paths title = do
  logDebug "Run cmd: post story"
  logDebug $ display ("publicationId: " <> pid)
  case listToMaybe paths of
    Just path -> postStory' path
    Nothing   -> logError "please input filepath"
  where
    postStory' path = do
      content <- readPostContent path
      token   <- asks (view #token)
      params  <- constructPostParams title content

      story <- API.postStroyWithPublicationId token pid params
      logDebug $ display ("post: " <> tshow story)
      logInfo  $ display ("post success, browse to: " <> story ^. #url)

readPostContent :: FilePath -> RIO Env Text
readPostContent path = do
  logDebug $ fromString ("read file: " <> path)
  content <- readFileUtf8 path
  logDebug $ display ("readed file: " <> content)
  pure content

constructPostParams :: Text -> Text -> RIO Env API.PostStoryParams
constructPostParams title content = do
  logDebug $ display ("post params: " <> tshow params)
  pure params
  where
    params = API.defaultPostStroyParams
           & #title `set` title
           & #contentFormat `set` "markdown"
           & #content `set` content
           & #publishStatus `set` Just "draft"

callMeAPI :: RIO Env ()
callMeAPI = do
  logDebug "Run cmd: call me api"
  token <- asks (view #token)
  user <- API.getMe token
  logDebug $ display ("get: " <> tshow user)
  logInfo  $ display ("Hi " <> user ^. #name <> "!!")

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
