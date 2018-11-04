{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mdium.Cmd.Run where

import           RIO

import           Data.Extensible
import qualified Mdium.API          as API (getMe, postStory)
import           Mdium.Cmd.Options
import           Mdium.Env
import           System.Environment (getEnv)

run :: (MonadUnliftIO m, MonadThrow m) => RIO Env () -> Options -> m ()
run cmd opts = do
  token   <- liftIO $ getEnv "MEDIUM_TOKEN"
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: #token  @= fromString token
           <: nil
    runRIO env cmd

postStory :: FilePath -> Text -> RIO Env ()
postStory path title = do
  logDebug "Run cmd: post story"

  logDebug $ fromString ("read file: " <> path)
  content <- readFileUtf8 path
  logDebug $ display ("readed file: " <> content)

  token   <- asks (view #token)
  user <- API.getMe token
  logDebug $ display ("get: " <> tshow user)

  let params = #title           @= title
            <: #contentFormat   @= "markdown"
            <: #content         @= content
            <: #tags            @= []
            <: #canonicalUrl    @= Nothing
            <: #publishStatus   @= Just "draft"
            <: #license         @= Nothing
            <: #notifyFollowers @= Nothing
            <: nil
  logDebug $ display ("post params: " <> tshow params)

  story <- API.postStory token user params
  logDebug $ display ("post: " <> tshow story)
  logInfo  $ display ("post success, browse to: " <> story ^. #url)

callMeAPI :: RIO Env ()
callMeAPI = do
  logDebug "Run cmd: call me api"
  token <- asks (view #token)
  user <- API.getMe token
  logDebug $ display ("get: " <> tshow user)
  logInfo  $ display ("Hi " <> user ^. #name <> "!!")

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"