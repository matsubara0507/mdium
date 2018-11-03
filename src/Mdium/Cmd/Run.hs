{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mdium.Cmd.Run where

import           RIO

import           Data.Extensible
import           Mdium.API          (getMe)
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

postStory :: RIO Env ()
postStory = showNotImpl

callMeAPI :: RIO Env ()
callMeAPI = do
  logDebug "Run cmd: call me api"
  token <- asks (view #token)
  user <- getMe token
  logDebug $ display ("get: " <> tshow user)
  logInfo  $ display ("Hi " <> user ^. #name <> "!!")

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
