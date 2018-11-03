{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mdium.Cmd.Run where

import           RIO

import           Data.Extensible
import           Mdium.Cmd.Options
import           Mdium.Env
import           System.Environment (getEnv)

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  token   <- liftIO $ getEnv "MEDIUM_TOKEN"
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: #token  @= token
           <: nil
    runRIO env run'

run' :: RIO Env ()
run' = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
