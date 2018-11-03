{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mdium.API where

import           RIO

import           Data.Extensible
import           Mdium.Env       (MediumToken)
import           Network.Wreq    (auth, oauth2Bearer)
import qualified Network.Wreq    as W

type RespData a = Record '[ "data" >: a ]

type User = Record
   '[ "id"       >: Text
    , "username" >: Text
    , "name"     >: Text
    , "url"      >: Text
    , "imageUrl" >: Text
    ]

peelData :: RespData a -> a
peelData = view #data

baseUrl :: String
baseUrl = "https://api.medium.com/v1"

getMe :: (MonadThrow m, MonadIO m) => MediumToken -> m User
getMe token = do
  resp <- W.asJSON =<< liftIO (W.getWith opts $ baseUrl <> "/me")
  pure $ peelData (resp ^. W.responseBody)
  where
    opts = W.defaults & auth `set` Just (oauth2Bearer token)
