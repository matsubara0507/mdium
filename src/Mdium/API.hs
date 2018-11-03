{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mdium.API where

import           RIO
import qualified RIO.Text        as Text

import           Data.Aeson      (toJSON)
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

type Story = Record
   '[ "id"            >: Text
    , "title"         >: Text
    , "authorId"      >: Text
    , "tags"          >: [Text]
    , "url"           >: Text
    , "canonicalUrl"  >: Maybe Text
    , "publishStatus" >: Text
    , "publishedAt"   >: Maybe Text
    , "license"       >: Maybe Text
    , "licenseUrl"    >: Text
    ]

type PostStoryParams = Record
   '[ "title"           >: Text
    , "contentFormat"   >: Text       -- ^ "html" or "markdown"
    , "content"         >: Text
    , "tags"            >: [Text]
    , "canonicalUrl"    >: Maybe Text
    , "publishStatus"   >: Maybe Text -- ^ "public", "draft" or "unlisted"
    , "license"         >: Maybe Text
    , "notifyFollowers" >: Maybe Bool
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

postStory ::
  (MonadThrow m, MonadIO m) => MediumToken -> User -> PostStoryParams -> m Story
postStory token user params = do
  resp <- W.asJSON =<< liftIO (W.postWith opts url $ toJSON params)
  pure $ peelData (resp ^. W.responseBody)
  where
    url  = baseUrl <> "/users/" <> Text.unpack (user ^. #id) <> "/posts"
    opts = W.defaults & auth `set` Just (oauth2Bearer token)
