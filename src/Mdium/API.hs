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

type Publication = Record
   '[ "id"          >: Text
    , "name"        >: Text
    , "description" >: Text
    , "url"         >: Text
    , "imageUrl"    >: Text
    ]

defaultPostStroyParams :: PostStoryParams
defaultPostStroyParams
    = #title           @= ""
   <: #contentFormat   @= "html"
   <: #content         @= ""
   <: #tags            @= []
   <: #canonicalUrl    @= Nothing
   <: #publishStatus   @= Nothing
   <: #license         @= Nothing
   <: #notifyFollowers @= Nothing
   <: nil

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

getPublications ::
  (MonadThrow m, MonadIO m) => MediumToken -> Text -> m [Publication]
getPublications token userId = do
  resp <- W.asJSON =<< liftIO (W.getWith opts url)
  pure $ peelData (resp ^. W.responseBody)
  where
    url  = baseUrl <> "/users/" <> Text.unpack userId <> "/publications"
    opts = W.defaults & auth `set` Just (oauth2Bearer token)

postStory ::
  (MonadThrow m, MonadIO m) => MediumToken -> User -> PostStoryParams -> m Story
postStory token user params = do
  resp <- W.asJSON =<< liftIO (W.postWith opts url $ toJSON params)
  pure $ peelData (resp ^. W.responseBody)
  where
    url  = baseUrl <> "/users/" <> Text.unpack (user ^. #id) <> "/posts"
    opts = W.defaults & auth `set` Just (oauth2Bearer token)

postStroyWithPublicationId ::
  (MonadThrow m, MonadIO m) => MediumToken -> Text -> PostStoryParams -> m Story
postStroyWithPublicationId token pid params = do
  resp <- W.asJSON =<< liftIO (W.postWith opts url $ toJSON params')
  pure $ peelData (resp ^. W.responseBody)
  where
    url     = baseUrl <> "/publications/" <> Text.unpack pid <> "/posts"
    opts    = W.defaults & auth `set` Just (oauth2Bearer token)
    params' = #publicationId @= pid <: params
