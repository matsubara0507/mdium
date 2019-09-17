module Mdium.Cmd.Run where

import           RIO

import           Data.Extensible
import           Data.Fallible
import           GHC.IO.Encoding        (utf8)
import qualified Mdium.API              as API
import           Mdium.Cmd.Options
import           Mdium.Env
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Mix.Plugin.Logger.JSON as MixLogger
import           System.Environment     (getEnv)
import           System.IO              (hSetEncoding)

run :: MonadUnliftIO m => RIO Env () -> Options -> m ()
run cmd opts = do
  liftIO $ hSetEncoding stdout utf8
  token <- liftIO $ getEnv "MEDIUM_TOKEN"
  let logOpt = #handle @= stdout <: shrink opts
      plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpt
            <: #token  <@=> pure (fromString token)
            <: nil
  Mix.run plugin cmd

postStory :: [FilePath] -> Text -> RIO Env ()
postStory paths title = evalContT $ do
  MixLogger.logDebugR "run command" (#cmd @= ("post story" :: Text) <: nil)
  path    <- listToMaybe paths ??? exit (MixLogger.logError "please input filepath")
  content <- readPostContent path
  token   <- asks (view #token)
  params  <- constructPostParams title content
  user    <- API.getMe token
  MixLogger.logDebugR "get user" (#user @= user <: nil)
  story   <- API.postStory token user params
  MixLogger.logDebugR "post story" (#story @= story <: nil)
  MixLogger.logInfo $ display ("post success, browse to: " <> story ^. #url)

postStroyWithPublicationId :: Text -> [FilePath] -> Text -> RIO Env ()
postStroyWithPublicationId pid paths title = evalContT $ do
  MixLogger.logDebugR "run command"
    (#cmd @= ("post story with publication id" :: Text) <: #publication_id @= pid <: nil)
  path    <- listToMaybe paths ??? exit (MixLogger.logError "please input filepath")
  content <- readPostContent path
  token   <- asks (view #token)
  params  <- constructPostParams title content
  story   <- API.postStroyWithPublicationId token pid params
  MixLogger.logDebugR "post story" (#story @= story <: nil)
  MixLogger.logInfo $ display ("post success, browse to: " <> story ^. #url)

readPostContent ::
  (MonadIO m, MonadReader e m, HasLogFunc e) => FilePath -> m Text
readPostContent path = do
  MixLogger.logDebugR "read file" (#path @= path <: nil)
  content <- readFileUtf8 path
  MixLogger.logDebugR "readed file" (#content @= content <: nil)
  pure content

constructPostParams ::
  (MonadIO m, MonadReader e m, HasLogFunc e) => Text -> Text -> m API.PostStoryParams
constructPostParams title content = do
  MixLogger.logDebugR  "post params" (#params @= params <: nil)
  pure params
  where
    params = API.defaultPostStroyParams
           & #title `set` title
           & #contentFormat `set` "markdown"
           & #content `set` content
           & #publishStatus `set` Just "draft"

callMeAPI :: RIO Env ()
callMeAPI = do
  MixLogger.logDebugR "run command" (#cmd @= ("call me api" :: Text) <: nil)
  token <- asks (view #token)
  user  <- API.getMe token
  MixLogger.logDebugR "get user" (#user @= user <: nil)
  MixLogger.logInfo $ display ("Hi " <> user ^. #name <> "!!")

publicationsAPI :: RIO Env ()
publicationsAPI = do
  MixLogger.logDebugR "run command" (#cmd @= ("call publications api" :: Text) <: nil)
  token <- asks (view #token)
  user  <- API.getMe token
  publications <- API.getPublications token (user ^. #id)
  MixLogger.logDebugR "get publications" (#publications @= publications <: nil)
  forM_ publications $ \publication ->
    MixLogger.logInfo $ display (publication ^. #name <> " (id: " <> publication ^. #id <> ")")

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
