module Mdium.Cmd.Run where

import           RIO
import qualified RIO.HashMap            as HM
import           RIO.State              (StateT (..))
import qualified RIO.State              as State

import           Data.Extensible
import           Data.Fallible
import           GHC.IO.Encoding        (utf8)
import qualified GitHub
import qualified Mdium.API              as API
import           Mdium.Cmd.Options
import           Mdium.Env
import           Mix
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           Mix.Plugin.Logger.JSON as MixLogger
import           System.Environment     (getEnv)
import           System.IO              (hSetEncoding)
import           Text.Pandoc            (Pandoc)
import qualified Text.Pandoc            as Pandoc
import qualified Text.Pandoc.Walk       as Pandoc

run :: MonadUnliftIO m => RIO Env () -> Options -> m ()
run cmd opts = do
  liftIO $ hSetEncoding stdout utf8
  token   <- liftIO $ getEnv "MEDIUM_TOKEN"
  ghToken <- liftIO $ bool (pure "") (getEnv "GH_TOKEN") (isJust $ opts ^. #gist)
  let logOpt = #handle @= stdout <: shrink opts
      plugin = hsequence
             $ #logger     <@=> MixLogger.buildPlugin logOpt
            <: #token      <@=> pure (fromString token)
            <: #github     <@=> pure (fromString ghToken)
            <: #gistPrefix <@=> pure (opts ^. #gist)
            <: nil
  Mix.run plugin cmd

postStory :: [FilePath] -> Text -> RIO Env ()
postStory paths title = evalContT $ do
  MixLogger.logDebugR "run command" (#cmd @= ("post story" :: Text) <: nil)
  path    <- listToMaybe paths ??? exit (MixLogger.logError "please input filepath")
  content <- (lift . customizeContent) =<< readPostContent path
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
  content <- (lift . customizeContent) =<< readPostContent path
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

customizeContent :: Text -> RIO Env Text
customizeContent content = do
  p0 <- liftIO $ Pandoc.runIOorExplode (Pandoc.readCommonMark Pandoc.def content)
  p1 <- replaceCodeBlockToGistLinkIn p0
  liftIO $ Pandoc.runIOorExplode (Pandoc.writeCommonMark Pandoc.def p1)

replaceCodeBlockToGistLinkIn :: Pandoc -> RIO Env Pandoc
replaceCodeBlockToGistLinkIn doc = do
  prefix <- asks (view #gistPrefix)
  case prefix of
    Just p  -> fst <$> runStateT (Pandoc.walkPandocM (replaceCodeBlockToGistLink p) doc) 1
    Nothing -> pure doc

replaceCodeBlockToGistLink :: Text -> Pandoc.Block -> StateT Int (RIO Env) Pandoc.Block
replaceCodeBlockToGistLink prefix = \case
  Pandoc.CodeBlock (_, [ext], _) txt -> do
    cnt  <- State.get
    gist <- lift $ createGist (prefix, tshow cnt) ext txt
    State.modify (+ 1)
    pure $ Pandoc.Plain [Pandoc.Str (GitHub.getUrl $ GitHub.gistHtmlUrl gist)]
  block -> pure block

createGist :: (Text, Text) -> Text -> Text -> RIO Env GitHub.Gist
createGist (prefix, suffix) ext txt = do
  let name  = prefix <> "sample" <> suffix <> "." <> ext
      files = HM.fromList [(name, GitHub.NewGistFile txt)]
  MixLogger.logDebugR "create gist" (#file_name @= name <: nil)
  either throwM pure =<< MixGitHub.fetch (GitHub.createGistR $ GitHub.NewGist "" files True)

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
