module Main where

import           Paths_mdium            (version)
import           RIO
import qualified RIO.ByteString         as B
import           RIO.Directory          (doesFileExist, getHomeDirectory)

import           Configuration.Dotenv   (Config (..), defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Development.GitRev
import           Mdium.Cmd

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args -> do
  homeDir <- getHomeDirectory
  _ <- loadEnvFileIfExist $ defaultConfig
  _ <- loadEnvFileIfExist $ defaultConfig { configPath = [homeDir <> "/.env"] }
  case toCmd (#input @= args <: r) of
    PrintVersion          -> B.putStr $ fromString (showVersion version <> "\n")
    CallMeAPI opts'       -> run callMeAPI opts'
    PostStory opts'       -> run (postStory (opts' ^. #input) (opts' ^. #title)) opts'
    PostStroyTo opts' pid -> run (postStroyWithPublicationId pid (opts' ^. #input) (opts' ^. #title)) opts'
    PublicationsAPI opts' -> run publicationsAPI opts'
  where
    loadEnvFileIfExist conf =
      whenM (and <$> mapM doesFileExist (configPath conf)) (void $ loadFile conf)
    opts = #version      @= versionOpt
        <: #verbose      @= verboseOpt
        <: #me           @= meOpt
        <: #title        @= titleOpt
        <: #org          @= orgOpt
        <: #publications @= publicationsOpt
        <: nil

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
