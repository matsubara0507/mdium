module Main where

import           Paths_mdium          (version)
import           RIO
import           RIO.Directory        (doesFileExist, getHomeDirectory)

import           Configuration.Dotenv (Config (..), defaultConfig, loadFile)
import           Data.Extensible
import           GetOpt               (withGetOpt')
import           Mdium.Cmd
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  homeDir <- getHomeDirectory
  _ <- loadEnvFileIfExist $ defaultConfig
  _ <- loadEnvFileIfExist $ defaultConfig { configPath = [homeDir <> "/.env"] }
  case toCmd (#input @= args <: r) of
    PrintHelp             -> hPutBuilder stdout (fromString usage)
    PrintVersion          -> hPutBuilder stdout (Version.build version)
    CallMeAPI opts'       -> run callMeAPI opts'
    PostStory opts'       -> run (postStory (opts' ^. #input) (opts' ^. #title)) opts'
    PostStroyTo opts' pid -> run (postStroyWithPublicationId pid (opts' ^. #input) (opts' ^. #title)) opts'
    PublicationsAPI opts' -> run publicationsAPI opts'
  where
    loadEnvFileIfExist conf =
      whenM (and <$> mapM doesFileExist (configPath conf)) (void $ loadFile conf)
    opts = #help         @= helpOpt
        <: #version      @= versionOpt
        <: #verbose      @= verboseOpt
        <: #me           @= meOpt
        <: #title        @= titleOpt
        <: #org          @= orgOpt
        <: #publications @= publicationsOpt
        <: nil
