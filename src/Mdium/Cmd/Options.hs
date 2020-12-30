module Mdium.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt

type Options = Record
  '[ "input"        >: [String]
   , "help"         >: Bool
   , "version"      >: Bool
   , "verbose"      >: Bool
   , "me"           >: Bool
   , "title"        >: Text
   , "org"          >: Maybe Text
   , "publications" >: Bool
   , "gist"         >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag [] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

meOpt :: OptDescr' Bool
meOpt = optFlag [] ["me"] "Call Medium `me` API"

titleOpt :: OptDescr' Text
titleOpt =
  fromString <$> optLastArgWithDefault [] ["title"] "" "TEXT" "Specify title of story that post to medium"

orgOpt :: OptDescr' (Maybe Text)
orgOpt =
  fmap fromString <$> optLastArg [] ["org"] "PUBLICATION_ID" "Post story to PUBLICATION_ID organization"

publicationsOpt :: OptDescr' Bool
publicationsOpt = optFlag [] ["publications"] "Call Medium `publications` API"

gistOpt :: OptDescr' Bool
gistOpt = optFlag [] ["gist"] "Replace code block to gist link"

optLastArgWithDefault
  :: [Char]   -- ^ short option
  -> [String] -- ^ long option
  -> String   -- ^ default value
  -> String   -- ^ placeholder
  -> String   -- ^ explanation
  -> OptDescr' String
optLastArgWithDefault ss ls dv ph expl = fromMaybe dv <$> optLastArg ss ls ph expl
