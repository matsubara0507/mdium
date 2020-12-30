{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mdium.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub as MixGitHub

type Env = Record
  '[ "logger" >: LogFunc
   , "token"  >: MediumToken
   , "github" >: MixGitHub.Token
   ]

type MediumToken = ByteString
