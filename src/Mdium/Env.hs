{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mdium.Env where

import           RIO

import           Data.Extensible

type Env = Record
  '[ "logger" >: LogFunc
   , "token"  >: MediumToken
   ]

type MediumToken = ByteString
