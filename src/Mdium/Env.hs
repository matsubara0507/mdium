{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mdium.Env where

import           RIO

import           Data.Extensible

type Env = Record
  '[ "logger" >: LogFunc
   , "token"  >: MediumToken
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

type MediumToken = ByteString
