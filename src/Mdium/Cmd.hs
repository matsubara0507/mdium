{-# LANGUAGE OverloadedLabels #-}

module Mdium.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           Mdium.Cmd.Options as X
import           Mdium.Cmd.Run     as X

data Cmd
  = PrintVersion
  | RunCmd Options
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = RunCmd opts
