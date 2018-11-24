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
  | CallMeAPI Options
  | PostStory Options
  | PostStroyTo Options Text
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | opts ^. #me      = CallMeAPI opts
  | otherwise        = maybe (PostStory opts) (PostStroyTo opts) (opts ^. #org)
