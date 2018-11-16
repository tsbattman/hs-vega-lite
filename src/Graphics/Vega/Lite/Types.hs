{-# LANGUAGE OverloadedStrings #-}

module Graphics.Vega.Lite.Types (
    Orientation(..)
  ) where

import Data.Aeson

data Orientation = OrientLeft | OrientTop | OrientRight | OrientBottom

instance ToJSON Orientation where
  toJSON OrientLeft = "left"
  toJSON OrientTop = "top"
  toJSON OrientRight = "right"
  toJSON OrientBottom = "bottom"
