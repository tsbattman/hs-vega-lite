{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.TopLevel (
    ViewPadding(..)
  , padding
  , AutoSize(..)
  , AutoSizeParams(..)
  , dfltAutoSizeParams
  , TopLevel(..)
  , dfltTopLevel
  ) where

import Data.Monoid ((<>))

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Graphics.Vega.Lite.Schema
import Graphics.Vega.Lite.View.Spec

data ViewPadding = ViewPadding {
    padLeft :: Int
  , padTop :: Int
  , padRight :: Int
  , padBottom :: Int
  }

instance ToJSON ViewPadding where
  toJSON ViewPadding{..} = object [
      "left" .= padLeft
    , "top" .= padTop
    , "right" .= padRight
    , "bottom" .= padBottom
    ]

padding :: Int -> ViewPadding
padding p = ViewPadding p p p p

data AutoSize = SizeNone | SizePad | SizeFit
  deriving (Eq, Show, Read)

instance ToJSON AutoSize where
  toJSON SizeNone = "none"
  toJSON SizePad = "pad"
  toJSON SizeFit = "fit"

data ContainsType = Content | Padding
  deriving (Eq, Show, Read)

instance ToJSON ContainsType where
  toJSON Content = "content"
  toJSON Padding = "padding"

data AutoSizeParams = AutoSizeParams {
    autoSizeType :: AutoSize
  , autoSizeResize :: Bool
  , autoSizeContains :: ContainsType
  }

instance ToJSON AutoSizeParams where
  toJSON AutoSizeParams{..} = object [
      "type" .= autoSizeType
    , "resize" .= autoSizeResize
    , "contains" .= autoSizeContains
    ]

dfltAutoSizeParams :: AutoSizeParams
dfltAutoSizeParams = AutoSizeParams {
    autoSizeType = SizePad
  , autoSizeResize = False
  , autoSizeContains = Content
  }

data TopLevel = TopLevel {
    vegaSchema :: T.Text
  , viewBackground :: Maybe T.Text
  , viewPadding :: Either Int ViewPadding
  , viewAutoSize :: Either AutoSize AutoSizeParams
  -- , viewConfig :: Config
  , viewSpec :: ViewSpec
  }

instance ToJSON TopLevel where
  toJSON TopLevel{..} =  Object $ HashMap.fromList [
      "$schema" .= vegaSchema
    , "background" .= viewBackground
    , "padding" .= either toJSON toJSON viewPadding
    , "autosize" .= either toJSON toJSON viewAutoSize
    ] <> viewSpecToObject viewSpec

dfltTopLevel :: ViewSpec -> TopLevel
dfltTopLevel spec = TopLevel {
    vegaSchema = v2Schema
  , viewBackground = Nothing
  , viewPadding = Left 5
  , viewAutoSize = Left SizePad
  , viewSpec = spec
  }
