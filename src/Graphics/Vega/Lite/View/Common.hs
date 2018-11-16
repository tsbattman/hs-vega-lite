{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.Common (
    Baseline(..)
  , Anchor(..)
  , Title(..)
  , CommonView(..)
  , commonToObject
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Graphics.Vega.Lite.Data
import Graphics.Vega.Lite.Types

data Baseline = BaselineTop | BaselineMiddle | BaselineBottom

instance ToJSON Baseline where
  toJSON BaselineTop = "top"
  toJSON BaselineMiddle = "middle"
  toJSON BaselineBottom = "bottom"

data Anchor = AnchorStart | AnchorMiddle | AnchorEnd

instance ToJSON Anchor where
  toJSON AnchorStart = "start"
  toJSON AnchorMiddle = "middle"
  toJSON AnchorEnd = "end"

data Title = Title {
    titleText :: T.Text
  , titleAnchor :: Anchor
  , titleOffset :: Int
  , titleOrient :: Orientation
  , titleStyle :: [T.Text]
  }

instance ToJSON Title where
  toJSON Title{..} = object [
      "name" .= titleText
    , "anchor" .= titleAnchor
    , "offset" .=  titleOffset
    , "orient" .= titleOrient
    , "style" .= titleStyle
    ]

data CommonView = CommonView {
    viewName :: T.Text
  , viewDescription :: T.Text
  , viewTitle :: Either T.Text Title
  , viewData :: DataSource
  -- , viewTransform :: [Transform]
  }

commonToObject :: CommonView -> Object
commonToObject v = HashMap.fromList [
    "name" .= viewName v
  , "description" .= viewDescription v
  , "title" .= either toJSON toJSON (viewTitle v)
  , "data" .= viewData v
  ]
