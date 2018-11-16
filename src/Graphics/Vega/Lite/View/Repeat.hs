{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.Repeat (
    RepeatDef(..)
  , Align(..)
  , Bounds(..)
  , RowCol(..)
  , Center
  , Spacing
  , RepeatView(..)
  , dfltRepeat
  , repeatToObject
  ) where

import Data.Aeson ((.=), Object, ToJSON(..), object)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Graphics.Vega.Lite.View.Common
import Graphics.Vega.Lite.View.Layer (LayerSpec)

data RepeatDef = RepeatDef {
    repeatColumn :: [T.Text]
  , repeatRow :: [T.Text]
  }

instance ToJSON RepeatDef where
  toJSON (RepeatDef c r) = object $
       (if null c then [] else ["column" .= c])
    ++ (if null r then [] else ["row" .= r])

data Align = AlignNone | AlignEach | AlignAll

instance ToJSON Align where
  toJSON AlignNone = "none"
  toJSON AlignEach = "each"
  toJSON AlignAll = "all"

data Bounds = Full | Flush

instance ToJSON Bounds where
  toJSON Full = "full"
  toJSON Flush = "flush"

data RowCol a = Both a | RowCol a a

instance ToJSON a => ToJSON (RowCol a) where
  toJSON (Both b) = toJSON b
  toJSON (RowCol r c) = object [ "row" .= r, "column" .= c ]

type Center = RowCol Bool
type Spacing = RowCol Double

data RepeatView = RepeatView {
    repeatCommon :: Maybe CommonView
  , repeatDef :: RepeatDef
  , repeatSpec :: LayerSpec
  , repeatAlign :: Align
  , repeatBounds :: Bounds
  , repeatCenter :: Center
  , repeatSpacing :: Spacing
  -- , repeatResolve :: Resolve
  }

dfltRepeat :: RepeatDef -> LayerSpec -> RepeatView
dfltRepeat rd ls = RepeatView {
    repeatCommon = Nothing
  , repeatDef = rd
  , repeatSpec = ls
  , repeatAlign = AlignAll
  , repeatBounds = Full
  , repeatCenter = Both False
  , repeatSpacing = Both 10
  -- , repeatResolve :: Resolve
  }

repeatToObject :: RepeatView -> Object
repeatToObject RepeatView{..} =
    maybe id (HashMap.union . commonToObject) repeatCommon
  $ HashMap.fromList [
      "repeat" .= repeatDef
    , "spec" .= repeatSpec
    , "align" .= repeatAlign
    , "bounds" .= repeatBounds
    , "center" .= repeatCenter
    , "spacing" .= repeatSpacing
    ]
