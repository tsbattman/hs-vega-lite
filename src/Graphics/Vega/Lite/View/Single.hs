{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.Single (
    SingleView(..)
  , dfltSingleView
  , singleToObject
  ) where

import Data.Aeson hiding (Encoding)
import qualified Data.HashMap.Strict as HashMap

import Graphics.Vega.Lite.Encoding
import Graphics.Vega.Lite.Mark
import Graphics.Vega.Lite.View.Common

data SingleView = SingleView {
    viewCommon :: Maybe CommonView
  , viewWidth :: Maybe Int
  , viewHeight :: Maybe Int
  -- , viewSelection :: Selection
  , viewMark :: Mark
  , viewEncoding :: Encoding
  }

dfltSingleView :: SingleView
dfltSingleView = SingleView Nothing Nothing Nothing dfltMark dfltEncoding

singleToObject :: SingleView -> Object
singleToObject SingleView{..} =
    maybe id (HashMap.union . commonToObject) viewCommon
  . maybe id (HashMap.insert "width" . toJSON) viewWidth
  . maybe id (HashMap.insert "height" . toJSON) viewHeight
  $ HashMap.fromList [
    -- , "selection" .= viewSelection
      "mark" .= viewMark
    , "encoding" .= viewEncoding
    ]
