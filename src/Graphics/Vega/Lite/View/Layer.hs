{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.Layer (
    LayerSpec(..)
  , LayerView(..)
  , consLayer
  , snocLayer
  , prependLayers
  , appendLayers
  , layerToObject
  ) where

import Data.Aeson hiding (Encoding)
import qualified Data.HashMap.Strict as HashMap

import Graphics.Vega.Lite.Encoding
import Graphics.Vega.Lite.View.Common
import Graphics.Vega.Lite.View.Single

data LayerSpec = LayerUpon LayerView | LayerSingle SingleView

instance ToJSON LayerSpec where
  toJSON (LayerUpon lv) = Object $ layerToObject lv
  toJSON (LayerSingle sv) = Object $ singleToObject sv

data LayerView = LayerView {
    layerCommon :: Maybe CommonView
  , layerSpecs :: [LayerSpec]
  , layerWidth :: Maybe Int
  , layerHeight :: Maybe Int
  , layerEncoding :: Encoding
  -- , layerProjectio :: Projection
  -- , layerResolve :: Resolve
  }

consLayer :: LayerSpec -> LayerView -> LayerView
consLayer ls lv = lv { layerSpecs = ls:layerSpecs lv }

snocLayer :: LayerView -> LayerSpec -> LayerView
snocLayer lv ls = lv { layerSpecs = layerSpecs lv  ++ [ls] }

prependLayers :: [LayerSpec] -> LayerView -> LayerView
prependLayers ls lv = lv { layerSpecs = ls ++ layerSpecs lv }

appendLayers :: LayerView -> [LayerSpec] -> LayerView
appendLayers lv ls = lv { layerSpecs = layerSpecs lv  ++ ls }

layerToObject :: LayerView -> Object
layerToObject LayerView{..} =
  maybe id (HashMap.union . commonToObject) layerCommon
  $ HashMap.fromList [
      "layer" .= layerSpecs
    , "width" .= layerWidth
    , "height" .= layerHeight
    , "encoding" .= layerEncoding
    ]
