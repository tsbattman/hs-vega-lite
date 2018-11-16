{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.Facet (
    FacetMapping(..)
  , FacetView(..)
  , facetToObject
  ) where

import Data.Aeson ((.=), Object, ToJSON(..), object)
import qualified Data.HashMap.Strict as HashMap

import Graphics.Vega.Lite.Encoding (FieldDefinition, FacetFieldDef)
import Graphics.Vega.Lite.View.Common
import Graphics.Vega.Lite.View.Layer (LayerSpec)

data FacetMapping = FacetMapping {
    facetRow :: FieldDefinition FacetFieldDef
  , facetColumn :: FieldDefinition FacetFieldDef
  }

instance ToJSON FacetMapping where
  toJSON (FacetMapping r c) = object [
      "row" .= r
    , "column" .= c
    ]

data FacetView = FacetView {
    facetCommon :: Maybe CommonView
  , facetMapping :: FacetMapping
  , facetSpec :: LayerSpec
  }

facetToObject :: FacetView -> Object
facetToObject FacetView{..} =
    maybe id (HashMap.union . commonToObject) facetCommon
  $ HashMap.fromList [
      "facet" .= facetMapping
    , "spec" .= facetSpec
    ]
