{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.View.Facet (
    FacetMapping(..)
  , FacetView(..)
  , facetToObject
  ) where

import Data.Maybe (maybeToList)

import Data.Aeson ((.=), Object, ToJSON(..), object)
import qualified Data.HashMap.Strict as HashMap

import Graphics.Vega.Lite.Encoding (FieldDefinition, FacetFieldDef)
import Graphics.Vega.Lite.View.Common
import Graphics.Vega.Lite.View.Layer (LayerSpec)

data FacetMapping = FacetMapping {
    facetRow :: Maybe (FieldDefinition FacetFieldDef)
  , facetColumn :: Maybe (FieldDefinition FacetFieldDef)
  }

instance ToJSON FacetMapping where
  toJSON (FacetMapping r c) = object $
    ["row" .= ri | ri <- maybeToList r] ++
    ["column" .= ci | ci <- maybeToList c]

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
