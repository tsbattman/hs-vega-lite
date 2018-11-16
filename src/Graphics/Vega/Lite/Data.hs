{-# LANGUAGE OverloadedStrings #-}

module Graphics.Vega.Lite.Data (
    DataFormatType(..)
  , DataFormat(..)
  , DataSource(..)
  ) where

import Data.Aeson
import qualified Data.Text as T

data DataFormatType = Json T.Text | Csv | Tsv | TopoJson T.Text T.Text

data DataFormat = DataFormat DataFormatType (Maybe Object)

instance ToJSON DataFormat where
  toJSON (DataFormat (Json property) obj) = object [ "type" .= T.pack "json", "property" .= property, "parse" .= obj ]
  toJSON (DataFormat Csv obj)= object [ "type" .= T.pack "csv", "parse" .= obj ]
  toJSON (DataFormat Tsv obj)= object [ "type" .= T.pack "tsv", "parse" .= obj ]
  toJSON (DataFormat (TopoJson f m) obj)= object [ "type" .= T.pack "topojson", "feature" .= f, "mesh" .= m, "parse" .= obj ]

data DataSource = Inline Array DataFormat
  | Url T.Text DataFormat
  | Named T.Text DataFormat

instance ToJSON DataSource where
  toJSON (Inline array fmt) = object [ "values" .= array, "format" .= fmt ]
  toJSON (Url url fmt) = object [ "url" .= url, "format" .= fmt ]
  toJSON (Named name fmt) = object [ "name" .= name, "format" .= fmt ]
