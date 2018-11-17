{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (div, head, id)
import Data.Aeson
import Graphics.Vega.Lite
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 as H hiding (a, b, map, main, object)
import Text.Blaze.Html5.Attributes as A hiding (title, style)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

data Sample = Sample Char Int

instance ToJSON Sample where
  toJSON (Sample a b) = object [
      "a" .= a
    , "b" .= b
    ]

sampleData :: DataSource
sampleData = Inline dat (DataFormat (Json "") Nothing)
  where
    dat = V.fromList . map toJSON $ [
        Sample 'C' 2
      , Sample 'C' 7
      , Sample 'C' 4
      , Sample 'D' 1
      , Sample 'D' 2
      , Sample 'D' 6
      , Sample 'E' 2
      , Sample 'E' 2
      , Sample 'E' 2
      ]

barChart :: TopLevel
barChart = dfltTopLevel . ViewSingle $ SingleView {
    viewCommon = Just CommonView {
        viewName = "sample 1"
      , viewDescription = "vega lite sample"
      , viewTitle = Left "sample 1"
      , viewData = sampleData
      }
  , viewWidth = Nothing
  , viewHeight = Nothing
  , viewMark = dfltMark { markType = Bar }
  , viewEncoding = dfltEncoding {
        encodeY = FieldDef $ FieldDefinition (Left "a") Nominal Nothing Nothing Nothing Nothing
      , encodeX = FieldDef $ FieldDefinition (Left "b") Quantitative Nothing Nothing (Just Mean) Nothing
      }
  }

vegaPage :: Html
vegaPage = docTypeHtml $ do
  head $ do
    title "Vega Lite Bar Chart"
    meta ! charset "utf-8"
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/vega/3.0.1/vega.js" $ pure ()
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/vega-lite/2.0.0-beta.15/vega-lite.js" $ pure ()
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/vega-embed/3.0.0-beta.20/vega-embed.js" $ pure ()
    style ! media "screen" $ ".vega-actions a { margin-right: 5px; }"
  body $ do
    h1 "Template for Embedding Vega-Lite Visualization"
    div ! id "vis" $ ""
    script . toHtml $ unlines [
        "// Assign the specification to a local variable vlSpec."
      , "var vlSpec = " ++ (T.unpack . T.decodeUtf8 . B8.toStrict $ encode barChart)
      , "// Embed the visualization in the container with id `vis`"
      , "vega.embed(\"#vis\", vlSpec);"
      ]

main :: IO ()
main = putStrLn $ renderHtml vegaPage
