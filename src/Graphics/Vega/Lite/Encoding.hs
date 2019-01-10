{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.Encoding (
    FieldType(..)
  , Repeat(..)
  , Aggregate(..)
  , FieldDefinition(..)
  , fieldDef
  , fieldDefExtra
  , ChannelDefinition(..)
  , SortType(..)
  , StackType(..)
  , ScaleType(..)
  , Scale(..)
  , emptyScale
  , PositionFieldDef(..)
  , xPositionFieldDef, yPositionFieldDef
  , MarkFieldDef(..)
  , TextFieldDef(..)
  , DetailFieldDef(..)
  , OrderFieldDef(..)
  , FontWeight(..)
  , Header(..)
  , FacetFieldDef(..)
  , Encoding(..)
  , dfltEncoding
  ) where

import Data.Aeson hiding (Encoding)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Graphics.Vega.Lite.View.Common (Anchor, Baseline)
import Graphics.Vega.Lite.Transform (BinParams, TimeUnit)
import Graphics.Vega.Lite.Types

data FieldType = Quantitative | Temporal | Ordinal | Nominal

instance ToJSON FieldType where
  toJSON Quantitative = "quantitative"
  toJSON Temporal = "temporal"
  toJSON Ordinal = "ordinal"
  toJSON Nominal = "nominal"

newtype Repeat = Repeat T.Text

instance ToJSON Repeat where
  toJSON (Repeat f) = object [ "repeat" .= f ]

data Aggregate = Mean | Sum | Median | Min | Max | Count

instance ToJSON Aggregate where
  toJSON Mean = "mean"
  toJSON Sum = "sum"
  toJSON Median = "median"
  toJSON Min = "min"
  toJSON Max = "max"
  toJSON Count = "count"

data FieldDefinition a = FieldDefinition {
    field :: Either T.Text Repeat
  , fieldType :: FieldType
  , fieldBin :: Maybe BinParams
  , fieldTimeUnit :: Maybe TimeUnit
  , fieldAggregate :: Maybe Aggregate
  , fieldExtra :: Maybe a
  }

instance ToJSON a => ToJSON (FieldDefinition a) where
  toJSON = Object . fieldDefinitionToObject

fieldDef :: T.Text -> FieldType -> FieldDefinition a
fieldDef f tp = FieldDefinition {
    field = Left f
  , fieldType = tp
  , fieldBin = Nothing
  , fieldTimeUnit = Nothing
  , fieldAggregate = Nothing
  , fieldExtra = Nothing
  }

fieldDefExtra :: T.Text -> FieldType -> a -> FieldDefinition a
fieldDefExtra f tp extra = (fieldDef f tp) { fieldExtra = Just extra }

fieldDefinitionToObject :: ToJSON a => FieldDefinition a -> Object
fieldDefinitionToObject FieldDefinition{..} =
    maybe id HashMap.union encExtra
  . maybe id (HashMap.insert "timeUnit" . toJSON) fieldTimeUnit
  . maybe id (HashMap.insert "aggregrate" . toJSON) fieldAggregate
  . maybe id (HashMap.insert "bin" . toJSON) fieldBin
  $ HashMap.fromList [
      "field" .= either toJSON toJSON field
    , "type" .= fieldType
    ]
  where
    encExtra = case toJSON fieldExtra of
      Object obj -> Just obj
      _ -> Nothing

data ChannelDefinition a = FieldDef (FieldDefinition a) | ValueDef Value | NoChannel

instance ToJSON a => ToJSON (ChannelDefinition a) where
  toJSON (FieldDef fdef) = toJSON fdef
  toJSON (ValueDef v) = object [ "value" .= v ]
  toJSON NoChannel = Null

data SortType = Ascending | Descending | NoSort

instance ToJSON SortType where
  toJSON Ascending = "ascending"
  toJSON Descending = "descending"
  toJSON NoSort = Null

data Axis = Axis {
    axisDomain :: Bool
  , axisGrid :: Bool
  , axisOrient :: Orientation
  , axisOffset :: Int
  , axisPosition :: Int
  , axisZIndex :: Int
  }

instance ToJSON Axis where
  toJSON Axis{..} = object [
      "domain" .= axisDomain
    , "grid" .= axisGrid
    , "orient" .= axisOrient
    , "offset" .= axisOffset
    , "position" .= axisPosition
    , "zindex" .= axisZIndex
    ]

emptyAxis :: Orientation -> Axis
emptyAxis o = Axis {
    axisDomain = True
  , axisGrid = True
  , axisOrient = o
  , axisOffset = 0
  , axisPosition = 0
  , axisZIndex = 0
  }

data StackType = Zero | Normalize | Center | NoStack

instance ToJSON StackType where
  toJSON Zero = "zero"
  toJSON Normalize = "normalize"
  toJSON Center = "center"
  toJSON NoStack = Null

data ScaleType = LinearScale | Pow | Sqrt | Log | Time | Utc | Sequential
  | Band | PointScale
  | BinLinear | BinOrdinal

instance ToJSON ScaleType where
  toJSON LinearScale = "linear"
  toJSON Pow = "pow"
  toJSON Sqrt = "sqrt"
  toJSON Log = "log"
  toJSON Time = "time"
  toJSON Utc = "utc"
  toJSON Sequential = "sequential"
  toJSON Band = "band"
  toJSON PointScale = "point"
  toJSON BinLinear = "bin-linear"
  toJSON BinOrdinal = "bin-ordinal"

data Scale = Scale {
    scaleType :: ScaleType
  , scaleNice :: Bool -- in general more complexe, use bool for now, expand to seperate type
  , scalePadding :: Double
  , scaleRound :: Bool
  , scaleZero :: Bool
  }

instance ToJSON Scale where
  toJSON Scale{..} = object [
      "type" .= scaleType
    , "nice" .= scaleNice
    , "padding" .= scalePadding
    , "round" .= scaleRound
    , "zero" .= scaleZero
    ]

emptyScale :: Scale
emptyScale = Scale LinearScale True 0 True True

data PositionFieldDef = PositionFieldDef {
    positionScale :: Scale
  , positionAxis :: Axis
  , positionSort :: SortType
  , positionStack :: StackType
  }

instance ToJSON PositionFieldDef where
  toJSON PositionFieldDef{..} = object [
      "scale" .= positionScale
    , "axis" .= positionAxis
    , "sort" .= positionSort
    , "stack" .= positionStack
    ]

xPositionFieldDef,yPositionFieldDef :: PositionFieldDef
xPositionFieldDef = PositionFieldDef {
    positionScale = emptyScale
  , positionAxis = emptyAxis OrientBottom
  , positionSort = Ascending
  , positionStack = NoStack
  }
yPositionFieldDef = xPositionFieldDef { positionAxis = emptyAxis OrientLeft }

data LegendType = Symbol | Gradient

instance ToJSON LegendType where
  toJSON Symbol = "symbol"
  toJSON Gradient = "gradient"

data Legend = Legend {
    legendEntryPadding :: Int
  , legendFillColor :: T.Text
  , legendFormat :: T.Text
  , legendOffset :: Int
  , legendOrient :: Orientation
  , legendPadding :: Int
  , legendType :: LegendType
  , legendValues :: [Value]
  , legendZIndex :: Int
  }

instance ToJSON Legend where
  toJSON Legend{..} = object [
      "entryPadding" .= legendEntryPadding
    , "fillColor" .= legendFillColor
    , "format" .= legendFormat
    , "offset" .= legendOffset
    , "orient" .= legendOrient
    , "padding" .= legendPadding
    , "type" .= legendType
    , "values" .= legendValues
    , "zindex" .= legendZIndex
    ]

data MarkFieldDef = MarkFieldDef {
    markScale :: Scale
  , markLegend :: Legend
  -- , markCondition :: Condition
  }

instance ToJSON MarkFieldDef where
  toJSON MarkFieldDef{..} = object [
      "scale" .= markScale
    , "legend" .= markLegend
    ]

newtype TextFieldDef = TextFieldDef {
    textFormat :: T.Text
  -- , textCondition :: Condition
  }

instance ToJSON TextFieldDef where
  toJSON TextFieldDef{..} = object [
      "format" .= textFormat
    ]

newtype DetailFieldDef = DetailFieldDef [FieldDefinition ()]

instance ToJSON DetailFieldDef where
  toJSON (DetailFieldDef d) = object [ "detail" .= d ]

newtype OrderFieldDef = OrderFieldDef SortType

instance ToJSON OrderFieldDef where
  toJSON (OrderFieldDef s) = object [ "sort" .= s ]

data FontWeight = FontWeightValue Double
  | FontWeightThin
  | FontWeightExtraLight
  | FontWeightLight
  | FontWeightNormal
  | FontWeightMedium
  | FontWeightSemiBold
  | FontWeightBold
  | FontWeightExtraBold
  | FontWeightBlack

instance ToJSON FontWeight where
  toJSON (FontWeightValue n) = toJSON n
  toJSON FontWeightThin = "thin"
  toJSON FontWeightExtraLight = "extra light"
  toJSON FontWeightLight = "light"
  toJSON FontWeightNormal = "normal"
  toJSON FontWeightMedium = "medium"
  toJSON FontWeightSemiBold = "semi bold"
  toJSON FontWeightBold = "bold"
  toJSON FontWeightExtraBold = "extra bold"
  toJSON FontWeightBlack = "black"

data Header = Header {
    headerFormat :: T.Text
  , headerTitle :: Maybe T.Text
  , headerLabelAngle :: Int
  , headerLabelColor :: T.Text
  , headerLabelFont :: T.Text
  , headerLabelFontSize :: Int
  , headerLabelLimit :: Int
  , headerLabelOffset :: Int
  , headerTitleAnchor :: Anchor
  , headerTitleAngle :: Int
  , headerTitleBaseline :: Baseline
  , headerTitleColor :: T.Text
  , headerTitleFont :: T.Text
  , headerTitleFontWeight :: FontWeight
  , headerTitleFontSize :: Int
  , headerTitleLimit :: Int
  , headerTitleOffset :: Int
  }

instance ToJSON Header where
  toJSON Header{..} = object [
      "format" .= headerFormat
    , "title" .= headerTitle
    , "labelAngle" .= headerLabelAngle
    , "labelColor" .= headerLabelColor
    , "labelFont" .= headerLabelFont
    , "labelFontSize" .= headerLabelFontSize
    , "labelLimit" .= headerLabelLimit
    , "labelOffset" .= headerLabelOffset
    , "titleAnchor" .= headerTitleAnchor
    , "titleAngle" .= headerTitleAngle
    , "titleBaseline" .= headerTitleBaseline
    , "titleColor" .= headerTitleColor
    , "titleFont" .= headerTitleFont
    , "titleFontWeight" .= headerTitleFontWeight
    , "titleFontSize" .= headerTitleFontSize
    , "titleLimit" .= headerTitleLimit
    , "titleOffset" .= headerTitleOffset
    ]

newtype FacetFieldDef = FacetFieldDef Header

instance ToJSON FacetFieldDef where
  toJSON (FacetFieldDef hdr) = object [ "header" .= hdr ]

data Encoding = Encoding {
    encodeX             :: ChannelDefinition PositionFieldDef
  , encodeY             :: ChannelDefinition PositionFieldDef
  , encodeX2            :: ChannelDefinition () -- No position field def since x2 shares with x and y2 for y
  , encodeY2            :: ChannelDefinition ()
  , encodeColor         :: ChannelDefinition MarkFieldDef
  , encodeFill          :: ChannelDefinition MarkFieldDef
  , encodeStroke        :: ChannelDefinition MarkFieldDef
  , encodeOpacity       :: ChannelDefinition MarkFieldDef
  , encodeFillOpacity   :: ChannelDefinition MarkFieldDef
  , encodeStrokeOpacity :: ChannelDefinition MarkFieldDef
  , encodeSize          :: ChannelDefinition MarkFieldDef
  , encodeShape         :: ChannelDefinition MarkFieldDef
  , encodeStrokeWidth   :: ChannelDefinition MarkFieldDef
  , encodeText          :: ChannelDefinition TextFieldDef
  , encodeTooltip       :: ChannelDefinition TextFieldDef
  , encodeDetail        :: ChannelDefinition DetailFieldDef
  , encodeOrder         :: ChannelDefinition OrderFieldDef
  , encodeRow           :: ChannelDefinition FacetFieldDef
  , encodeColumn        :: ChannelDefinition FacetFieldDef
  }

instance ToJSON Encoding where
  toJSON Encoding{..} = Object
    . addChannel "x" encodeX
    . addChannel "y" encodeY
    . addChannel "x2" encodeX2
    . addChannel "y2" encodeY2
    . addChannel "color" encodeColor
    . addChannel "fill" encodeFill
    . addChannel "stroke" encodeStroke
    . addChannel "opacity" encodeOpacity
    . addChannel "fillOpacity" encodeFillOpacity
    . addChannel "strokeOpacity" encodeStrokeOpacity
    . addChannel "size" encodeSize
    . addChannel "shape" encodeShape
    . addChannel "strokeWidth" encodeStrokeWidth
    . addChannel "text" encodeText
    . addChannel "tooltip" encodeTooltip
    . addChannel "detail" encodeDetail
    . addChannel "order" encodeOrder
    . addChannel "row" encodeRow
    . addChannel "column" encodeColumn
    $ HashMap.empty
    where
      addChannel :: ToJSON a => T.Text -> ChannelDefinition a -> Object -> Object
      addChannel _ NoChannel = id
      addChannel x chan = HashMap.insert x (toJSON chan)

dfltEncoding :: Encoding
dfltEncoding = Encoding {
    encodeX             = NoChannel
  , encodeY             = NoChannel
  , encodeX2            = NoChannel
  , encodeY2            = NoChannel
  , encodeColor         = NoChannel
  , encodeFill          = NoChannel
  , encodeStroke        = NoChannel
  , encodeOpacity       = NoChannel
  , encodeFillOpacity   = NoChannel
  , encodeStrokeOpacity = NoChannel
  , encodeSize          = NoChannel
  , encodeShape         = NoChannel
  , encodeStrokeWidth   = NoChannel
  , encodeText          = NoChannel
  , encodeTooltip       = NoChannel
  , encodeDetail        = NoChannel
  , encodeOrder         = NoChannel
  , encodeRow           = NoChannel
  , encodeColumn        = NoChannel
  }
