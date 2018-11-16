{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.Mark (
    MarkType(..)
  , InterpolateMethod(..)
  , Mark(..)
  , dfltMark
  ) where

import Data.Aeson
import qualified Data.Text as T

data MarkType = Area | Bar | Circle | Line | Point | Rect | Rule | Square | Text | Tick

instance ToJSON MarkType where
  toJSON Area = "area"
  toJSON Bar = "bar"
  toJSON Circle = "circle"
  toJSON Line = "line"
  toJSON Point = "point"
  toJSON Rect = "rect"
  toJSON Rule = "rule"
  toJSON Square = "square"
  toJSON Text = "text"
  toJSON Tick = "tick"

data InterpolateMethod = Linear | LinearClosed
  | Step     | StepBefore   | StepAfter
  | Basis    | BasisOpen    | BasisClosed
  | Cardinal | CardinalOpen | CardinalClosed
  | Bundle   | Monotone

instance ToJSON InterpolateMethod where
  toJSON Linear = "linear"
  toJSON LinearClosed = "linear-closed"
  toJSON Step = "step"
  toJSON StepBefore = "step-before"
  toJSON StepAfter = "step-after"
  toJSON Basis = "basis"
  toJSON BasisOpen = "basis-open"
  toJSON BasisClosed = "basis-closed"
  toJSON Cardinal = "cardinal"
  toJSON CardinalOpen = "cardinal-open"
  toJSON CardinalClosed = "cardinal-closed"
  toJSON Bundle = "bundle"
  toJSON Monotone = "monotone"


data Mark = Mark {
    markType :: MarkType
  , markStyle :: T.Text
  , markClip :: Bool
  , markFilled :: Bool
  -- , markOrient :: T.Text
  , markInterpolate :: InterpolateMethod
  , markTension :: Double
  }

instance ToJSON Mark where
  toJSON Mark{..} = object [
      "type" .= markType
    , "style" .= markStyle
    , "clip" .= markClip
    , "filled" .= markFilled
    -- , "orient" .= markOrient
    , "interpolate" .= markInterpolate
    , "tension" .= markTension
    ]

dfltMark :: Mark
dfltMark = Mark {
    markType = Point
  , markStyle = ""
  , markClip = False
  , markFilled = False
  -- , "orient" .= markOrient
  , markInterpolate = Linear
  , markTension = 1
  }
