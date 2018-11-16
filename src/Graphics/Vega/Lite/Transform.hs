{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vega.Lite.Transform (
    BinParams(..)
  , TimeUnit(..)
  , Bin(..)
  -- , Transform(..)
  ) where

import Data.Aeson
import qualified Data.Text as T

data BinParams = BinParams {
    binsMax :: Int
  , binBase :: Int
  , binStep :: Int
  , binSteps :: [Int]
  , binMinStep :: Int
  , binDivide :: [Int]
  , binExtent :: (Int, Int)
  , binNice :: Bool
  }

instance ToJSON BinParams where
  toJSON BinParams{..} = object [
      "max" .= binsMax
    , "base" .= binBase
    , "step" .= binStep
    , "steps" .= binSteps
    , "minstep" .= binMinStep
    , "divide" .= binDivide
    , "extent" .= binExtent
    , "nice" .= binNice
    ]

data Bin = Bin {
    binParams :: BinParams
  , binField :: T.Text
  , binAs :: T.Text
  }


data TimeUnit = Year
  | YearQuarter
  | YearQuartermonth
  | YearMonth
  | YearMonthDate
  | YearMonthDateHours
  | YearMonthDateHoursMinutes
  | YearMonthDateHoursSeconds
  | Quarter
  | QuarterMonth
  | Month
  | MonthDate
  | Date
  | Day
  | Hours
  | HoursMinutes
  | HoursMinutesSeconds
  | Minutes
  | MinutesSeconds
  | Seconds
  | SecondsMilliseconds
  | Milliseconds

instance ToJSON TimeUnit where
  toJSON Year = "year"
  toJSON YearQuarter = "year-quarter"
  toJSON YearQuartermonth = "year-quarter-month"
  toJSON YearMonth = "year-month"
  toJSON YearMonthDate = "year-month-date"
  toJSON YearMonthDateHours = "year-month-date-hours"
  toJSON YearMonthDateHoursMinutes = "year-month-date-hours-minutes"
  toJSON YearMonthDateHoursSeconds = "year-month-date-hours-seconds"
  toJSON Quarter = "quarter"
  toJSON QuarterMonth = "quarter-month"
  toJSON Month = "month"
  toJSON MonthDate = "month-date"
  toJSON Date = "date"
  toJSON Day = "day"
  toJSON Hours = "hours"
  toJSON HoursMinutes = "hours-minutes"
  toJSON HoursMinutesSeconds = "hours-minutes-seconds"
  toJSON Minutes = "minutes"
  toJSON MinutesSeconds = "minutes-seconds"
  toJSON Seconds = "seconds"
  toJSON SecondsMilliseconds = "seconds-milliseconds"
  toJSON Milliseconds = "milliseconds"
