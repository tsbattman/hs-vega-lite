
module Graphics.Vega.Lite (
    module Lite
  , encodeVegaLite
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LB

import Graphics.Vega.Lite.Data as Lite
import Graphics.Vega.Lite.Encoding as Lite
import Graphics.Vega.Lite.Mark as Lite
import Graphics.Vega.Lite.View as Lite
import Graphics.Vega.Lite.Types as Lite

encodeVegaLite :: TopLevel -> LB.ByteString
encodeVegaLite = encode
