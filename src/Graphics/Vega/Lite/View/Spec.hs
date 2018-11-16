
module Graphics.Vega.Lite.View.Spec (
    ViewSpec(..)
  , viewSpecToObject
  ) where

import Data.Aeson

import Graphics.Vega.Lite.View.Facet
import Graphics.Vega.Lite.View.Layer
import Graphics.Vega.Lite.View.Repeat
import Graphics.Vega.Lite.View.Single

data ViewSpec = ViewSingle SingleView
  | ViewLayer LayerView
  | ViewFacet FacetView
  | ViewRepeat RepeatView

viewSpecToObject :: ViewSpec -> Object
viewSpecToObject (ViewSingle sv) = singleToObject sv
viewSpecToObject (ViewLayer lv) = layerToObject lv
viewSpecToObject (ViewFacet fv) = facetToObject fv
viewSpecToObject (ViewRepeat rv) = repeatToObject rv
