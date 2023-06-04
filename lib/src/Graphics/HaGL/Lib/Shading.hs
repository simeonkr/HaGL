module Graphics.HaGL.Lib.Shading (
    blinnPhong
) where

import Prelude hiding (max)

import Graphics.HaGL


blinnPhong :: FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) ->
              FragExpr Float -> 
              FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) ->
              FragExpr (Vec 4 Float)
blinnPhong ka kd ks pp n e l = app color 1 where
    h = normalize (e + l)
    color = max 0.0 (dot n l) .# kd + 
            (max 0.0 (dot n h) ** pp) .# ks + 
            0.4 .# ka
