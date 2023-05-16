module Graphics.HEGL.Lib.Shading (
    blinnPhong
) where

import Prelude hiding (max)

import Graphics.HEGL


blinnPhong :: FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) ->
              FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) -> 
              FragExpr (Vec 3 Float) ->
              FragExpr Float -> 
              FragExpr (Vec 4 Float)
blinnPhong ka kd ks n e l pp = app color 1 where
    h = normalize (e + l)
    color = (max 0.0 (dot n l)) .# kd + 
            (pow (max 0.0 (dot n h)) pp) .# ks + 
            0.4 .# ka
