module Graphics.HEGL.Examples.Common (
    defaultProj,
    defaultBlinnPhong
) where

import Prelude hiding (sin, cos)

import Graphics.HEGL
import Graphics.HEGL.Lib.Math
import Graphics.HEGL.Lib.Shading (blinnPhong)


defaultProj :: GLExpr d (Mat 4 4 Float)
defaultProj = perspective (pi / 6) 1 1 10

defaultBlinnPhong :: FragExpr (Vec 3 Float) -> 
                     FragExpr (Vec 3 Float) -> 
                     FragExpr (Vec 3 Float) -> 
                     FragExpr (Vec 4 Float)
defaultBlinnPhong p n e = blinnPhong ka kd ks n e l pp where
    kd = vec3 0.8 0.6 0.6
    ks = vec3 1 1 1
    ka = vec3 0.2 0.8 0.5
    pp = 1000
    l = normalize $ (xyRotLight 5) - p
    t = uniform time
    xyRotLight off = vec3 (off * cos t) 0 (off * sin t)
