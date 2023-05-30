module Graphics.HaGL.Examples.Common (
    rotatingView,
    defaultProj,
    defaultBlinnPhong
) where

import Prelude hiding (sin, cos)

import Graphics.HaGL
import Graphics.HaGL.Lib.Math
import Graphics.HaGL.Lib.Shading (blinnPhong)


rotatingView :: _ => HostExpr (Vec 3 Float) -> HostExpr (Vec 3 Float) -> 
    HostExpr (Mat 4 4 Float)
rotatingView axis initialEye = 
    translate (-initialEye) .@ rotate (normalize axis) time

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
