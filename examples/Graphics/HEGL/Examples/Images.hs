module Graphics.HEGL.Examples.Images (
    colorGrad,
    circle,
    vstrip,

) where

import Prelude hiding (length)
import Graphics.HEGL
import Graphics.HEGL.Lib.Image


colorGrad :: GLObj
colorGrad = fromImage im where
    im pos = app pos 0

circle :: GLObj
circle = fromImage im where
    im pos = pre (cast (length pos .<= 0.5)) (vec2 0 0)

vstrip :: GLObj
vstrip = fromImage im where
    im (decon -> (x, _)) = cast (abs x .<= 0.5) .# vec3 1 1 1
