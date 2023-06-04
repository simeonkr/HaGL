module Graphics.HaGL.Examples.Particles (
    explosion
) where

import Prelude hiding (sin, cos, sqrt, max)

import Graphics.HaGL
import Graphics.HaGL.Lib.Camera
import Graphics.HaGL.Lib.Random
import Graphics.HaGL.Examples.Common


explosion :: GLObj
explosion = 
    let s = vert [vec2 i j | i <- [-30..30], j <- [-30..30]]
        speed = randFloat21 s ** 2
        pos = (uniform time * speed / 10) .# randDir s
        cpos = uniform (interactiveView $ vec3 0 0 1) .@ app pos 1
        col = mix (vec4 1 0 0 1) (vec4 1 1 0 1) (frag speed .# 1)
    in points { position = cpos, color = col }
