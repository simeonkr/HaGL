module Graphics.HEGL.Examples.Particles (
    particles,
    particles2
) where

import Prelude hiding (sin, cos, sqrt, max, const)

import Graphics.HEGL
import Graphics.HEGL.Lib.Camera
import Graphics.HEGL.Lib.Random


particles :: GLObj
particles =
    let vel = vert [vec3 i j k | i <- [-20..20], j <- [-20..20], k <- [-20..20]]
        p = (uniform time / 10) .# normalize vel
        pos = uniform (rotatingView $ vec3 0 0 1) .@ app p 1
    in points { position = pos, color = 1 }

particles2 :: GLObj
particles2 = 
    let s = vert [vec2 i j | i <- [-30..30], j <- [-30..30]]
        speed = pow (randFloat21 s) 2
        p = (uniform time * speed / 10) .# randDir s
        pos = uniform (rotatingView $ vec3 0 0 1) .@ app p 1
    in points { position = pos, color = 1 }