module Graphics.HEGL.Examples (
    helloTriangles,
    module Graphics.HEGL.Examples.Images,
    module Graphics.HEGL.Examples.Particles,
    module Graphics.HEGL.Examples.Simulations
) where

import Graphics.HEGL
import Graphics.HEGL.Examples.Images
import Graphics.HEGL.Examples.Particles
import Graphics.HEGL.Examples.Simulations


helloTriangles :: GLObj
helloTriangles = let 
    pos = vert 
        [(vec4 (-0.9) (-0.9) 0 1), 
         (vec4 0.85 (-0.9) 0 1), 
         (vec4 (-0.9) 0.85 0 1), 
         (vec4 0.9 (-0.85) 0 1), 
         (vec4 0.9 0.9 0 1), 
         (vec4 (-0.85) 0.9 0 1)]
    color = vec4 0 0 1 1
    in triangles { position = pos, color = color }
