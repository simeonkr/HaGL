module Graphics.HaGL.Examples (
    helloTriangles,
    interactiveCube,
    interactiveMesh,
    module Graphics.HaGL.Examples.Images,
    module Graphics.HaGL.Examples.Particles,
    module Graphics.HaGL.Examples.Simulations,
    module Graphics.HaGL.Examples.Manifolds,
    module Graphics.HaGL.Examples.Spheres
) where

import Graphics.HaGL
import Graphics.HaGL.Lib.Camera
import Graphics.HaGL.Lib.Mesh
import Graphics.HaGL.Lib.Objects3D
import Graphics.HaGL.Examples.Common
import Graphics.HaGL.Examples.Images
import Graphics.HaGL.Examples.Particles
import Graphics.HaGL.Examples.Simulations
import Graphics.HaGL.Examples.Manifolds
import Graphics.HaGL.Examples.Spheres


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

interactiveCube :: GLObj
interactiveCube = let
    pos = uniform (defaultProj .@ interactiveView (vec3 0 0 7)) .@ box
    color = vec4 1 0 0 1
    inds = [0,1,2, 0,2,3, 0,3,4, 0,4,5, 0,5,6, 0,6,1,
            1,6,7, 1,7,2, 7,4,3, 7,3,2, 4,7,6, 4,6,5]
    in triangles { indices = Just inds, position = pos, color = color }

interactiveMesh :: Mesh -> GLObj
interactiveMesh mesh = let
    pos0 = vert $ meshVertices mesh
    norm = vert $ meshNormals mesh

    -- Apply camera transformation
    initialEye = vec3 0 0 10
    view = interactiveView initialEye
    pos = uniform (defaultProj .@ view) .@ app pos0 1
    
    -- Apply lighting shader of choice
    eyePos = uniform $ xyz_ $ col2 $ inverse view
    color = defaultBlinnPhong (frag pos0) (normalize $ frag norm)
                              (normalize $ frag eyePos - frag pos0) 

    in triangles { indices = Just $ meshFaces mesh, position = pos, color = color }
