module Graphics.HEGL.Examples (
    helloTriangles,
    interactiveCube,
    interactiveMesh,
    module Graphics.HEGL.Examples.Images,
    module Graphics.HEGL.Examples.Particles,
    module Graphics.HEGL.Examples.Simulations,
    module Graphics.HEGL.Examples.Manifolds,
    module Graphics.HEGL.Examples.Spheres
) where

import Graphics.HEGL
import Graphics.HEGL.Lib.Camera
import Graphics.HEGL.Lib.Mesh
import Graphics.HEGL.Lib.Objects3D
import Graphics.HEGL.Examples.Common
import Graphics.HEGL.Examples.Images
import Graphics.HEGL.Examples.Particles
import Graphics.HEGL.Examples.Simulations
import Graphics.HEGL.Examples.Manifolds
import Graphics.HEGL.Examples.Spheres


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
