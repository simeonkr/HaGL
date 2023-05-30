module Graphics.HaGL.Examples (
    blueTriangles,
    redBlueTriangles,
    interactiveCube,
    interactiveMesh,
    module Graphics.HaGL.Examples.Images,
    module Graphics.HaGL.Examples.Particles,
    module Graphics.HaGL.Examples.Simulations,
    module Graphics.HaGL.Examples.Manifolds,
    module Graphics.HaGL.Examples.Spheres,
    exampleList
) where

import Data.Map

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


blueTriangles :: GLObj
blueTriangles = let 
    pos = vert 
        [(vec4 (-0.9) (-0.9) 0 1), 
         (vec4 0.85 (-0.9) 0 1), 
         (vec4 (-0.9) 0.85 0 1), 
         (vec4 0.9 (-0.85) 0 1), 
         (vec4 0.9 0.9 0 1), 
         (vec4 (-0.85) 0.9 0 1)]
    blue = vec4 0 0 1 1
    in triangles { position = pos, color = blue }

redBlueTriangles :: GLObj
redBlueTriangles = let 
    pos = vert 
        [(vec4 (-0.9) (-0.9) 0 1), 
         (vec4 0.85 (-0.9) 0 1), 
         (vec4 (-0.9) 0.85 0 1), 
         (vec4 0.9 (-0.85) 0 1), 
         (vec4 0.9 0.9 0 1), 
         (vec4 (-0.85) 0.9 0 1)]
    red = vec4 1 0 0 1
    blue = vec4 0 0 1 1
    s = 0.5 * (x_ (frag pos) + 1)
    color = mix red blue (s .# 1)
    in triangles { position = pos, color = color }

cube :: (GLExpr HostDomain (Mat 4 4 Float)) -> GLObj
cube view = let
    pos = vert 
        [vec4 1 1 1 1,
         vec4 (-1) 1 1 1,
         vec4 (-1) (-1) 1 1, 
         vec4 1 (-1) 1 1,
         vec4 1 (-1) (-1) 1, 
         vec4 1 1 (-1) 1, 
         vec4 (-1) 1 (-1) 1, 
         vec4 (-1) (-1) (-1) 1]
    cpos = uniform (defaultProj .@ view) .@ pos
    color = vec4 1 0 0 1
    faces = [0,1,2, 0,2,3, 0,3,4, 0,4,5, 0,5,6, 0,6,1,
             1,6,7, 1,7,2, 7,4,3, 7,3,2, 4,7,6, 4,6,5]
    in triangles { indices = Just faces, position = cpos, color = color }

rotatingCube :: GLObj
rotatingCube = cube (rotatingView (vec3 1 1 1) (vec3 0 0 5))

interactiveCube :: GLObj
interactiveCube = cube (interactiveView (vec3 0 0 5))

interactiveMesh :: Mesh -> GLObj
interactiveMesh mesh = let
    pos = vert $ meshVertices mesh
    norm = vert $ meshNormals mesh

    -- Apply camera transformation
    initialEye = vec3 0 0 10
    view = interactiveView initialEye
    cpos = uniform (defaultProj .@ view) .@ app pos 1
    
    -- Apply lighting shader of choice
    eyePos = uniform $ xyz_ $ col2 $ inverse view
    color = defaultBlinnPhong (frag pos) (normalize $ frag norm)
                              (normalize $ frag eyePos - frag pos) 

    in triangles { indices = Just $ meshFaces mesh, position = cpos, color = color }


exampleList :: [(String, [GLObj])]
exampleList =
    [("blue_triangle", [blueTriangles]),
     ("red_blue_triangle", [redBlueTriangles]),
     ("color_grad", [colorGrad]),
     ("blue_circle", [blueCircle]),
     ("blue_plus_red_circle", [bluePlusRedCircle]),
     ("blue_over_red_circle", blueOverRedCircle),
     ("rotated_checkboard", [rotatedCheckboard (pi / 4)]),
     ("rotating_checkboard", [rotatedCheckboard (uniform time)]),
     ("inverted_checkboard", [invertedCheckboard]),
     ("winding_paths", [windingPaths]),
     ("interactive_winding_paths", [interactiveWindingPaths]),
     ("frag_sphere", [fragSphere]),
     ("random_grid", [randomGrid]),
     ("noise_grid", [noiseGrid]),
     ("fractal_noise_grid", [fractalNoiseGrid]),
     ("warped_noise_grid", [warpedNoiseGrid]),
     ("procgen_2d_world", [procgen2DWorld]),
     ("mandelbrot", [mandelbrot]),
     ("particles", [particles]),
     ("particles2", [particles2]),
     ("pendulum", [pendulum]),
     ("double_pendulum", doublePendulum),
     ("rotating_cube", [rotatingCube]),
     ("interactive_cube", [interactiveCube]),
     ("param_sphere", [paramSphere]),
     ("param_torus", [paramTorus 1.5 1]),
     ("loxodrome", [loxodrome]),
     ("checkered_sphere", [checkeredSphere]),
     ("earth-like", [earthlike])
    ]
