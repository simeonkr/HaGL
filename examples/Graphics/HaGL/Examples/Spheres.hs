module Graphics.HaGL.Examples.Spheres (
    checkeredSphere,
    earthlike,
) where

import Prelude hiding (sin, cos, floor, mod)

import Graphics.HaGL
import Graphics.HaGL.Lib.Camera
import Graphics.HaGL.Lib.Math
import Graphics.HaGL.Lib.Mesh
import Graphics.HaGL.Lib.Objects3D (uvSphere)
import Graphics.HaGL.Lib.Random
import Graphics.HaGL.Lib.Shading
import Graphics.HaGL.Examples.Common


checkeredSphere :: GLObj
checkeredSphere = let
    ((Mesh verts norms inds), uvGrid) = uvSphere 1
    pos = vert verts
    (decon -> (u,v)) = frag $ vert uvGrid
    cpos = uniform (defaultProj .@ interactiveView (vec3 0 0 7)) .@ app pos 1
    c = cast $ (floor u + floor v) `mod` 2 .== 0
    in triangles { indices = Just inds, position = cpos, color = app (c .# 1) 1 }

-- TODO: realistic water
earthlike :: GLObj
earthlike = let
    ((Mesh verts norms inds), _) = uvSphere 1
    pos = vert verts
    norm = vert norms

    view = interactiveView (vec3 0 0 7)
    cpos = uniform (defaultProj .@ view) .@ app pos 1

    fpos = frag pos
    fnorm@(decon -> (nx, ny, nz)) = normalize $ frag norm

    dispFactor p = smoothstep 0 1 (fbm 1 6 (2 .* p) - 0.1)
    dispPos p = p + 0.1 * dispFactor p .# normalize p
    eps = 0.001
    du = normalize $ vec3 1 1 ((-nx - ny) / nz) 
    dv = normalize $ cross fnorm du
    dispNorm = normalize $ 
        cross ((dispPos (fpos + eps * du) - dispPos fpos) / eps) 
              ((dispPos (fpos + eps * dv) - dispPos fpos) / eps)

    kd = cond (dispFactor fpos .== 0) (vec3 0 0.2 0.8) (vec3 0.3 0.8 0.1)
    ks = vec3 1 1 1
    ka = kd
    pp = 1000
    l = normalize $ xyRotLight 5 - dispPos fpos
    xyRotLight off = let t = uniform time in vec3 (off * cos t) 0 (off * sin t)

    eyePos = uniform $ xyz_ $ col2 $ inverse view
    eyeDir = normalize $ frag eyePos - dispPos fpos

    color = blinnPhong ka kd ks dispNorm eyeDir l pp

    in triangles { indices = Just inds, position = cpos, color = color }

