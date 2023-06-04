module Graphics.HaGL.Examples.Spheres (
    checkeredSphere,
    shadedSphere,
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
    ((Mesh verts _ inds), (decon -> (u,v))) = uvSphere 32 1
    pos = vert verts

    view = interactiveView (vec3 0 0 3)
    cpos = uniform (defaultProj .@ view) .@ app pos 1

    c = (floor u + floor v) `mod` 2 .# 1

    in triangles { 
        indices = Just inds, 
        position = cpos, 
        color = app c 1 }

shadedSphere :: GLObj
shadedSphere = let
    (mesh, _) = uvSphere 32 1

    view = interactiveView (vec3 0 0 3)
    eyePos = uniform $ eyeFromView view

    colorMap fpos fnorm = 
        defaultBlinnPhong fpos fnorm (normalize $ frag eyePos - fpos) 
    
    in meshDrawable defaultProj view colorMap mesh

-- TODO: realistic water
earthlike :: GLObj
earthlike = let
    ((Mesh verts norms inds), _) = uvSphere 32 1
    pos = vert verts
    norm = vert norms

    view = interactiveView (vec3 0 0 3)
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

    eyePos = uniform $ eyeFromView view
    eyeDir = normalize $ frag eyePos - dispPos fpos

    color = blinnPhong ka kd ks pp dispNorm eyeDir l

    in triangles { indices = Just inds, position = cpos, color = color }

