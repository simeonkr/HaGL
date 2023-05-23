
module Graphics.HaGL.Examples.Manifolds (
    paramSphere,
    paramTorus,
    paramPlot,
    loxodrome
) where

import Prelude hiding (sin, cos, sqrt)

import Graphics.HaGL
import Graphics.HaGL.Lib.Math
import Graphics.HaGL.Lib.Camera
import Graphics.HaGL.Lib.Objects3D
import Graphics.HaGL.Examples.Common


-- TODO: create a Mesh from a ParamSurface

paramSurface :: ParamSurface -> GLObj
paramSurface (ParamSurface uRange@(decon -> (ul, ur)) vRange@(decon -> (vl, vr)) f) = let
    -- Specify uniform grid of input vertices
    res = 100
    uv@(decon -> (u, v)) = vert $ paramRange2D uRange vRange res

    -- Transform vertices according to parametric equation
    vpos = f uv

    -- Compute normal
    du = cnst $ (ur - ul) / res
    dv = cnst $ (vr - vl) / res
    dfdu = f (vec2 (u + du) v) - vpos
    dfdv = f (vec2 u (v + dv)) - vpos
    norm = normalize $ cross dfdv dfdu

    -- Apply camera transformation
    eyePos = vec3 0 0.5 5
    pos = uniform (defaultProj .@ interactiveView eyePos) .@ app vpos 1

    -- Apply lighting shader of choice
    color = defaultBlinnPhong (frag vpos) (normalize $ frag norm)
                (normalize $ frag (uniform eyePos) - frag vpos) 

    in triangles { indices = Just $ paramInds2D (cast res), position = pos, color = color }

data ParamSurface = ParamSurface {
    uRange :: ConstExpr (Vec 2 Float),
    vRange :: ConstExpr (Vec 2 Float),
    paramFunc :: VertExpr (Vec 2 Float) -> VertExpr (Vec 3 Float)
}

paramSphere :: GLObj
paramSphere = paramSurface (ParamSurface (vec2 0 (2 * pi)) (vec2 0 pi) f) where
    f (decon -> (u, v)) = 
        vec3 (cos u * sin v) (sin u * sin v) (cos v)

paramTorus :: VertExpr Float -> VertExpr Float -> GLObj
paramTorus c a = paramSurface (ParamSurface (vec2 0 (2 * pi)) (vec2 0 (2 * pi)) f) where
    f (decon -> (u, v)) =
        vec3 ((c + a * cos v) * cos u) (a * sin v) ((c + a * cos v) * sin u)

paramPlot :: ConstExpr (Vec 2 Float) -> ConstExpr (Vec 2 Float) ->
    (VertExpr Float -> VertExpr Float -> VertExpr Float) -> GLObj
paramPlot uRange@(decon -> (ul, ur)) vRange@(decon -> (vl, vr)) f = let
    res = 100
    uv@(decon -> (u, v)) = vert $ paramRange2D uRange vRange res

    vpos = vec3 u (f u v) v 

    eyePos = vec3 0 0.5 5
    pos = uniform (defaultProj .@ translate (-eyePos)) .@ app vpos 1

    du = cnst $ (ur - ul) / res
    dv = cnst $ (vr - vl) / res
    dfdu = (vec3 (u + du) (f (u + du) v) v) - vpos
    dfdv = (vec3 u (f u (v + dv)) (v + dv)) - vpos
    norm = normalize $ cross dfdv dfdu

    c = (normalize (frag norm) + 1) / 2
    color = app c 1

    in triangles { indices = Just $ paramInds2D (cast res), position = pos, color = color }
    
loxodrome :: GLObj
loxodrome = let
    -- Specify uniform grid of input vertices
    res = 10000
    t = vert $ [i / res | i <- [0..res]]

    -- Transform vertices according to parametric equation
    u = 100 * (t - 0.5)
    a = 0.1
    x = (0.7 / sqrt (1 + a * a * u * u)) .# vec3 (cos u) (-a * u) (sin u)

    -- Apply camera transformation
    eyePos = vec3 0 0.5 5
    pos = uniform (defaultProj .@ interactiveView eyePos) .@ app x 1

    -- Use fancy colors
    r = frag t
    red = vec3 0.8 0.2 0.2
    cyan = vec3 0.2 0.7 0.5
    c = smoothstep red cyan (r .# 1)

    -- Animate time variable of the equation
    color = app (step r (uniform time / 5) .# c) 1

    in lineStrip { position = pos, color = color }
