
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


data ParamSurface = ParamSurface {
    uRange :: ConstExpr (Vec 2 Float),
    vRange :: ConstExpr (Vec 2 Float),
    paramFunc :: VertExpr (Vec 2 Float) -> VertExpr (Vec 3 Float)
}

-- TODO: create a Mesh from a ParamSurface

paramSurface :: ParamSurface -> GLObj
paramSurface (ParamSurface uRange@(decon -> (ul, ur)) vRange@(decon -> (vl, vr)) f) = let
    -- specify uniform grid of input vertices
    res = 100
    uv@(decon -> (u, v)) = vert $ paramRange2D uRange vRange res

    -- transform vertices according to parametric equation
    pos = f uv

    -- compute normal
    du = cnst $ (ur - ul) / res
    dv = cnst $ (vr - vl) / res
    dfdu = f (vec2 (u + du) v) - pos
    dfdv = f (vec2 u (v + dv)) - pos
    norm = normalize $ cross dfdv dfdu

    -- apply camera transformation
    initialEye = vec3 0 0 3
    view = interactiveView initialEye
    cpos = uniform (defaultProj .@ view) .@ app pos 1
    eyePos = uniform $ eyeFromView view

    -- apply lighting shader of choice
    color = defaultBlinnPhong (frag pos) (normalize $ frag norm)
                (normalize $ frag eyePos - frag pos) 

    in triangles { indices = Just $ paramInds2D (cast res), position = cpos, color = color }

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

    pos = vec3 u (f u v) v 

    initialEye = vec3 0 0 3
    cpos = uniform (defaultProj .@ translate (-initialEye)) .@ app pos 1

    du = cnst $ (ur - ul) / res
    dv = cnst $ (vr - vl) / res
    dfdu = (vec3 (u + du) (f (u + du) v) v) - pos
    dfdv = (vec3 u (f u (v + dv)) (v + dv)) - pos
    norm = normalize $ cross dfdv dfdu

    c = (normalize (frag norm) + 1) / 2
    color = app c 1

    in triangles { indices = Just $ paramInds2D (cast res), position = cpos, color = color }
    
loxodrome :: GLObj
loxodrome = let
    -- specify uniform grid of input vertices
    res = 10000
    u = vert [i / res | i <- [0..res]]

    -- transform vertices according to parametric equation
    t = 100 * (u - 0.5)  -- t ∈ [-50, 50]
    a = 0.1
    x = (1 / sqrt (1 + a * a * t * t)) .# vec3 (cos t) (-a * t) (sin t)

    -- apply camera transformation
    initialEye = vec3 0 0 3
    cpos = uniform (defaultProj .@ interactiveView initialEye) .@ app x 1

    -- use fancy colors
    red = vec3 0.8 0.2 0.2
    blue = vec3 0.2 0.2 0.8
    c = smoothstep red blue (frag u .# 1)

    -- animate time variable of the equation
    color = app c $ step (frag u) (uniform time / 5)

    in lineStrip { position = cpos, color = color }
