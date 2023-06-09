module Graphics.HaGL.Presentation where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (max, sin, cos, length, abs, sqrt)

import Graphics.HaGL hiding (drawGlut)
import qualified Graphics.HaGL.Lib as Lib
import qualified Graphics.HaGL.Examples as Examples

drawGlut obj = drawGlutCustom 
    defaultGlutOptions { 
        winSize = (980, 980), 
        winPosition = Just (980, 0),
        clearCol = (1, 1, 1, 1) } 
    obj


-- Basics

redBlueTriangles :: GLObj
redBlueTriangles = let 
    -- Vertex "processing"
    pos :: VertExpr (Vec 4 Float)
    pos = vert 
         -- triangle 1
        [(vec4 (-0.9) (-0.9) 0 1), 
         (vec4 0.85 (-0.9) 0 1), 
         (vec4 (-0.9) 0.85 0 1), 
         -- triangle 2
         (vec4 0.9 (-0.85) 0 1), 
         (vec4 0.9 0.9 0 1), 
         (vec4 (-0.85) 0.9 0 1)]

    -- Fragment processing
    fpos :: FragExpr (Vec 4 Float)
    fpos = frag pos
    mixFactor :: FragExpr Float
    mixFactor = 0.5 * (x_ fpos + 1)

    red, blue, color :: FragExpr (Vec 4 Float)
    red = vec4 1 0 0 1
    blue = vec4 0 0 1 1
    color = mix red blue (mixFactor .# vec4 1 1 1 1)

    -- Object creation
    in triangles { position = pos, color = color }


-- Images

type ImagePos = FragExpr (Vec 2 Float)
type ImageColor = FragExpr (Vec 4 Float)
type Image = ImagePos -> ImageColor

fromImage :: Image -> GLObj
fromImage im = 
    let pos = vert 
            [vec4 (-1) (-1) 0 1, 
             vec4 (-1) 1 0 1, 
             vec4 1 (-1) 0 1, 
             vec4 1 1 0 1]
        fpos = frag (xy_ pos)
    in triangleStrip { position = pos, color = im fpos }

blueCircle :: GLObj
blueCircle = fromImage $ \pos ->
    cast (length pos .<= 0.5) .# vec4 0 0 1 1

blueOffsetCircle :: GLObj
blueOffsetCircle = fromImage $ \pos ->
    let offPos = pos - vec2 0.5 0.5
    in cast (length offPos .<= 0.25) .# vec4 0 0 1 1

redOffsetAnimatedCircle :: GLObj
redOffsetAnimatedCircle = fromImage $ \pos ->
    let t = uniform time
        offPos = pos - 0.5 .* vec2 (cos t) (-sin t)
    in cast (length offPos .<= 0.25) .# vec4 1 0 0 1

lotsOfCircles :: GLObj
lotsOfCircles = fromImage $ \pos ->
    let circleAt :: ImagePos -> ImageColor -> ImageColor
        circleAt off col = cast (length (pos - off) .<= 0.1) .# col

        circle :: FragExpr Float -> ImageColor
        circle i = circleAt off col where
            t = uniform time
            angle = 2 * pi * i + t
            --angle = 2 * pi * i + t * i
            off = 0.8 .* vec2 (cos angle) (-sin angle)
            mixFactor = cond (i .< 0.5) i (1 - i)
            col = mix (vec4 1 0 0 1) (vec4 0 0 1 1) (mixFactor .# 1)
        
        circles :: [ImageColor]
        circles = [circle (cnst i / 20) | i <- [0..19]]
    in sum circles


-- Noise

noiseGrid :: GLObj
noiseGrid = Lib.fromImageInteractive $ \pos ->
    let xyz = app pos (uniform time / 200)
        nv = Lib.perlinNoise 1 (32 .# xyz) .# 0.5 + 0.5
    in app nv 1

procgen2DWorld :: GLObj
procgen2DWorld = Lib.fromImageInteractive $ \pos ->
    let perlin amp freq = amp * Lib.perlinNoise2D 1 (freq .* pos)
        tot = perlin 4 4 + perlin 2 8 + perlin 1 32
        rgb1 rgb = app rgb 1
    in rgb1 $ cond (tot .<= 0)  (vec3 0 0 1) $
              cond (tot .< 0.5) (vec3 1 1 0) $
                                (vec3 0 1 0)


-- Curves

loxodrome :: GLObj
loxodrome = let
    -- specify uniform range of input vertices
    res = 10000
    u, t :: VertExpr Float
    u = vert [i / res | i <- [0..res]]
    t = 100 * (u - 0.5)  -- t ∈ [-50, 50]

    -- transform vertices according to parametric equation
    a = 0.1
    pos :: VertExpr (Vec 3 Float)
    pos = (1 / sqrt (1 + a * a * t * t)) .# vec3 (cos t) (-a * t) (sin t)

    -- apply camera transformation
    view, proj :: HostExpr (Mat 4 4 Float)
    view = Lib.rotatingView (vec3 1 1 1) (vec3 0 0 3)
    proj = Lib.perspective (pi / 6) 1 1 10
    cpos :: VertExpr (Vec 4 Float)
    cpos = uniform (proj .@ view) .@ app pos 1

    -- use color gradient
    red = vec3 0.8 0.2 0.2
    blue = vec3 0.2 0.2 0.8
    c = smoothstep red blue (frag u .# 1)

    -- animate time variable of the equation
    color :: FragExpr (Vec 4 Float)
    color = app c $ step (frag u) (uniform time / 5)

    in lineStrip { position = cpos, color = color }


-- Particles

explosion :: GLObj
explosion = 
    let s = vert [vec2 i j | i <- [-30..30], j <- [-30..30]]
        speed = Lib.randFloat21 s ** 2
        pos = (uniform time * speed / 10) .# Lib.randDir s
        cpos = uniform (Lib.interactiveView $ vec3 0 0 1) .@ app pos 1
        col = mix (vec4 1 0 0 1) (vec4 1 1 0 1) (frag speed .# 1)
    in points { position = cpos, color = col }


-- 3D Objects

blinnPhong ka kd ks pp n e l = app color 1 where
    h = normalize (e + l)
    color = max 0.0 (dot n l) .# kd + 
            (max 0.0 (dot n h) ** pp) .# ks + 
            0.4 .# ka

drawBunny :: IO ()
drawBunny = do
    mesh <- Lib.loadMesh "res/bunny.obj"
    drawGlut $ Examples.shadedInteractiveMesh mesh

-- Examples.earthlike


-- Fractals

mand' :: FragExpr (Vec 2 Float) -> FragExpr (Vec 2 Float) -> FragExpr Float -> FragExpr Float
mand' pos0@(decon -> (x0, y0)) (decon -> (x, y)) n =
    cond (n .== 0 .|| ((x * x + y * y) .> 4)) n $
        mand pos0 (vec2 (x * x - y * y + x0) (2 * x * y + y0)) (n - 1)

mand = glFunc3 mand'

mandelbrot :: GLObj
mandelbrot = Lib.fromImageInteractive $ \pos ->
    app (mand pos pos 50 .# 0.02) 1


-- Simulations

-- Examples.doublePendulum
