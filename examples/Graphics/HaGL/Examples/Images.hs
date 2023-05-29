module Graphics.HaGL.Examples.Images (
    colorGrad,
    blueCircle,
    bluePlusRedCircle,
    blueOverRedCircle,
    checkboard,
    rotatedCheckboard,
    invertedCheckboard,
    windingPaths,
    interactiveWindingPaths,
    fragSphere,
    randomGrid,
    noiseGrid,
    fractalNoiseGrid,
    warpedNoiseGrid,
    procgen2DWorld,
    mandelbrot
) where

import Prelude hiding (length, min, max, abs, floor, mod, sin, cos, tan, atan)
import Graphics.HaGL
import Graphics.HaGL.Lib.Image
import Graphics.HaGL.Lib.Random


-- Simple images and their combinations

colorGrad :: GLObj
colorGrad = fromImage $ \pos -> pos $- vec2 0 1

blueCircle :: GLObj
blueCircle = fromImage $ \pos ->
    cast (length pos .<= 0.5) .# vec4 0 0 1 1

bluePlusRedCircle :: GLObj
bluePlusRedCircle = fromImage $ liftToImage2 (+) blueCircIm redCircIm where
    blueCircIm pos = cast (length (pos + vec2 0.25 0) .<= 0.5) .# vec4 0 0 1 1
    redCircIm pos = cast (length (pos - vec2 0.25 0) .<= 0.5) .# vec4 1 0 0 1

blueOverRedCircle :: [GLObj]
blueOverRedCircle = [redCircle, blueCircle] where
    blueCircle = circle (vec2 (-0.25) 0) (vec4 0 0 1 1)
    redCircle = circle (vec2 0.25 0) (vec4 1 0 0 1)
    circle center color = fromImage $ \pos ->
        cast (length (pos - center) .<= 0.5) .# color


-- Image transformations

rotate :: FragExpr Float -> ImageTransform
rotate ang pos@(decon -> (x, y)) = 
    let r = length pos
        theta = atan (y / x)
    in r .# vec2 (cos $ theta + ang) (sin $ theta + ang)

invert :: ImageTransform
invert = fromPolar . (\(decon -> (r, theta)) -> vec2 (1 / r) theta) . toPolar where
    toPolar :: ImageTransform
    toPolar xy@(decon -> (x, y)) = vec2 r theta where
        r = length xy
        theta = atan (y / x)
    fromPolar :: ImageTransform
    fromPolar (decon -> (r, theta)) = vec2 x y where
        x = r * cos theta
        y = r * sin theta

checkboardImage :: Image
checkboardImage (decon -> (x, y)) = c .# vec4 0 0 0 1 where
    c = cast $ (floor (10 * x) + floor (10 * y)) `mod` 2 .== 0

checkboard :: GLObj
checkboard = fromImage checkboardImage

rotatedCheckboard :: FragExpr Float -> GLObj
rotatedCheckboard angle = fromImage $ checkboardImage . (rotate angle)

invertedCheckboard :: GLObj
invertedCheckboard = fromImage $ checkboardImage . invert


-- Use of uniforms

mkWindingPaths :: FragExpr Float -> GLObj
mkWindingPaths t = fromImage $ \(decon -> (x, y)) ->
    let curve x t = 0.2 * sin (x + 4 * t)
        distPlot scale y' = 
            smoothstep (y' - 0.05) y' y -
            smoothstep y' (y' + 0.05) y
        greenish = vec4 0.1 0.7 0.3 1 
        redish = vec4 0.7 0.1 0.1 1 
        bluish = vec4 0.1 0.1 0.7 1
    in distPlot 150 (curve x t) .# greenish +
       distPlot 250 (curve x (2 * t + 0.5)) .# redish +
       distPlot 600 (curve x (0.5 * t - 0.5)) .# bluish

windingPaths :: GLObj
windingPaths = mkWindingPaths $ uniform time

interactiveWindingPaths :: GLObj
interactiveWindingPaths = 
    let dt = time - prec 0 time
        t = prec 0 $ cond mouseLeft (t + 10 * (-mouseX + 0.5) * dt) t
    in mkWindingPaths $ uniform t


-- Simulating 3D objects

fragSphere :: GLObj
fragSphere = fromImage $ \pos@(decon -> (x, y)) -> 
    let r = 0.7
        disc = length pos .<= r
        color = vec3 1 0.4 0.3
        norm = normalize $ vec3 x y (r - length pos)
        dir = normalize $ vec3 1 1 1
        diffuse = max 0 (dot dir norm)
        specular = diffuse ** 50
        int = max (diffuse .# color) (specular .# (vec3 1 1 1))
    in rgb1 $ cond disc int 1


-- Applications of randomness/noise

randomGrid :: GLObj
randomGrid = fromImage $ \pos ->
    rgb1 $ randFloat21 pos .# 1

noiseGrid :: GLObj
noiseGrid = fromImageInteractive $ \pos ->
    let xyz = app pos (uniform time / 200)
        nv = perlinNoise 1 (32 .# xyz) .# 0.5 + 0.5
    in rgb1 nv

fractalNoiseGrid :: GLObj
fractalNoiseGrid = fromImageInteractive $ \pos ->
    rgb1 $ fbm 1 20 (app pos (uniform time / 10)) .# 0.5 + 0.5 

warpedNoiseGrid :: GLObj
warpedNoiseGrid = fromImageInteractive $ \pos ->
    let off = vec2 
            (fbm 1 2 (app pos (uniform time / 10 + 2))) 
            (fbm 1 2 (app pos (uniform time / 10 + 3)))
        c = rgb1 $ fbm 1 2 (app (pos + off) (uniform time / 10)) .# 0.5 + 0.5
    in mix (vec4 0.1 0.3 0.3 1) (vec4 1 1 1 1) c

procgen2DWorld :: GLObj
procgen2DWorld = fromImageInteractive $ \pos ->
    let perlin amp freq = amp * perlinNoise2D 1 (freq .* pos)
        tot = perlin 4 4 + perlin 2 8 + perlin 1 32
    in rgb1 $ cond (tot .<= 0)  (vec3 0 0 1) $
              cond (tot .< 0.5) (vec3 1 1 0) $
                                (vec3 0 1 0)


-- Fractals

mandelbrot :: GLObj
mandelbrot = fromImageInteractive $ \pos ->
    let mand = glFunc3 $ \pos0@(decon -> (x0, y0)) (decon -> (x, y)) i -> 
            cond (i .> 50 .|| ((x * x + y * y) .> 4)) i $
                mand pos0 (vec2 (x * x - y * y + x0) (2 * x * y + y0)) (i + 1)
    in mand pos pos 0 .# 0.02  -- TODO: use better color map
