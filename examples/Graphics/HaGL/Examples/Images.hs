module Graphics.HaGL.Examples.Images (
    colorGrad,
    circle,
    vstrip,
    circlePlusStrip,
    circlePlusStrip',
    checkboard,
    rotatingCheckboard,
    invertedCheckboard,
    windingPath,
    interactiveWindingPath,
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

circle :: GLObj
circle = fromImage $ \pos ->
    cast (length pos .<= 0.5) .# vec4 1 1 1 1

vstrip :: GLObj
vstrip = fromImage $ \(decon -> (x, _)) ->
    cast (abs x .<= 0.3) .# vec4 1 1 1 1

-- images can be combined by lifting operators on them
circlePlusStrip :: GLObj
circlePlusStrip = fromImage $ liftToImage2 (+) circImage stripImage where
    circImage pos = cast (length pos .<= 0.5) .# vec4 1 1 1 1
    stripImage (decon -> (x, _)) = cast (abs x .<= 0.3) .# vec4 1 1 1 1

-- alternatively, we can draw two separate objects and blend them
circlePlusStrip' :: [GLObj]
circlePlusStrip' = [vstrip, cutCircle] where
    cutCircle = fromImage $ \pos ->
        let c = cast (length pos .<= 0.5)
        in c .# vec4 1 1 1 1 -- note the alpha component


-- Image transformations

rotate :: FragExpr Float -> ImageTransform
rotate ang pos = r .# vec2 (cos $ theta + ang) (sin $ theta + ang) where
    r = length pos
    theta = atan (y_ pos / x_ pos)

toPolar :: ImageTransform
toPolar xy@(decon -> (x, y)) = vec2 r theta where
    r = length xy
    theta = atan (y / x)

fromPolar :: ImageTransform
fromPolar (decon -> (r, theta)) = vec2 x y where
    x = r * cos theta
    y = r * sin theta

invert :: ImageTransform
invert = fromPolar . (\(decon -> (r, theta)) -> vec2 (1 / r) theta) . toPolar where


checkboardImage :: Image
checkboardImage (decon -> (x, y)) = c .# vec4 1 1 1 1 where
    c = cast $ (floor (10 * x) + floor (10 * y)) `mod` 2 .== 0

checkboard :: GLObj
checkboard = fromImage checkboardImage

rotatingCheckboard :: GLObj
rotatingCheckboard = fromImage $ checkboardImage . rotate (uniform time)

invertedCheckboard :: GLObj
invertedCheckboard = fromImage $ checkboardImage . invert


-- Use of uniforms

windingPath :: GLObj
windingPath = fromImage $ \pos ->
    let (decon -> (u, v)) = 2 * pos - 1
        t = uniform time
        c = abs (1 / (25 * sin (u + 0.2 * sin (v + 4 * t) + 1)))
    in c .# (vec4 0.2 0.7 0.5 1)

interactiveWindingPath :: GLObj
interactiveWindingPath = fromImage $ \pos ->
    let (decon -> (u, v)) = 2 * pos - 1
        dt = time - prec 0 time
        t' = prec 0 $ cond mouseLeft (t' + 10 * (mouseY - 0.5) * dt) t'
        t = uniform t'
        c = abs (1 / (25 * sin (u + 0.2 * sin (v + 4 * t) + 1)))
    in c .# (vec4 0.2 0.7 0.5 1)


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
    in rgb1 $ cond disc int 0


-- Applications of randomness/noise

randomGrid :: GLObj
randomGrid = fromImage $ \pos ->
    rgb1 $ randFloat21 pos .# 1

noiseGrid :: GLObj
noiseGrid = fromImageInteractive $ \pos ->
    rgb1 $ perlinNoise 1 (32 .# app pos (uniform time / 200)) .# 0.5 + 0.5

fractalNoiseGrid :: GLObj
fractalNoiseGrid = fromImageInteractive $ \pos ->
    rgb1 $ fbm 1 20 (app pos (uniform time / 10)) .# 0.5 + 0.5 

-- TODO: add pretty colors
warpedNoiseGrid :: GLObj
warpedNoiseGrid = fromImageInteractive $ \pos ->
    let off = vec2 
            (fbm 1 2 (app pos (uniform time / 10 + 2))) 
            (fbm 1 2 (app pos (uniform time / 10 + 3)))
    in rgb1 $ fbm 1 2 (app (pos + off) (uniform time / 10)) .# 0.5 + 0.5 

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
