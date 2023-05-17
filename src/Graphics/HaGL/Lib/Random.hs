module Graphics.HaGL.Lib.Random (
    randFloat21,
    randDir,
    perlinNoise,
    perlinNoise2d,
    fbm,
    turbulence
) where

import Prelude hiding (sin, cos, min, max, const, floor)

import Graphics.HaGL


-- https://github.com/patriciogonzalezvivo/lygia/blob/main/generative/random.glsl
randFloat21 :: GLExpr d (Vec 2 Float) -> GLExpr d Float
randFloat21 seed = fract $ (sin $ dot seed (vec2 12.9898 78.233)) * 43758.5453123

randDir :: GLExpr d (Vec 2 Float) -> GLExpr d (Vec 3 Float)
randDir seed =
    let s = 2 * 3.14 * randFloat21 seed
        t = 2 * 3.14 * randFloat21 (vec2 1 (randFloat21 seed))
    in vec3 (cos s * sin t) (sin s * sin t) (cos t)

-- http://burtleburtle.net/bob/hash/integer.html
hash :: GLExpr d Int -> GLExpr d Int
hash = glFunc1 hash' where
    hash' a = cast a6 where
        a0 = cast a :: GLExpr _ UInt
        a1 = (a0 + 0x7ed55d16) + (a0 .<< 12)
        a2 = (a1 .^ 0xc761c23c) .^ (a1 .>> 19)
        a3 = (a2 + 0x165667b1) + (a2 .<< 5)
        a4 = (a3 + 0xd3a2646c) .^ (a3 .<< 9)
        a5 = (a4 + 0xfd7046c5) + (a4 .<< 3)
        a6 = (a5 .^ 0xb55a4f09) .^ (a5 .>> 16)

perlinNoise :: GLExpr d Int -> GLExpr d (Vec 3 Float) -> GLExpr d Float
perlinNoise = glFunc2 perlinNoise' where
    perlinNoise' :: GLExpr d Int -> GLExpr d (Vec 3 Float) -> GLExpr d Float
    perlinNoise' seed xyz = x where
        (decon -> (ix, iy, iz)) = matCast $ floor xyz
        (decon -> (dx, dy, dz)) = fract xyz
        (decon -> (fx, fy, fz)) = fade (fract xyz) where
            fade t = t * t * t * (t * (6 * t - 15) + 10)

        x = mix y0 y1 fx

        y0 = mix z00 z01 fy
        y1 = mix z10 z11 fy

        z00 = mix w000 w001 fz
        z01 = mix w010 w011 fz
        z10 = mix w100 w101 fz
        z11 = mix w110 w111 fz

        w000 = grad seed (vec3 ix iy iz) (vec3 dx dy dz)
        w001 = grad seed (vec3 ix iy (iz + 1)) (vec3 dx dy (dz - 1))
        w010 = grad seed (vec3 ix (iy + 1) iz) (vec3 dx (dy - 1) dz)
        w011 = grad seed (vec3 ix (iy + 1) (iz + 1)) (vec3 dx (dy - 1) (dz - 1))
        w100 = grad seed (vec3 (ix + 1) iy iz) (vec3 (dx - 1) dy dz)
        w101 = grad seed (vec3 (ix + 1) iy (iz + 1)) (vec3 (dx - 1) dy (dz - 1))
        w110 = grad seed (vec3 (ix + 1) (iy + 1) iz) (vec3 (dx - 1) (dy - 1) dz)
        w111 = grad seed (vec3 (ix + 1) (iy + 1) (iz + 1)) (vec3 (dx - 1) (dy - 1) (dz - 1))

        grad = glFunc3 grad'
        grad' seed (decon -> (ix, iy, iz)) (decon -> (dx, dy, dz)) =
            let h = hash (hash (hash ix + iy + seed) + iz) in
                cond ((h .& 15) .== 0)  ( dx + dy) $
                cond ((h .& 15) .== 1)  (-dx + dy) $
                cond ((h .& 15) .== 2)  ( dx - dy) $
                cond ((h .& 15) .== 3)  (-dx - dy) $
                cond ((h .& 15) .== 4)  ( dx + dz) $
                cond ((h .& 15) .== 5)  (-dx + dz) $
                cond ((h .& 15) .== 6)  ( dx - dz) $
                cond ((h .& 15) .== 7)  (-dx - dz) $
                cond ((h .& 15) .== 8)  ( dy + dz) $
                cond ((h .& 15) .== 9)  (-dy + dz) $
                cond ((h .& 15) .== 10) ( dy - dz) $
                cond ((h .& 15) .== 11) (-dy - dz) $
                cond ((h .& 15) .== 12) ( dx + dy) $
                cond ((h .& 15) .== 13) (-dx + dy) $
                cond ((h .& 15) .== 14) (-dy + dz) $
                cond ((h .& 15) .== 15) (-dy - dz) 0

perlinNoise2d :: GLExpr d Int -> GLExpr d (Vec 2 Float) -> GLExpr d Float
perlinNoise2d seed xy = perlinNoise seed (app xy 0)

fbm :: GLExpr d Int -> GLExpr d Int -> GLExpr d (Vec 3 Float) -> GLExpr d Float
fbm seed numOctaves xy = f 0 0 1 1 where
    f = glFunc4 $ \i t a k -> cond (i .== numOctaves) t $
        f (i + 1) (t + a * perlinNoise seed (k .# xy)) (0.5 * a) (2 * k)

turbulence :: GLExpr d Int -> GLExpr d Int -> GLExpr d (Vec 3 Float) -> GLExpr d Float
turbulence seed numOctaves xy = f 0 0 1 1 where
    f = glFunc4 $ \i t a k -> cond (i .== numOctaves) t $
        f (i + 1) (t + a * abs (perlinNoise seed (k .# xy))) (0.5 * a) (2 * k)
