module Graphics.HEGL.Lib.Math (
    idMat,
    scale,
    translate,
    rotate,
    lookAt,
    ortho,
    perspective',
    perspective
) where

import Prelude hiding (sin, cos, tan)

import Graphics.HEGL


idMat :: _ => GLExpr d (Mat 4 4 t)
idMat = mat4x4 (vec4 1 0 0 0) (vec4 0 1 0 0) (vec4 0 0 1 0) (vec4 0 0 0 1)

scale :: _ => GLExpr d (Vec 3 t) -> GLExpr d (Mat 4 4 t)
scale (decon -> (x, y, z)) = mat4x4 (vec4 x 0 0 0) (vec4 0 y 0 0) (vec4 0 0 z 0) (vec4 0 0 0 1)

translate :: _ => GLExpr d (Vec 3 t) -> GLExpr d (Mat 4 4 t)
translate (decon -> (x, y, z)) = mat4x4 (vec4 1 0 0 0) (vec4 0 1 0 0) (vec4 0 0 1 0) (vec4 x y z 1)

rotate :: _ => GLExpr d (Vec 3 t) -> GLExpr d t -> GLExpr d (Mat 4 4 t)
rotate (decon -> (x, y, z)) theta = 
    mat4x4 (vec4 (cos theta + x * x * (1 - cos theta)) 
                 (x * y * (1 - cos theta) + z * sin theta) 
                 (x * z * (1 - cos theta) - y * sin theta)
                 0)
           (vec4 (y * x * (1 - cos theta) - z * sin theta) 
                 (cos theta + y * y * (1 - cos theta)) 
                 (y * z * (1 - cos theta) + x * sin theta) 
                 0)
                 (vec4 (z * x * (1 - cos theta) + y * sin theta) 
                 (z * y * (1 - cos theta) - x * sin theta) 
                 (cos theta + z * z * (1 - cos theta)) 
                 0)
                 (vec4 0 0 0 1)

-- the following assume a right-hand system and a z value ranging from -1 to 1
-- (equivalent to GLM_CLIP_CONTROL_RH_NO in the glm implementation)

lookAt :: _ => GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr d (Mat 4 4 t)
lookAt eye centre up = 
    let f = normalize $ centre - eye
        s = normalize $ cross f up
        u = cross s f
    in mat4x4 (app s (-(dot s eye))) 
              (app u (-(dot u eye)))
              (app f (-(dot f eye)))
              (vec4 0 0 0 1)

ortho :: _ => GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d (Mat 4 4 t)
ortho l r b t n f = 
    mat4x4 (vec4 (2 / (r - l)) 0 0 (-(r + l) / (r - l))) 
           (vec4 0 (2 / (t - b)) 0 (-(t + b) / (t - b))) 
           (vec4 0 0 (-2 / (f - n)) (-(f + n) / (f - n))) 
           (vec4 0 0 0 1)

perspective' :: _ => GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d (Mat 4 4 t)
perspective' l r b t n f =
    mat4x4 (vec4 (2*n / (r - l)) 0 ((r + l) / (r - l)) 0) 
           (vec4 0 (2*n / (t - b)) ((t + b) / (t - b)) 0) 
           (vec4 0 0 (-(f + n) / (f - n)) (-2 * f * n / (f - n))) 
           (vec4 0 0 (-1) 0)

perspective :: _ => GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d (Mat 4 4 t)
perspective fovy aspect n f =
    mat4x4 (vec4 (1 / (aspect * tan (fovy / 2))) 0 0 0) 
           (vec4 0 (1 / tan (fovy / 2)) 0 0) 
           (vec4 0 0 (-(f + n) / (f - n)) (-2 * f * n / (f - n))) 
           (vec4 0 0 (-1) 0)
