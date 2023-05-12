module Graphics.HEGL.Examples.Simulations (
    pendulum,
    doublePendulum
    --sphericalPendulum
) where

import Prelude hiding (sin, cos, atan, sqrt, mod, max, const, length, floor)

import Graphics.HEGL
import Graphics.HEGL.Lib.Image
import Graphics.HEGL.Lib.Math (perspective)
import Graphics.HEGL.Lib.Camera (interactiveView)


pendulum :: GLObj
pendulum = fromImage $ \pos ->
    let damping = 0.9999
        dt = time - prec 0 time
        theta = prec (pi / 3) $ theta + theta' * dt
        theta' = prec 0 $ damping * theta' - 10 * sin theta * dt
        x = uniform $ vec2 (sin theta / 2) (- cos theta / 2)
        circleAt pos off r col = cast (length (pos - off) .<= r) .# col
    in rgb1 $ circleAt pos (vec2 0 0) 0.01 (vec3 1 1 1) 
        + circleAt pos (0.25 * x) 0.01 (vec3 0 1 1) 
        + circleAt pos (0.50 * x) 0.01 (vec3 0 1 1) 
        + circleAt pos (0.75 * x) 0.01 (vec3 0 1 1)
        + circleAt pos x 0.04 (vec3 1 0 0) 

doublePendSim :: (GLExpr d (Vec 2 Float), GLExpr d (Vec 2 Float))
doublePendSim = (x1, x2) where
    dt = time - prec 0 time
    theta1 = prec (pi / 3) $ theta1 + theta1' * dt
    theta2 = prec (pi / 3) $ theta2 + theta2' * dt
    theta1' = prec 0 $ theta1' + theta1'' * dt
    theta2' = prec 0 $ theta2' + theta2'' * dt
    theta1'' = prec 0 $ (-2 * sin theta1 - sin (theta1 - 2 * theta2) - 
        2 * sin (theta1 - theta2) * (theta2' * theta2' + 
        theta1' * theta1' * cos (theta1 - theta2))) / 
        (3 - cos (2 * theta1 - theta2))
    theta2'' = prec 0 $ (2 * sin (theta1 - theta2) * 
        (2 * theta1' * theta1' + 2 * cos theta1 + theta2'
         * theta2' * cos (theta1 - theta2))) /
        (3 - cos (2 * theta1 - theta2))
    x1 = uniform $ vec2 (sin theta1 / 3) (- cos theta1 / 3)
    x2 = uniform $ vec2 (sin theta1 / 3) (- cos theta1 / 3) +
        vec2 (sin theta2 / 3) (- cos theta2 / 3)

doublePendulum :: [GLObj]
doublePendulum = [circles, path] where
    (x1, x2) = doublePendSim
    circles = fromImage $ \pos ->
        let circleAt pos off r col = cast (length (pos - off) .<= r) .# col
        in rgb1 $ circleAt pos (vec2 0 0) 0.01 (vec3 1 1 1)
            + circleAt pos x1 0.04 (vec3 1 0 0)
            + circleAt pos (0.5 * x1) 0.01 (vec3 0 1 1) 
            + circleAt pos x2 0.04 (vec3 1 0 0)
            + circleAt pos (x1 + 0.5 * (x2 - x1)) 0.01 (vec3 0 1 1)
    path = 
        let pathLength = 5000
            pathLength' = (fromInteger . toInteger) pathLength

            a = uniform $ array $ take 5000 $ iterate (prec x2) x2
            ind = vert [0..(cnst pathLength' - 1)]
            x = a .! ind

            fade = cast ind / cast (cnst pathLength')
            pos = x $- vec2 0 1
            color = (1 - frag fade) .# vec4 1 1 1 1
        in lineStrip { position = pos, color = color }

sphericalPendulum :: GLObj
sphericalPendulum = error "unimplemented"
