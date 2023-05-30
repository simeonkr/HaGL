module Graphics.HaGL.Lib.Camera (
    interactiveView
) where

import Graphics.HaGL
import Graphics.HaGL.Lib.Math (idMat, rotate, translate, scale)

interactiveView :: _ => HostExpr (Vec 3 Float) -> HostExpr (Mat 4 4 Float)
interactiveView initialEye = inverse curCam where
    translationFactor = 5
    initCam = inverse $ translate (-initialEye)
    prevCam = prec initCam curCam
    camX = normalize . xyz_ $ col0 prevCam
    camY = normalize . xyz_ $ col1 prevCam
    dx = mouseX - prec mouseX mouseX
    dy = mouseY - prec mouseY mouseY
    rotx = cond mouseLeft 
        (rotate camY (-2.0 * pi * dx)) 
        idMat
    roty = cond mouseLeft 
        (rotate camX (2.0 * pi * dy)) 
        idMat
    transx = cond mouseRight
        (translate $ -translationFactor * dx .# camX)
        idMat
    transy = cond mouseRight
        (translate $ -translationFactor * dy .# camY)
        idMat
    zoom = scale $ (-0.5 * mouseWheel + 1) .# (vec3 1 1 1)
    curCam = zoom .@ transy .@ transx .@ roty .@ rotx .@ prevCam
