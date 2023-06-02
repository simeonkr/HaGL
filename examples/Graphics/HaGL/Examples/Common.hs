module Graphics.HaGL.Examples.Common (
    defaultProj,
    defaultBlinnPhong,
    MeshPainter,
    meshDrawable
) where

import Prelude hiding (sin, cos)

import Graphics.HaGL
import Graphics.HaGL.Lib.Math
import Graphics.HaGL.Lib.Mesh
import Graphics.HaGL.Lib.Shading (blinnPhong)


defaultProj :: GLExpr d (Mat 4 4 Float)
defaultProj = perspective (pi / 6) 1 1 10

defaultBlinnPhong :: FragExpr (Vec 3 Float) -> 
                     FragExpr (Vec 3 Float) -> 
                     FragExpr (Vec 3 Float) -> 
                     FragExpr (Vec 4 Float)
defaultBlinnPhong p n e = blinnPhong ka kd ks n e l pp where
    kd = vec3 0.8 0.6 0.6
    ks = vec3 1 1 1
    ka = vec3 0.2 0.8 0.5
    pp = 1000
    t = uniform time
    xyRotLight off = vec3 (off * cos t) 0 (off * sin t)
    l = normalize $ (xyRotLight 5) - p


type MeshPainter = FragExpr (Vec 3 Float) -> 
                  FragExpr (Vec 3 Float) -> 
                  FragExpr (Vec 4 Float)

meshDrawable :: HostExpr (Mat 4 4 Float) -> 
                HostExpr (Mat 4 4 Float) ->
                MeshPainter ->
                Mesh ->
                GLObj
meshDrawable proj view painter mesh = let
    pos = vert $ meshVertices mesh
    norm = case meshNormals mesh of
        [] -> pos   -- pretend mesh is a sphere
        ns -> vert ns

    cpos = uniform (proj .@ view) .@ app pos 1
    
    color = painter (frag pos) (normalize $ frag norm)

    in triangles { indices = Just $ meshFaces mesh, 
                   position = cpos, 
                   color = color }
