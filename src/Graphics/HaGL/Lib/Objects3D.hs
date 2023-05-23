module Graphics.HaGL.Lib.Objects3D (
    paramRange2D,
    paramInds2D,
    uvSphere
) where

import Prelude hiding (sin, cos)

import Graphics.HaGL
import Graphics.HaGL.Lib.Mesh (Mesh(..))


paramRange2D :: ConstExpr (Vec 2 Float) -> ConstExpr (Vec 2 Float) -> 
    ConstExpr Float -> [ConstExpr (Vec 2 Float)]
paramRange2D (decon -> (ul, ur)) (decon -> (vl, vr)) res =
    [vec2 (ul + i * (ur - ul) / res) 
          (vl + j * (vr - vl) / res) 
        | i <- [0..res], j <- [0..res]]

paramInds2D :: ConstExpr UInt -> [ConstExpr UInt]
paramInds2D res =
    concatMap (map cast)
        [[i * (res + 1) + j, 
          i * (res + 1) + (j + 1),  
          (i + 1) * (res + 1) + (j + 1),
          (i + 1) * (res + 1) + j,
          i * (res + 1) + j,
          (i + 1) * (res + 1) + (j + 1)] 
         | i <- [0..(res-1)], j <- [0..(res-1)]]

uvSphere :: ConstExpr Float -> (Mesh, [ConstExpr (Vec 2 Float)])
uvSphere radius = 
    let res = 32
        unifGrid = paramRange2D (vec2 0 (2 * pi)) (vec2 0 pi) res
        f (decon -> (u, v)) = radius .# vec3 (cos u * sin v) (sin u * sin v) (cos v)
        verts = map f unifGrid
        n uv = normalize uv
        normals = map n verts
        inds = paramInds2D $ cast res
    in (Mesh verts normals inds, [vec2 i j | i <- [0..res], j <- [0..res]])
