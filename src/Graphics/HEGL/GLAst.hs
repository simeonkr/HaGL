module Graphics.HEGL.GLAst (
    GLAst(..),
    GLTypeInfo(..),
    IsGLDomain(..),
    getID,
    toGLAst
) where

import Data.List (intercalate)

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID


data GLAst where
    GLAstAtom :: ExprID -> GLTypeInfo -> GLAtom d t -> GLAst
    GLAstFunc :: ExprID -> GLTypeInfo -> GLAst -> [GLAst] -> GLAst
    GLAstFuncApp :: ExprID -> GLTypeInfo -> GLAst -> [GLAst] -> GLAst
    GLAstExpr :: ExprID -> GLTypeInfo -> String -> [GLAst] -> GLAst


data GLTypeInfo = GLTypeInfo {
    shaderType :: ShaderDomain,
    exprType :: String
}

class IsGLDomain (d :: ShaderDomain) where
    getShaderType :: (GLExpr d t) -> ShaderDomain
instance IsGLDomain ConstDomain where
    getShaderType = const ConstDomain
instance IsGLDomain HostDomain where
    getShaderType = const HostDomain
instance IsGLDomain VertexDomain where
    getShaderType = const VertexDomain
instance IsGLDomain FragmentDomain where
    getShaderType = const FragmentDomain

instance HasExprID GLAst where
    getID (GLAstAtom id _ _) = id
    getID (GLAstFunc id _ _ _) = id
    getID (GLAstFuncApp id _ _ _) = id
    getID (GLAstExpr id _ _ _) = id


getGLTypeInfo e = GLTypeInfo (getShaderType e) (showGlslType e)
mkGLFn funcID r params args = 
    GLAstFuncApp callID (getGLTypeInfo r) func args where
        func = GLAstFunc funcID (getGLTypeInfo r) (toGLAst r) params
        callID = encodeUniquely (map getID args)
mkGLExpr id e = GLAstExpr id (getGLTypeInfo e)

vd = error "GLLift*: Output list length must be independent of list contents"
showRetArraySize r = if arrayLen r == 1 then "" else "[" ++ show (arrayLen r) ++ "]"


toGLAst :: IsGLDomain d => GLExpr d t -> GLAst
toGLAst e@(GLAtom id x@(GLLift1 f _)) = GLAstAtom id (GLTypeInfo (getShaderType e) tystr) x where
    tystr = showGlslType e ++ showRetArraySize (f vd)
toGLAst e@(GLAtom id x@(GLLift2 f _ _)) = GLAstAtom id (GLTypeInfo (getShaderType e) tystr) x where
    tystr = showGlslType e ++ showRetArraySize (f vd vd)
toGLAst e@(GLAtom id x) = GLAstAtom id (getGLTypeInfo e) x

toGLAst (GLFunc id (GLFunc1 f x x0)) = mkGLFn id (f x) [toGLAst x] [toGLAst x0]
toGLAst (GLFunc id (GLFunc2 f x y x0 y0)) = mkGLFn id (f x y) [toGLAst x, toGLAst y] [toGLAst x0, toGLAst y0]
toGLAst (GLFunc id (GLFunc3 f x y z x0 y0 z0)) = mkGLFn id (f x y z) [toGLAst x, toGLAst y, toGLAst z] [toGLAst x0, toGLAst y0, toGLAst z0]

toGLAst e@(GLGenExpr id (GLVec2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLVec3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLVec4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (GLMat2x2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLMat2x3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLMat2x4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (GLMat3x2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLMat3x3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLMat3x4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (GLMat4x2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLMat4x3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLMat4x4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (Pre x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (App x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Conc x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (HorConc x y)) = (mkGLExpr id e) (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLArray xs)) = GLAstExpr id (GLTypeInfo (getShaderType e) tystr) "array" (map toGLAst xs) where
    tystr = showGlslType e ++ "[" ++ show (length xs) ++ "]"

toGLAst e@(GLGenExpr id (OpCoord coord x)) = (mkGLExpr id e) ("." ++ show coord) [toGLAst x]
toGLAst e@(GLGenExpr id (OpCoordMulti coordList x)) = (mkGLExpr id e) ("." ++ show coordList) [toGLAst x]
toGLAst e@(GLGenExpr id (OpCol col x)) = (mkGLExpr id e) (show col) [toGLAst x]
toGLAst e@(GLGenExpr id (OpArrayElt arr i)) = (mkGLExpr id e) "[]" [toGLAst arr, toGLAst i]

toGLAst e@(GLGenExpr id (Cast x)) = (mkGLExpr id e) (showGlslType e) [toGLAst x]

toGLAst e@(GLGenExpr id (OpAdd x y)) = (mkGLExpr id e) "+" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpSubt x y)) = (mkGLExpr id e) "-" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpMult x y)) = (mkGLExpr id e) "*" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpDiv x y)) = (mkGLExpr id e) "/" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpMod x y)) = (mkGLExpr id e) "%" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpNeg x)) = (mkGLExpr id e) "-" [toGLAst x]
toGLAst e@(GLGenExpr id (OpLessThan x y)) = (mkGLExpr id e) "<" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpLessThanEqual x y)) = (mkGLExpr id e) "<=" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpGreaterThan x y)) = (mkGLExpr id e) ">" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpGreaterThanEqual x y)) = (mkGLExpr id e) ">=" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpEqual x y)) = (mkGLExpr id e) "==" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpNotEqual x y)) = (mkGLExpr id e) "!=" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpAnd x y)) = (mkGLExpr id e) "&&" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpOr x y)) = (mkGLExpr id e) "||" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpXor x y)) = (mkGLExpr id e) "^^" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpNot x)) = (mkGLExpr id e) "!" [toGLAst x]
toGLAst e@(GLGenExpr id (OpCond x y z)) = (mkGLExpr id e) "?:" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (OpCompl x)) = (mkGLExpr id e) "~" [toGLAst x]
toGLAst e@(GLGenExpr id (OpLshift x y)) = (mkGLExpr id e) "<<" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpRshift x y)) = (mkGLExpr id e) ">>" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpBitAnd x y)) = (mkGLExpr id e) "&" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpBitOr x y)) = (mkGLExpr id e) "|" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpBitXor x y)) = (mkGLExpr id e) "^" [toGLAst x, toGLAst y]

toGLAst e@(GLGenExpr id (OpScalarMult x y)) = (mkGLExpr id e) "*" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpMatrixMult x y)) = (mkGLExpr id e) "*" [toGLAst x, toGLAst y]

toGLAst e@(GLGenExpr id (Radians x)) = (mkGLExpr id e) "radians" [toGLAst x]
toGLAst e@(GLGenExpr id (Degrees x)) = (mkGLExpr id e) "degrees" [toGLAst x]
toGLAst e@(GLGenExpr id (Sin x)) = (mkGLExpr id e) "sin" [toGLAst x]
toGLAst e@(GLGenExpr id (Cos x)) = (mkGLExpr id e) "cos" [toGLAst x]
toGLAst e@(GLGenExpr id (Tan x)) = (mkGLExpr id e) "tan" [toGLAst x]
toGLAst e@(GLGenExpr id (Asin x)) = (mkGLExpr id e) "asin" [toGLAst x]
toGLAst e@(GLGenExpr id (Acos x)) = (mkGLExpr id e) "acos" [toGLAst x]
toGLAst e@(GLGenExpr id (Atan x)) = (mkGLExpr id e) "atan" [toGLAst x]

toGLAst e@(GLGenExpr id (Pow x y)) = (mkGLExpr id e) "pow" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Exp x)) = (mkGLExpr id e) "exp" [toGLAst x]
toGLAst e@(GLGenExpr id (Log x)) = (mkGLExpr id e) "log" [toGLAst x]
toGLAst e@(GLGenExpr id (Exp2 x)) = (mkGLExpr id e) "exp2" [toGLAst x]
toGLAst e@(GLGenExpr id (Log2 x)) = (mkGLExpr id e) "log2" [toGLAst x]
toGLAst e@(GLGenExpr id (Sqrt x)) = (mkGLExpr id e) "sqrt" [toGLAst x]

toGLAst e@(GLGenExpr id (Abs x)) = (mkGLExpr id e) "abs" [toGLAst x]
toGLAst e@(GLGenExpr id (Sign x)) = (mkGLExpr id e) "sign" [toGLAst x]
toGLAst e@(GLGenExpr id (Floor x)) = (mkGLExpr id e) "floor" [toGLAst x]
toGLAst e@(GLGenExpr id (Trunc x)) = (mkGLExpr id e) "trunc" [toGLAst x]
toGLAst e@(GLGenExpr id (Round x)) = (mkGLExpr id e) "round" [toGLAst x]
toGLAst e@(GLGenExpr id (RoundEven x)) = (mkGLExpr id e) "roundEven" [toGLAst x]
toGLAst e@(GLGenExpr id (Ceil x)) = (mkGLExpr id e) "ceil" [toGLAst x]
toGLAst e@(GLGenExpr id (Fract x)) = (mkGLExpr id e) "fract" [toGLAst x]
toGLAst e@(GLGenExpr id (Mod x y)) = (mkGLExpr id e) "mod" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Min x y)) = (mkGLExpr id e) "min" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Max x y)) = (mkGLExpr id e) "max" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Clamp x y z)) = (mkGLExpr id e) "clamp" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (Mix x y z)) = (mkGLExpr id e) "mix" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (Step x y)) = (mkGLExpr id e) "step" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Smoothstep x y z)) = (mkGLExpr id e) "smoothstep" [toGLAst x, toGLAst y, toGLAst z]

toGLAst e@(GLGenExpr id (Length x)) = (mkGLExpr id e) "length" [toGLAst x]
toGLAst e@(GLGenExpr id (Distance x y)) = (mkGLExpr id e) "distance" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Dot x y)) = (mkGLExpr id e) "dot" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Cross x y)) = (mkGLExpr id e) "cross" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Normalize x)) = (mkGLExpr id e) "normalize" [toGLAst x]
toGLAst e@(GLGenExpr id (Faceforward x y z)) = (mkGLExpr id e) "faceforward" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (Reflect x y)) = (mkGLExpr id e) "reflect" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Refract x y z)) = (mkGLExpr id e) "refract" [toGLAst x, toGLAst y, toGLAst z]

toGLAst e@(GLGenExpr id (MatrixCompMult x y)) = (mkGLExpr id e) "matrixCompMult" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OuterProduct x y)) = (mkGLExpr id e) "outerProduct" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Transpose x)) = (mkGLExpr id e) "transpose" [toGLAst x]
toGLAst e@(GLGenExpr id (Determinant x)) = (mkGLExpr id e) "determinant" [toGLAst x]
toGLAst e@(GLGenExpr id (Inverse x)) = (mkGLExpr id e) "inverse" [toGLAst x]

toGLAst e@(GLGenExpr id (LessThan x y)) = (mkGLExpr id e) "lessThan" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (LessThanEqual x y)) = (mkGLExpr id e) "lessThanEqual" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GreaterThan x y)) = (mkGLExpr id e) "greaterThan" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GreaterThanEqual x y)) = (mkGLExpr id e) "greaterThanEqual" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Equal x y)) = (mkGLExpr id e) "equal" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (NotEqual x y)) = (mkGLExpr id e) "notEqual" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Any x)) = (mkGLExpr id e) "any" [toGLAst x]
toGLAst e@(GLGenExpr id (All x)) = (mkGLExpr id e) "all" [toGLAst x]
toGLAst e@(GLGenExpr id (Compl x)) = (mkGLExpr id e) "not" [toGLAst x]
