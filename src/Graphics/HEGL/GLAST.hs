module Graphics.HEGL.GLAST (
    GLAST(..),
    GLTypeInfo(..),
    IsGLDomain(..),
    getID,
    toGLAST
) where

import Data.List (intercalate)

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID


data GLAST where
    GLASTAtom :: ExprID -> GLTypeInfo -> GLAtom d t -> GLAST
    GLASTFuncApp :: ExprID -> GLTypeInfo -> GLAST -> [GLAST] -> [GLAST] -> GLAST
    GLASTExpr :: ExprID -> GLTypeInfo -> String -> [GLAST] -> GLAST


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

instance HasExprID GLAST where
    getID (GLASTAtom id _ _) = id
    getID (GLASTFuncApp id _ _ _ _) = id
    getID (GLASTExpr id _ _ _) = id


getGLTypeInfo e = GLTypeInfo (getShaderType e) (showGlslType e)
mkGLFn funcId r params args = 
    GLASTFuncApp callId (getGLTypeInfo r) (toGLAST r) params args where
        callId = encodeUniquely (map getID args)
mkGLExpr id e = GLASTExpr id (getGLTypeInfo e)

vd = error "GLLift*: Output list length must be independent of list contents"
showRetArraySize r = if arrayLen r == 1 then "" else "[" ++ show (arrayLen r) ++ "]"


toGLAST :: IsGLDomain d => GLExpr d t -> GLAST
toGLAST e@(GLAtom id x@(GLLift1 f _)) = GLASTAtom id (GLTypeInfo (getShaderType e) tystr) x where
    tystr = showGlslType e ++ showRetArraySize (f vd)
toGLAST e@(GLAtom id x@(GLLift2 f _ _)) = GLASTAtom id (GLTypeInfo (getShaderType e) tystr) x where
    tystr = showGlslType e ++ showRetArraySize (f vd vd)
toGLAST e@(GLAtom id x) = GLASTAtom id (getGLTypeInfo e) x

toGLAST (GLFunc id (GLFunc1 f x x0)) = mkGLFn id (f x) [toGLAST x] [toGLAST x0]
toGLAST (GLFunc id (GLFunc2 f x y x0 y0)) = mkGLFn id (f x y) [toGLAST x, toGLAST y] [toGLAST x0, toGLAST y0]
toGLAST (GLFunc id (GLFunc3 f x y z x0 y0 z0)) = mkGLFn id (f x y z) [toGLAST x, toGLAST y, toGLAST z] [toGLAST x0, toGLAST y0, toGLAST z0]

toGLAST e@(GLGenExpr id (GLVec2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GLVec3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (GLVec4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]
toGLAST e@(GLGenExpr id (GLMat2x2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GLMat2x3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (GLMat2x4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]
toGLAST e@(GLGenExpr id (GLMat3x2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GLMat3x3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (GLMat3x4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]
toGLAST e@(GLGenExpr id (GLMat4x2 x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GLMat4x3 x y z)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (GLMat4x4 x y z w)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]
toGLAST e@(GLGenExpr id (Pre x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (App x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Conc x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (HorConc x y)) = (mkGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GLArray xs)) = GLASTExpr id (GLTypeInfo (getShaderType e) tystr) "array" (map toGLAST xs) where
    tystr = showGlslType e ++ "[" ++ show (length xs) ++ "]"

toGLAST e@(GLGenExpr id (OpCoord coord x)) = (mkGLExpr id e) ("." ++ show coord) [toGLAST x]
toGLAST e@(GLGenExpr id (OpCoordMulti coordList x)) = (mkGLExpr id e) ("." ++ show coordList) [toGLAST x]
toGLAST e@(GLGenExpr id (OpCol col x)) = (mkGLExpr id e) (show col) [toGLAST x]
toGLAST e@(GLGenExpr id (OpArrayElt arr i)) = (mkGLExpr id e) "[]" [toGLAST arr, toGLAST i]

toGLAST e@(GLGenExpr id (ToFloat x)) = (mkGLExpr id e) "float" [toGLAST x]
toGLAST e@(GLGenExpr id (ToInt x)) = (mkGLExpr id e) "int" [toGLAST x]
toGLAST e@(GLGenExpr id (ToUInt x)) = (mkGLExpr id e) "uint" [toGLAST x]

toGLAST e@(GLGenExpr id (OpAdd x y)) = (mkGLExpr id e) "+" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpSubt x y)) = (mkGLExpr id e) "-" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpMult x y)) = (mkGLExpr id e) "*" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpDiv x y)) = (mkGLExpr id e) "/" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpMod x y)) = (mkGLExpr id e) "%" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpNeg x)) = (mkGLExpr id e) "-" [toGLAST x]
toGLAST e@(GLGenExpr id (OpLessThan x y)) = (mkGLExpr id e) "<" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpLessThanEqual x y)) = (mkGLExpr id e) "<=" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpGreaterThan x y)) = (mkGLExpr id e) ">" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpGreaterThanEqual x y)) = (mkGLExpr id e) ">=" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpEqual x y)) = (mkGLExpr id e) "==" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpNotEqual x y)) = (mkGLExpr id e) "!=" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpAnd x y)) = (mkGLExpr id e) "&&" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpOr x y)) = (mkGLExpr id e) "||" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpXor x y)) = (mkGLExpr id e) "^^" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpNot x)) = (mkGLExpr id e) "!" [toGLAST x]
toGLAST e@(GLGenExpr id (OpCond x y z)) = (mkGLExpr id e) "?:" [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (OpCompl x)) = (mkGLExpr id e) "~" [toGLAST x]
toGLAST e@(GLGenExpr id (OpLshift x y)) = (mkGLExpr id e) "<<" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpRshift x y)) = (mkGLExpr id e) ">>" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpBitAnd x y)) = (mkGLExpr id e) "&" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpBitOr x y)) = (mkGLExpr id e) "|" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpBitXor x y)) = (mkGLExpr id e) "^" [toGLAST x, toGLAST y]

toGLAST e@(GLGenExpr id (OpScalarMult x y)) = (mkGLExpr id e) "*" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OpMatrixMult x y)) = (mkGLExpr id e) "*" [toGLAST x, toGLAST y]

toGLAST e@(GLGenExpr id (Radians x)) = (mkGLExpr id e) "radians" [toGLAST x]
toGLAST e@(GLGenExpr id (Degrees x)) = (mkGLExpr id e) "degrees" [toGLAST x]
toGLAST e@(GLGenExpr id (Sin x)) = (mkGLExpr id e) "sin" [toGLAST x]
toGLAST e@(GLGenExpr id (Cos x)) = (mkGLExpr id e) "cos" [toGLAST x]
toGLAST e@(GLGenExpr id (Tan x)) = (mkGLExpr id e) "tan" [toGLAST x]
toGLAST e@(GLGenExpr id (Asin x)) = (mkGLExpr id e) "asin" [toGLAST x]
toGLAST e@(GLGenExpr id (Acos x)) = (mkGLExpr id e) "acos" [toGLAST x]
toGLAST e@(GLGenExpr id (Atan x)) = (mkGLExpr id e) "atan" [toGLAST x]

toGLAST e@(GLGenExpr id (Pow x y)) = (mkGLExpr id e) "pow" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Exp x)) = (mkGLExpr id e) "exp" [toGLAST x]
toGLAST e@(GLGenExpr id (Log x)) = (mkGLExpr id e) "log" [toGLAST x]
toGLAST e@(GLGenExpr id (Exp2 x)) = (mkGLExpr id e) "exp2" [toGLAST x]
toGLAST e@(GLGenExpr id (Log2 x)) = (mkGLExpr id e) "log2" [toGLAST x]
toGLAST e@(GLGenExpr id (Sqrt x)) = (mkGLExpr id e) "sqrt" [toGLAST x]

toGLAST e@(GLGenExpr id (Abs x)) = (mkGLExpr id e) "abs" [toGLAST x]
toGLAST e@(GLGenExpr id (Sign x)) = (mkGLExpr id e) "sign" [toGLAST x]
toGLAST e@(GLGenExpr id (Floor x)) = (mkGLExpr id e) "floor" [toGLAST x]
toGLAST e@(GLGenExpr id (Trunc x)) = (mkGLExpr id e) "trunc" [toGLAST x]
toGLAST e@(GLGenExpr id (Round x)) = (mkGLExpr id e) "round" [toGLAST x]
toGLAST e@(GLGenExpr id (RoundEven x)) = (mkGLExpr id e) "roundEven" [toGLAST x]
toGLAST e@(GLGenExpr id (Ceil x)) = (mkGLExpr id e) "ceil" [toGLAST x]
toGLAST e@(GLGenExpr id (Fract x)) = (mkGLExpr id e) "fract" [toGLAST x]
toGLAST e@(GLGenExpr id (Mod x y)) = (mkGLExpr id e) "mod" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Min x y)) = (mkGLExpr id e) "min" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Max x y)) = (mkGLExpr id e) "max" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Clamp x y z)) = (mkGLExpr id e) "clamp" [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (Mix x y z)) = (mkGLExpr id e) "mix" [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (Step x y)) = (mkGLExpr id e) "step" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Smoothstep x y z)) = (mkGLExpr id e) "smoothstep" [toGLAST x, toGLAST y, toGLAST z]

toGLAST e@(GLGenExpr id (Length x)) = (mkGLExpr id e) "length" [toGLAST x]
toGLAST e@(GLGenExpr id (Distance x y)) = (mkGLExpr id e) "distance" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Dot x y)) = (mkGLExpr id e) "dot" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Cross x y)) = (mkGLExpr id e) "cross" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Normalize x)) = (mkGLExpr id e) "normalize" [toGLAST x]
toGLAST e@(GLGenExpr id (Faceforward x y z)) = (mkGLExpr id e) "faceforward" [toGLAST x, toGLAST y, toGLAST z]
toGLAST e@(GLGenExpr id (Reflect x y)) = (mkGLExpr id e) "reflect" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Refract x y z)) = (mkGLExpr id e) "refract" [toGLAST x, toGLAST y, toGLAST z]

toGLAST e@(GLGenExpr id (MatrixCompMult x y)) = (mkGLExpr id e) "matrixCompMult" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (OuterProduct x y)) = (mkGLExpr id e) "outerProduct" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Transpose x)) = (mkGLExpr id e) "transpose" [toGLAST x]
toGLAST e@(GLGenExpr id (Determinant x)) = (mkGLExpr id e) "determinant" [toGLAST x]
toGLAST e@(GLGenExpr id (Inverse x)) = (mkGLExpr id e) "inverse" [toGLAST x]

toGLAST e@(GLGenExpr id (LessThan x y)) = (mkGLExpr id e) "lessThan" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (LessThanEqual x y)) = (mkGLExpr id e) "lessThanEqual" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GreaterThan x y)) = (mkGLExpr id e) "greaterThan" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (GreaterThanEqual x y)) = (mkGLExpr id e) "greaterThanEqual" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Equal x y)) = (mkGLExpr id e) "equal" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (NotEqual x y)) = (mkGLExpr id e) "notEqual" [toGLAST x, toGLAST y]
toGLAST e@(GLGenExpr id (Any x)) = (mkGLExpr id e) "any" [toGLAST x]
toGLAST e@(GLGenExpr id (All x)) = (mkGLExpr id e) "all" [toGLAST x]
toGLAST e@(GLGenExpr id (Compl x)) = (mkGLExpr id e) "not" [toGLAST x]
