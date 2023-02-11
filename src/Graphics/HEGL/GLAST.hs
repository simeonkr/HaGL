module Graphics.HEGL.GLAST (
    GLAST,
    toGLAST
) where

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID (ExprID)


data GLAST where
    GLASTAtom :: ExprID -> GLTypeinfo -> GLAtom d t -> GLAST
    GLASTExpr :: ExprID -> GLTypeinfo -> String -> [GLAST] -> GLAST


data GLTypeinfo = GLTypeinfo {
    shaderType :: ShaderDomain,
    exprType :: ExprType
}

type ExprType = String

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


makeGLExpr id e = GLASTExpr id (GLTypeinfo (getShaderType e) (showGlslType e))

toGLAST :: IsGLDomain d => GLExpr d t -> GLAST
toGLAST e@(GLAtom id x) = GLASTAtom id (GLTypeinfo (getShaderType e) (showGlslType e)) x
toGLAST e@(GLGenExpr _ _) = exprToGLAST e

exprToGLAST :: IsGLDomain d => GLExpr d t -> GLAST

exprToGLAST e@(GLGenExpr id (GLVec2 x y)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GLVec3 x y z)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (GLVec4 x y z w)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]
exprToGLAST e@(GLGenExpr id (GLMat2x2 x y)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GLMat4x4 x y z w)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]

exprToGLAST e@(GLGenExpr id (GLArray xs)) = (makeGLExpr id e) "array" (map toGLAST xs)
exprToGLAST e@(GLGenExpr id (OpArrayElt arr i)) = (makeGLExpr id e) "[]" [toGLAST arr, toGLAST i]

exprToGLAST e@(GLGenExpr id (Pre x y)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (App x y)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Conc x y)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (HorConc x y)) = (makeGLExpr id e) (showGlslType e) [toGLAST x, toGLAST y]

exprToGLAST e@(GLGenExpr id (ToFloat x)) = (makeGLExpr id e) "float" [toGLAST x]
exprToGLAST e@(GLGenExpr id (ToInt x)) = (makeGLExpr id e) "int" [toGLAST x]
exprToGLAST e@(GLGenExpr id (ToUInt x)) = (makeGLExpr id e) "uint" [toGLAST x]

exprToGLAST e@(GLGenExpr id (OpCoord coord x)) = (makeGLExpr id e) ("." ++ show coord) [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpCoordMulti coordList x)) = (makeGLExpr id e) ("." ++ show coordList) [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpCol col x)) = (makeGLExpr id e) (show col) [toGLAST x]

exprToGLAST e@(GLGenExpr id (OpAdd x y)) = (makeGLExpr id e) "+" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpSubt x y)) = (makeGLExpr id e) "-" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpMult x y)) = (makeGLExpr id e) "*" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpDiv x y)) = (makeGLExpr id e) "/" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpMod x y)) = (makeGLExpr id e) "%" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpNeg x)) = (makeGLExpr id e) "-" [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpLessThan x y)) = (makeGLExpr id e) "<" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpLessThanEqual x y)) = (makeGLExpr id e) "<=" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpGreaterThan x y)) = (makeGLExpr id e) ">" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpGreaterThanEqual x y)) = (makeGLExpr id e) ">=" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpEqual x y)) = (makeGLExpr id e) "==" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpNotEqual x y)) = (makeGLExpr id e) "!=" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpAnd x y)) = (makeGLExpr id e) "&&" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpOr x y)) = (makeGLExpr id e) "||" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpXor x y)) = (makeGLExpr id e) "^^" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpNot x)) = (makeGLExpr id e) "!" [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpCond x y z)) = (makeGLExpr id e) "?:" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (OpCompl x)) = (makeGLExpr id e) "~" [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpLshift x y)) = (makeGLExpr id e) "<<" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpRshift x y)) = (makeGLExpr id e) ">>" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpBitAnd x y)) = (makeGLExpr id e) "&" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpBitOr x y)) = (makeGLExpr id e) "|" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpBitXor x y)) = (makeGLExpr id e) "^" [toGLAST x, toGLAST y]

exprToGLAST e@(GLGenExpr id (OpScalarMult x y)) = (makeGLExpr id e) "*" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpMatrixMult x y)) = (makeGLExpr id e) "*" [toGLAST x, toGLAST y]

exprToGLAST e@(GLGenExpr id (Radians x)) = (makeGLExpr id e) "radians" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Degrees x)) = (makeGLExpr id e) "degrees" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Sin x)) = (makeGLExpr id e) "sin" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Cos x)) = (makeGLExpr id e) "cos" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Tan x)) = (makeGLExpr id e) "tan" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Asin x)) = (makeGLExpr id e) "asin" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Acos x)) = (makeGLExpr id e) "acos" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Atan x)) = (makeGLExpr id e) "atan" [toGLAST x]

exprToGLAST e@(GLGenExpr id (Pow x y)) = (makeGLExpr id e) "pow" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Exp x)) = (makeGLExpr id e) "exp" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Log x)) = (makeGLExpr id e) "log" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Exp2 x)) = (makeGLExpr id e) "exp2" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Log2 x)) = (makeGLExpr id e) "log2" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Sqrt x)) = (makeGLExpr id e) "sqrt" [toGLAST x]

exprToGLAST e@(GLGenExpr id (Abs x)) = (makeGLExpr id e) "abs" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Sign x)) = (makeGLExpr id e) "sign" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Floor x)) = (makeGLExpr id e) "floor" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Trunc x)) = (makeGLExpr id e) "trunc" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Round x)) = (makeGLExpr id e) "round" [toGLAST x]
exprToGLAST e@(GLGenExpr id (RoundEven x)) = (makeGLExpr id e) "roundEven" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Ceil x)) = (makeGLExpr id e) "ceil" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Fract x)) = (makeGLExpr id e) "fract" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Mod x y)) = (makeGLExpr id e) "mod" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Min x y)) = (makeGLExpr id e) "min" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Max x y)) = (makeGLExpr id e) "max" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Clamp x y z)) = (makeGLExpr id e) "clamp" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (Mix x y z)) = (makeGLExpr id e) "mix" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (Step x y)) = (makeGLExpr id e) "step" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Smoothstep x y z)) = (makeGLExpr id e) "smoothstep" [toGLAST x, toGLAST y, toGLAST z]

exprToGLAST e@(GLGenExpr id (Length x)) = (makeGLExpr id e) "length" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Distance x y)) = (makeGLExpr id e) "distance" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Dot x y)) = (makeGLExpr id e) "dot" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Cross x y)) = (makeGLExpr id e) "cross" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Normalize x)) = (makeGLExpr id e) "normalize" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Faceforward x y z)) = (makeGLExpr id e) "faceforward" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (Reflect x y)) = (makeGLExpr id e) "reflect" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Refract x y z)) = (makeGLExpr id e) "refract" [toGLAST x, toGLAST y, toGLAST z]

exprToGLAST e@(GLGenExpr id (MatrixCompMult x y)) = (makeGLExpr id e) "matrixCompMult" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OuterProduct x y)) = (makeGLExpr id e) "outerProduct" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Transpose x)) = (makeGLExpr id e) "transpose" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Determinant x)) = (makeGLExpr id e) "determinant" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Inverse x)) = (makeGLExpr id e) "inverse" [toGLAST x]

exprToGLAST e@(GLGenExpr id (LessThan x y)) = (makeGLExpr id e) "lessThan" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (LessThanEqual x y)) = (makeGLExpr id e) "lessThanEqual" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GreaterThan x y)) = (makeGLExpr id e) "greaterThan" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GreaterThanEqual x y)) = (makeGLExpr id e) "greaterThanEqual" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Equal x y)) = (makeGLExpr id e) "equal" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (NotEqual x y)) = (makeGLExpr id e) "notEqual" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Any x)) = (makeGLExpr id e) "any" [toGLAST x]
exprToGLAST e@(GLGenExpr id (All x)) = (makeGLExpr id e) "all" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Compl x)) = (makeGLExpr id e) "not" [toGLAST x]
