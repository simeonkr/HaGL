module Graphics.HEGL.GLAST (
    GLAST,
    toGLAST
) where

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID (ExprID)


data GLAST where
    GLASTAtom :: ExprID -> GlTypeInfo -> GLAtom d t -> GLAST
    GLASTExpr :: ExprID -> GlTypeInfo -> String -> [GLAST] -> GLAST


data GlTypeInfo = GlTypeInfo {
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

showGlslType = undefined


makeGlExpr id e = GLASTExpr id (GlTypeInfo (getShaderType e) (showGlslType e))

toGLAST :: IsGLDomain d => GLExpr d t -> GLAST
toGLAST e@(GLAtom id x) = GLASTAtom id (GlTypeInfo (getShaderType e) (showGlslType e)) x
toGLAST e@(GLGenExpr _ _) = exprToGLAST e

exprToGLAST :: IsGLDomain d => GLExpr d t -> GLAST

exprToGLAST e@(GLGenExpr id (GLVec2 x y)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GLVec3 x y z)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (GLVec4 x y z w)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]
exprToGLAST e@(GLGenExpr id (GLMat2x2 x y)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GLMat4x4 x y z w)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y, toGLAST z, toGLAST w]

exprToGLAST e@(GLGenExpr id (GLArray xs)) = (makeGlExpr id e) "array" (map toGLAST xs)
exprToGLAST e@(GLGenExpr id (OpArrayElt arr i)) = (makeGlExpr id e) "[]" [toGLAST arr, toGLAST i]

exprToGLAST e@(GLGenExpr id (Pre x y)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (App x y)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Conc x y)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (HorConc x y)) = (makeGlExpr id e) (showGlslType e) [toGLAST x, toGLAST y]

exprToGLAST e@(GLGenExpr id (ToFloat x)) = (makeGlExpr id e) "float" [toGLAST x]
exprToGLAST e@(GLGenExpr id (ToInt x)) = (makeGlExpr id e) "int" [toGLAST x]
exprToGLAST e@(GLGenExpr id (ToUInt x)) = (makeGlExpr id e) "uint" [toGLAST x]

exprToGLAST e@(GLGenExpr id (OpCoord coord x)) = (makeGlExpr id e) ("." ++ show coord) [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpCoordMulti coordList x)) = (makeGlExpr id e) ("." ++ show coordList) [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpCol col x)) = (makeGlExpr id e) (show col) [toGLAST x]

exprToGLAST e@(GLGenExpr id (OpAdd x y)) = (makeGlExpr id e) "+" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpSubt x y)) = (makeGlExpr id e) "-" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpMult x y)) = (makeGlExpr id e) "*" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpDiv x y)) = (makeGlExpr id e) "/" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpMod x y)) = (makeGlExpr id e) "%" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpNeg x)) = (makeGlExpr id e) "-" [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpLessThan x y)) = (makeGlExpr id e) "<" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpLessThanEqual x y)) = (makeGlExpr id e) "<=" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpGreaterThan x y)) = (makeGlExpr id e) ">" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpGreaterThanEqual x y)) = (makeGlExpr id e) ">=" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpEqual x y)) = (makeGlExpr id e) "==" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpNotEqual x y)) = (makeGlExpr id e) "!=" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpAnd x y)) = (makeGlExpr id e) "&&" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpOr x y)) = (makeGlExpr id e) "||" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpXor x y)) = (makeGlExpr id e) "^^" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpNot x)) = (makeGlExpr id e) "!" [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpCond x y z)) = (makeGlExpr id e) "?:" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (OpCompl x)) = (makeGlExpr id e) "~" [toGLAST x]
exprToGLAST e@(GLGenExpr id (OpLshift x y)) = (makeGlExpr id e) "<<" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpRshift x y)) = (makeGlExpr id e) ">>" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpBitAnd x y)) = (makeGlExpr id e) "&" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpBitOr x y)) = (makeGlExpr id e) "|" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpBitXor x y)) = (makeGlExpr id e) "^" [toGLAST x, toGLAST y]

exprToGLAST e@(GLGenExpr id (OpScalarMult x y)) = (makeGlExpr id e) "*" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OpMatrixMult x y)) = (makeGlExpr id e) "*" [toGLAST x, toGLAST y]

exprToGLAST e@(GLGenExpr id (Radians x)) = (makeGlExpr id e) "radians" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Degrees x)) = (makeGlExpr id e) "degrees" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Sin x)) = (makeGlExpr id e) "sin" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Cos x)) = (makeGlExpr id e) "cos" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Tan x)) = (makeGlExpr id e) "tan" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Asin x)) = (makeGlExpr id e) "asin" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Acos x)) = (makeGlExpr id e) "acos" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Atan x)) = (makeGlExpr id e) "atan" [toGLAST x]

exprToGLAST e@(GLGenExpr id (Pow x y)) = (makeGlExpr id e) "pow" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Exp x)) = (makeGlExpr id e) "exp" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Log x)) = (makeGlExpr id e) "log" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Exp2 x)) = (makeGlExpr id e) "exp2" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Log2 x)) = (makeGlExpr id e) "log2" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Sqrt x)) = (makeGlExpr id e) "sqrt" [toGLAST x]

exprToGLAST e@(GLGenExpr id (Abs x)) = (makeGlExpr id e) "abs" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Sign x)) = (makeGlExpr id e) "sign" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Floor x)) = (makeGlExpr id e) "floor" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Trunc x)) = (makeGlExpr id e) "trunc" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Round x)) = (makeGlExpr id e) "round" [toGLAST x]
exprToGLAST e@(GLGenExpr id (RoundEven x)) = (makeGlExpr id e) "roundEven" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Ceil x)) = (makeGlExpr id e) "ceil" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Fract x)) = (makeGlExpr id e) "fract" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Mod x y)) = (makeGlExpr id e) "mod" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Min x y)) = (makeGlExpr id e) "min" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Max x y)) = (makeGlExpr id e) "max" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Clamp x y z)) = (makeGlExpr id e) "clamp" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (Mix x y z)) = (makeGlExpr id e) "mix" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (Step x y)) = (makeGlExpr id e) "step" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Smoothstep x y z)) = (makeGlExpr id e) "smoothstep" [toGLAST x, toGLAST y, toGLAST z]

exprToGLAST e@(GLGenExpr id (Length x)) = (makeGlExpr id e) "length" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Distance x y)) = (makeGlExpr id e) "distance" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Dot x y)) = (makeGlExpr id e) "dot" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Cross x y)) = (makeGlExpr id e) "cross" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Normalize x)) = (makeGlExpr id e) "normalize" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Faceforward x y z)) = (makeGlExpr id e) "faceforward" [toGLAST x, toGLAST y, toGLAST z]
exprToGLAST e@(GLGenExpr id (Reflect x y)) = (makeGlExpr id e) "reflect" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Refract x y z)) = (makeGlExpr id e) "refract" [toGLAST x, toGLAST y, toGLAST z]

exprToGLAST e@(GLGenExpr id (MatrixCompMult x y)) = (makeGlExpr id e) "matrixCompMult" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (OuterProduct x y)) = (makeGlExpr id e) "outerProduct" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Transpose x)) = (makeGlExpr id e) "transpose" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Determinant x)) = (makeGlExpr id e) "determinant" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Inverse x)) = (makeGlExpr id e) "inverse" [toGLAST x]

exprToGLAST e@(GLGenExpr id (LessThan x y)) = (makeGlExpr id e) "lessThan" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (LessThanEqual x y)) = (makeGlExpr id e) "lessThanEqual" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GreaterThan x y)) = (makeGlExpr id e) "greaterThan" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (GreaterThanEqual x y)) = (makeGlExpr id e) "greaterThanEqual" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Equal x y)) = (makeGlExpr id e) "equal" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (NotEqual x y)) = (makeGlExpr id e) "notEqual" [toGLAST x, toGLAST y]
exprToGLAST e@(GLGenExpr id (Any x)) = (makeGlExpr id e) "any" [toGLAST x]
exprToGLAST e@(GLGenExpr id (All x)) = (makeGlExpr id e) "all" [toGLAST x]
exprToGLAST e@(GLGenExpr id (Compl x)) = (makeGlExpr id e) "not" [toGLAST x]
