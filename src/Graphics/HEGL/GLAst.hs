-- required due to use of genID
{-# OPTIONS_GHC -fno-full-laziness #-}

module Graphics.HEGL.GLAst (
    GLAst(..),
    GLTypeInfo(..),
    IsGLDomain(..),
    getID,
    toGLAst
) where

import Prelude hiding (id)
import Data.List (isPrefixOf)
import Control.Exception (throw)

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID


data GLAst where
    GLAstAtom :: ExprID -> GLTypeInfo -> GLAtom d t -> GLAst
    GLAstFunc :: ExprID -> GLTypeInfo -> GLAst -> [GLAst] -> GLAst
    GLAstFuncApp :: ExprID -> GLTypeInfo -> GLAst -> [GLAst] -> GLAst
    GLAstExpr :: ExprID -> GLTypeInfo -> String -> [GLAst] -> GLAst


data GLTypeInfo = GLTypeInfo {
    shaderType :: GLDomain,
    exprType :: String
}

class IsGLDomain (d :: GLDomain) where
    getShaderType :: GLExpr d t -> GLDomain
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
mkGLFn funcID callID r params args = 
    GLAstFuncApp callID (getGLTypeInfo r) func args where
        func = GLAstFunc funcID (getGLTypeInfo r) (toGLAst r) params
mkGLExpr id e = GLAstExpr id (getGLTypeInfo e)

showSizedArrayType e n = takeWhile (/= ']') (showGlslType e) ++ show n ++ "]"
showPotentialArrayType e arr = 
    if arrayLen arr == 1 then showGlslType e else showSizedArrayType e (arrayLen arr)
-- TODO: keep this as an assertion, but enforce the constraint at the type level
vd = throw UnknownArraySize


toGLAst :: IsGLDomain d => GLExpr d t -> GLAst

-- Special case where we need to extract a known array size
-- (the current assumption is that only uniform arrays are used in the shader)
toGLAst e@(GLAtom id x@(Uniform (GLGenExpr _ (GLArray arr)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showSizedArrayType e (length arr))
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift0 x0)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e x0)
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift1 f _)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e (f vd))
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift2 f _ _)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e (f vd vd))
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift3 f _ _ _)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e (f vd vd vd))
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift4 f _ _ _ _)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e (f vd vd vd vd))
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift5 f _ _ _ _ _)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e (f vd vd vd vd vd))
toGLAst e@(GLAtom id x@(Uniform (GLAtom _ (GLLift6 f _ _ _ _ _ _)))) = GLAstAtom id ti x where
    ti = GLTypeInfo (getShaderType e) (showPotentialArrayType e (f vd vd vd vd vd vd))
-- FIXME: we never consider the case when arrays appear in precs

toGLAst e@(GLAtom id x) = GLAstAtom id (getGLTypeInfo e) x

toGLAst e@(GLFunc fnID (GLFunc1 f x x0)) = mkGLFn fnID (getID e) (f x) [toGLAst x] [toGLAst x0]
toGLAst e@(GLFunc fnID (GLFunc2 f x y x0 y0)) = mkGLFn fnID (getID e) (f x y) [toGLAst x, toGLAst y] [toGLAst x0, toGLAst y0]
toGLAst e@(GLFunc fnID (GLFunc3 f x y z x0 y0 z0)) = mkGLFn fnID (getID e) (f x y z) [toGLAst x, toGLAst y, toGLAst z] [toGLAst x0, toGLAst y0, toGLAst z0]
toGLAst e@(GLFunc fnID (GLFunc4 f x y z w x0 y0 z0 w0)) = mkGLFn fnID (getID e) (f x y z w) [toGLAst x, toGLAst y, toGLAst z, toGLAst w] [toGLAst x0, toGLAst y0, toGLAst z0, toGLAst w0]
toGLAst e@(GLFunc fnID (GLFunc5 f x y z w u x0 y0 z0 w0 u0)) = mkGLFn fnID (getID e) (f x y z w u) [toGLAst x, toGLAst y, toGLAst z, toGLAst w, toGLAst u] [toGLAst x0, toGLAst y0, toGLAst z0, toGLAst w0, toGLAst u0]
toGLAst e@(GLFunc fnID (GLFunc6 f x y z w u v x0 y0 z0 w0 u0 v0)) = mkGLFn fnID (getID e) (f x y z w u v) [toGLAst x, toGLAst y, toGLAst z, toGLAst w, toGLAst u, toGLAst v] [toGLAst x0, toGLAst y0, toGLAst z0, toGLAst w0, toGLAst u0, toGLAst v0]

toGLAst e@(GLGenExpr id (GLVec2 x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLVec3 x y z)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLVec4 x y z w)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (GLMat2x2 x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLMat2x3 x y z)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLMat2x4 x y z w)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (GLMat3x2 x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLMat3x3 x y z)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLMat3x4 x y z w)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (GLMat4x2 x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GLMat4x3 x y z)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (GLMat4x4 x y z w)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y, toGLAst z, toGLAst w]
toGLAst e@(GLGenExpr id (Pre x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (App x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Conc x y)) = mkGLExpr id e (showGlslType e) [toGLAst x, toGLAst y]
-- FIXME: temporary patch-up to make printGLAST work
toGLAst e@(GLGenExpr id (GLArray _)) = mkGLExpr id e (showGlslType e) []

toGLAst e@(GLGenExpr id (OpCoord coord x)) = mkGLExpr id e ("." ++ show coord) [toGLAst x]
toGLAst e@(GLGenExpr id (OpCoordMulti coordList x)) = mkGLExpr id e ("." ++ show coordList) [toGLAst x]
toGLAst e@(GLGenExpr id (OpCol col x)) = mkGLExpr id e (show col) [toGLAst x]
toGLAst e@(GLGenExpr id (OpArrayElt arr i)) = mkGLExpr id e "[]" [toGLAst arr, toGLAst i]

toGLAst e@(GLGenExpr id (Cast x)) = mkGLExpr id e (showGlslType e) [toGLAst x]
toGLAst e@(GLGenExpr id (MatCast x)) = mkGLExpr id e (showGlslType e) [toGLAst x]

toGLAst e@(GLGenExpr id (OpAdd x y)) = mkGLExpr id e "+" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpSubt x y)) = mkGLExpr id e "-" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpMult x y)) = 
    let op = case showGlslType e of
            ty | "mat" `isPrefixOf` ty -> "matrixCompMult"
            ty | "dmat" `isPrefixOf` ty -> "matrixCompMult"
            _ -> "*"
    in mkGLExpr id e op [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpDiv x y)) = mkGLExpr id e "/" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpMod x y)) = mkGLExpr id e "%" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpNeg x)) = mkGLExpr id e "-" [toGLAst x]
toGLAst e@(GLGenExpr id (OpLessThan x y)) = mkGLExpr id e "<" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpLessThanEqual x y)) = mkGLExpr id e "<=" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpGreaterThan x y)) = mkGLExpr id e ">" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpGreaterThanEqual x y)) = mkGLExpr id e ">=" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpEqual x y)) = mkGLExpr id e "==" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpNotEqual x y)) = mkGLExpr id e "!=" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpAnd x y)) = mkGLExpr id e "&&" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpOr x y)) = mkGLExpr id e "||" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpXor x y)) = mkGLExpr id e "^^" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpNot x)) = mkGLExpr id e "!" [toGLAst x]
toGLAst e@(GLGenExpr id (OpCond x y z)) = mkGLExpr id e "?:" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (OpCompl x)) = mkGLExpr id e "~" [toGLAst x]
toGLAst e@(GLGenExpr id (OpLshift x y)) = mkGLExpr id e "<<" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpRshift x y)) = mkGLExpr id e ">>" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpBitAnd x y)) = mkGLExpr id e "&" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpBitOr x y)) = mkGLExpr id e "|" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpBitXor x y)) = mkGLExpr id e "^" [toGLAst x, toGLAst y]

toGLAst e@(GLGenExpr id (OpScalarMult x y)) = mkGLExpr id e "*" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OpMatrixMult x y)) = mkGLExpr id e "*" [toGLAst x, toGLAst y]

toGLAst e@(GLGenExpr id (Radians x)) = mkGLExpr id e "radians" [toGLAst x]
toGLAst e@(GLGenExpr id (Degrees x)) = mkGLExpr id e "degrees" [toGLAst x]
toGLAst e@(GLGenExpr id (Sin x)) = mkGLExpr id e "sin" [toGLAst x]
toGLAst e@(GLGenExpr id (Cos x)) = mkGLExpr id e "cos" [toGLAst x]
toGLAst e@(GLGenExpr id (Tan x)) = mkGLExpr id e "tan" [toGLAst x]
toGLAst e@(GLGenExpr id (Asin x)) = mkGLExpr id e "asin" [toGLAst x]
toGLAst e@(GLGenExpr id (Acos x)) = mkGLExpr id e "acos" [toGLAst x]
toGLAst e@(GLGenExpr id (Atan x)) = mkGLExpr id e "atan" [toGLAst x]
toGLAst e@(GLGenExpr id (Sinh x)) = mkGLExpr id e "sinh" [toGLAst x]
toGLAst e@(GLGenExpr id (Cosh x)) = mkGLExpr id e "cosh" [toGLAst x]
toGLAst e@(GLGenExpr id (Tanh x)) = mkGLExpr id e "tanh" [toGLAst x]
toGLAst e@(GLGenExpr id (Asinh x)) = mkGLExpr id e "asinh" [toGLAst x]
toGLAst e@(GLGenExpr id (Acosh x)) = mkGLExpr id e "acosh" [toGLAst x]
toGLAst e@(GLGenExpr id (Atanh x)) = mkGLExpr id e "atanh" [toGLAst x]

toGLAst e@(GLGenExpr id (Pow x y)) = mkGLExpr id e "pow" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Exp x)) = mkGLExpr id e "exp" [toGLAst x]
toGLAst e@(GLGenExpr id (Log x)) = mkGLExpr id e "log" [toGLAst x]
toGLAst e@(GLGenExpr id (Exp2 x)) = mkGLExpr id e "exp2" [toGLAst x]
toGLAst e@(GLGenExpr id (Log2 x)) = mkGLExpr id e "log2" [toGLAst x]
toGLAst e@(GLGenExpr id (Sqrt x)) = mkGLExpr id e "sqrt" [toGLAst x]
toGLAst e@(GLGenExpr id (Inversesqrt x)) = mkGLExpr id e "inversesqrt" [toGLAst x]

toGLAst e@(GLGenExpr id (Abs x)) = mkGLExpr id e "abs" [toGLAst x]
toGLAst e@(GLGenExpr id (Sign x)) = mkGLExpr id e "sign" [toGLAst x]
toGLAst e@(GLGenExpr id (Floor x)) = mkGLExpr id e "floor" [toGLAst x]
toGLAst e@(GLGenExpr id (Trunc x)) = mkGLExpr id e "trunc" [toGLAst x]
toGLAst e@(GLGenExpr id (Round x)) = mkGLExpr id e "round" [toGLAst x]
toGLAst e@(GLGenExpr id (RoundEven x)) = mkGLExpr id e "roundEven" [toGLAst x]
toGLAst e@(GLGenExpr id (Ceil x)) = mkGLExpr id e "ceil" [toGLAst x]
toGLAst e@(GLGenExpr id (Fract x)) = mkGLExpr id e "fract" [toGLAst x]
toGLAst e@(GLGenExpr id (Mod x y)) = mkGLExpr id e "mod" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Min x y)) = mkGLExpr id e "min" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Max x y)) = mkGLExpr id e "max" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Clamp x y z)) = mkGLExpr id e "clamp" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (Mix x y z)) = mkGLExpr id e "mix" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (Step x y)) = mkGLExpr id e "step" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Smoothstep x y z)) = mkGLExpr id e "smoothstep" [toGLAst x, toGLAst y, toGLAst z]

toGLAst e@(GLGenExpr id (Length x)) = mkGLExpr id e "length" [toGLAst x]
toGLAst e@(GLGenExpr id (Distance x y)) = mkGLExpr id e "distance" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Dot x y)) = mkGLExpr id e "dot" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Cross x y)) = mkGLExpr id e "cross" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Normalize x)) = mkGLExpr id e "normalize" [toGLAst x]
toGLAst e@(GLGenExpr id (Faceforward x y z)) = mkGLExpr id e "faceforward" [toGLAst x, toGLAst y, toGLAst z]
toGLAst e@(GLGenExpr id (Reflect x y)) = mkGLExpr id e "reflect" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Refract x y z)) = mkGLExpr id e "refract" [toGLAst x, toGLAst y, toGLAst z]

toGLAst e@(GLGenExpr id (MatrixCompMult x y)) = mkGLExpr id e "matrixCompMult" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (OuterProduct x y)) = mkGLExpr id e "outerProduct" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Transpose x)) = mkGLExpr id e "transpose" [toGLAst x]
toGLAst e@(GLGenExpr id (Determinant x)) = mkGLExpr id e "determinant" [toGLAst x]
toGLAst e@(GLGenExpr id (Inverse x)) = mkGLExpr id e "inverse" [toGLAst x]

toGLAst e@(GLGenExpr id (LessThan x y)) = mkGLExpr id e "lessThan" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (LessThanEqual x y)) = mkGLExpr id e "lessThanEqual" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GreaterThan x y)) = mkGLExpr id e "greaterThan" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (GreaterThanEqual x y)) = mkGLExpr id e "greaterThanEqual" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Equal x y)) = mkGLExpr id e "equal" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (NotEqual x y)) = mkGLExpr id e "notEqual" [toGLAst x, toGLAst y]
toGLAst e@(GLGenExpr id (Any x)) = mkGLExpr id e "any" [toGLAst x]
toGLAst e@(GLGenExpr id (All x)) = mkGLExpr id e "all" [toGLAst x]
toGLAst e@(GLGenExpr id (Not x)) = mkGLExpr id e "not" [toGLAst x]
