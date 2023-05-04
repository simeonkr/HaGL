module Graphics.HEGL.GLExpr (
    GLExpr(..),
    GLAtom(..),
    GLFunc(..),
    GLGenExpr(..),
    ShaderDomain(..),
    shaderDomains,
    InterpolationType(..),
    IOVarID,
    GLCoord(..),
    GLCoordList(..),
    GLCol(..),
    GLExprException(..),
    ConstExpr,
    HostExpr,
    VertExpr,
    FragExpr
) where

import Prelude hiding (id)
import GHC.TypeNats
import Control.Exception (Exception)

import Graphics.HEGL.GLType
import Graphics.HEGL.Numerical
import Graphics.HEGL.ExprID
import Graphics.HEGL.Util.Types
import qualified Graphics.HEGL.Util.DepMap as DepMap


-- * Expression definitions

data GLExpr :: ShaderDomain -> * -> * where
    GLAtom :: GLType t => ExprID -> GLAtom d t -> GLExpr d t
    GLFunc :: GLType t => ExprID -> GLFunc d t -> GLExpr d t
    GLGenExpr :: GLType t => ExprID -> GLGenExpr d t -> GLExpr d t


-- Irreducible variables and placeholders

data GLAtom :: ShaderDomain -> * -> * where

    Const :: GLType t => 
        t -> GLAtom d t
    -- Generic variables; e.g., function parameters
    GenVar :: GLType t => GLAtom d t
    Uniform :: GLType t => 
        GLExpr HostDomain t -> GLAtom d t
    Inp :: GLInputType t => 
        [GLExpr ConstDomain t] -> GLAtom VertexDomain t
    Frag :: GLInputType t =>
        InterpolationType -> GLExpr VertexDomain t -> GLAtom FragmentDomain t

    -- IO variables and placeholders exclusive to HostDomain
    IOFloat :: IOVarID -> GLAtom HostDomain Float
    IODouble :: IOVarID -> GLAtom HostDomain Double
    IOInt :: IOVarID -> GLAtom HostDomain Int
    IOUInt :: IOVarID -> GLAtom HostDomain UInt
    IOBool :: IOVarID -> GLAtom HostDomain Bool
    IOPrec :: GLType t => GLExpr HostDomain t -> GLExpr HostDomain t -> GLAtom HostDomain t
    GLLift0 :: GLType t =>
        t -> GLAtom HostDomain t 
    GLLift1 :: (GLType t, GLType t1) =>
        (t1 -> t) -> GLExpr HostDomain t1 -> GLAtom HostDomain t
    GLLift2 :: (GLType t, GLType t1, GLType t2) =>
        (t1 -> t2 -> t) -> GLExpr HostDomain t1 -> GLExpr HostDomain t2 -> GLAtom HostDomain t
    GLLift3 :: (GLType t, GLType t1, GLType t2, GLType t3) =>
        (t1 -> t2 -> t3 -> t) -> GLExpr HostDomain t1 -> GLExpr HostDomain t2 -> GLExpr HostDomain t3 -> GLAtom HostDomain t
    GLLift4 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4) =>
        (t1 -> t2 -> t3 -> t4 -> t) -> GLExpr HostDomain t1 -> GLExpr HostDomain t2 -> GLExpr HostDomain t3 -> GLExpr HostDomain t4 -> GLAtom HostDomain t
    GLLift5 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5) =>
        (t1 -> t2 -> t3 -> t4 -> t5 -> t) -> GLExpr HostDomain t1 -> GLExpr HostDomain t2 -> GLExpr HostDomain t3 -> GLExpr HostDomain t4 -> GLExpr HostDomain t5 -> GLAtom HostDomain t
    GLLift6 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5, GLType t6) =>
        (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t) -> GLExpr HostDomain t1 -> GLExpr HostDomain t2 -> GLExpr HostDomain t3 -> GLExpr HostDomain t4 -> GLExpr HostDomain t5 -> GLExpr HostDomain t6 -> GLAtom HostDomain t


-- User-defined functions

data GLFunc :: ShaderDomain -> * -> * where

    GLFunc1 :: (GLType t, GLType t1) =>
        (GLExpr d t1 -> GLExpr d t) ->
        GLExpr d t1 -> GLExpr d t1 -> GLFunc d t
    GLFunc2 :: (GLType t, GLType t1, GLType t2) =>
        (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t) ->
        GLExpr d t1 -> GLExpr d t2 -> 
        GLExpr d t1 -> GLExpr d t2 -> GLFunc d t
    GLFunc3 :: (GLType t, GLType t1, GLType t2, GLType t3) =>
        (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t) ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLFunc d t
    GLFunc4 :: (GLType t, GLType t1, GLType t2, GLType t3) =>
        (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t) ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLFunc d t
    GLFunc5 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5) =>
        (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t) ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLFunc d t
    GLFunc6 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5, GLType t6) =>
        (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 -> GLExpr d t) ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 ->
        GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 -> GLFunc d t


-- Compound expressions corresponding to built-in functions and operators
-- TODO: order constraints in a consistent manner

data GLGenExpr :: ShaderDomain -> * -> * where

    GLVec2 :: (GLPrim t, GLType (Vec 2 t)) =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d (Vec 2 t)
    GLVec3 :: (GLPrim t, GLType (Vec 3 t)) =>
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLGenExpr d (Vec 3 t)
    GLVec4 :: (GLPrim t, GLType (Vec 4 t)) =>
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLGenExpr d (Vec 4 t)
    GLMat2x2 :: (GLFloating t, GLType (Vec 2 t), GLType (Mat 2 2 t)) =>
        GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLGenExpr d (Mat 2 2 t)
    GLMat2x3 :: (GLFloating t, GLType (Vec 2 t), GLType (Mat 2 3 t)) =>
        GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLGenExpr d (Mat 2 3 t)
    GLMat2x4 :: (GLFloating t, GLType (Vec 2 t), GLType (Mat 2 4 t)) =>
        GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLGenExpr d (Mat 2 4 t)
    GLMat3x2 :: (GLFloating t, GLType (Vec 3 t), GLType (Mat 3 2 t)) =>
        GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLGenExpr d (Mat 3 2 t)
    GLMat3x3 :: (GLFloating t, GLType (Vec 3 t), GLType (Mat 3 3 t)) =>
        GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLGenExpr d (Mat 3 3 t)
    GLMat3x4 :: (GLFloating t, GLType (Vec 3 t), GLType (Mat 3 4 t)) =>
        GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLGenExpr d (Mat 3 4 t)
    GLMat4x2 :: (GLFloating t, GLType (Vec 4 t), GLType (Mat 4 2 t)) =>
        GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLGenExpr d (Mat 4 2 t)
    GLMat4x3 :: (GLFloating t, GLType (Vec 4 t), GLType (Mat 4 3 t)) =>
        GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLGenExpr d (Mat 4 3 t)
    GLMat4x4 :: (GLFloating t, GLType (Vec 4 t), GLType (Mat 4 4 t)) =>
        GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLGenExpr d (Mat 4 4 t)
    Pre :: (GLPrim t, GLType (Vec n t), GLType (Vec (n + 1) t)) => 
        GLExpr d t -> GLExpr d (Vec n t) -> GLGenExpr d (Vec (n + 1) t)
    App :: (GLPrim t, GLType (Vec n t), GLType t, GLType (Vec (n + 1) t)) => 
        GLExpr d (Vec n t) -> GLExpr d t -> GLGenExpr d (Vec (n + 1) t)
    Conc :: (GLPrim t, GLType (Vec m t), GLType (Vec n t), GLType (Vec (m + n) t)) => 
        GLExpr d (Vec m t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec (m + n) t)
    GLArray :: GLType [t] =>
        [GLExpr HostDomain t] -> GLGenExpr HostDomain [t]

    OpCoord :: (GLPrim t, GLType (Vec n t), m <= n) =>
        GLCoord m -> GLExpr d (Vec n t) -> GLGenExpr d t
    OpCoordMulti :: (GLPrim t, GLType (Vec n t), GLType (Vec l t), m <= n) =>
        GLCoordList l m -> GLExpr d (Vec n t) -> GLGenExpr d (Vec l t)
    OpCol :: (GLPrim t, GLType (Mat r c t), GLType (Vec r t), m + 1 <= c) =>
        GLCol m -> GLExpr d (Mat r c t) -> GLGenExpr d (Vec r t)
    OpArrayElt :: (GLType [t], GLType t) =>
        GLExpr d [t] -> GLExpr d Int -> GLGenExpr d t

    Cast :: (GLPrim t1, GLPrim t2) => 
        GLExpr d t1 -> GLGenExpr d t2
    MatCast :: (GLPrim t1, GLPrim t2, KnownNat p, KnownNat q) => 
        GLExpr d (Mat p q t1) -> GLGenExpr d (Mat p q t2)

    OpAdd :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpSubt :: (GLNumeric (GLElt t), GLType t) =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpMult :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpDiv :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpMod :: (GLInteger (GLElt t), GLType t) =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpNeg :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    OpLessThan :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d Bool
    OpLessThanEqual :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d Bool
    OpGreaterThan :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d Bool
    OpGreaterThanEqual :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d Bool
    OpEqual :: GLType t =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d Bool
    OpNotEqual :: GLType t =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d Bool
    OpAnd :: GLExpr d Bool -> GLExpr d Bool -> GLGenExpr d Bool
    OpOr :: GLExpr d Bool -> GLExpr d Bool -> GLGenExpr d Bool
    OpXor :: GLExpr d Bool -> GLExpr d Bool -> GLGenExpr d Bool
    OpNot :: GLExpr d Bool -> GLGenExpr d Bool
    OpCond :: GLType t =>
        GLExpr d Bool -> GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpCompl :: GLSupportsBitwiseOps t => 
        GLExpr d t -> GLGenExpr d t
    OpLshift :: GLSupportsBitwiseOps t => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpRshift :: GLSupportsBitwiseOps t => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpBitAnd :: GLSupportsBitwiseOps t => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpBitOr :: GLSupportsBitwiseOps t => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpBitXor :: GLSupportsBitwiseOps t => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    OpScalarMult :: (GLNumeric t, GLType (Mat p q t)) => 
        GLExpr d t -> GLExpr d (Mat p q t) -> GLGenExpr d (Mat p q t)
    OpMatrixMult :: (GLFloating t, GLType (Mat p q t), GLType (Mat q r t), GLType (Mat p r t)) => 
        GLExpr d (Mat p q t) -> GLExpr d (Mat q r t) -> GLGenExpr d (Mat p r t)

    Radians :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Degrees :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Sin :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Cos :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Tan :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Asin :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Acos :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Atan :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Sinh :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Cosh :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Tanh :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Asinh :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Acosh :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Atanh :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t

    Pow :: (GLElt t ~ Float, GLType t) =>
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Exp :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Log :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Exp2 :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Log2 :: (GLElt t ~ Float, GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Sqrt :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Inversesqrt :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t

    Abs :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Sign :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Floor :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Trunc :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Round :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    RoundEven :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Ceil :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Fract :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLGenExpr d t
    Mod :: (GLFloating (GLElt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Min :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Max :: (GLNumeric (GLElt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Clamp :: (GLNumeric (GLElt t), GLPrimOrVec t) => 
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Mix :: (GLFloating (GLElt t), GLPrimOrVec t) => 
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Step :: (GLFloating (GLElt t), GLPrimOrVec t) => 
        GLExpr d t -> GLExpr d t -> GLGenExpr d t
    Smoothstep :: (GLFloating (GLElt t), GLPrimOrVec t) => 
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLGenExpr d t

    Length :: GLFloating t =>
        GLExpr d (Vec n t) -> GLGenExpr d t
    Distance :: GLFloating t =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d t
    Dot :: (GLFloating t, GLType (Vec n t))  =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d t
    Cross :: (GLFloating t, GLType (Vec 3 t)) =>
        GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLGenExpr d (Vec 3 t)
    Normalize :: (GLFloating t, GLType (Vec n t))=>
        GLExpr d (Vec n t) -> GLGenExpr d (Vec n t)
    Faceforward :: (GLFloating t, KnownNat n, GLType (Vec n t)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n t)
    Reflect :: (GLFloating t, KnownNat n, GLType (Vec n t)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n t)
    Refract :: (GLFloating t, KnownNat n, GLType (Vec n t), GLType t) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr d t -> GLGenExpr d (Vec n t)

    MatrixCompMult :: (GLFloating t, GLType (Mat p q t), GLType (Mat p q t)) => 
        GLExpr d (Mat p q t) -> GLExpr d (Mat p q t) -> GLGenExpr d (Mat p q t)
    OuterProduct :: (GLFloating t, GLType (Vec q t), GLType (Mat p q t)) => 
        GLExpr d (Vec p t) -> GLExpr d (Vec q t) -> GLGenExpr d (Mat p q t)
    Transpose :: (GLFloating t, GLType (Mat p q t), GLType (Mat q p t)) => 
        GLExpr d (Mat p q t) -> GLGenExpr d (Mat q p t)
    Determinant :: GLType (Mat p p Float) => 
        GLExpr d (Mat p p Float) -> GLGenExpr d Float
    Inverse :: GLType (Mat p p Float) => 
        GLExpr d (Mat p p Float) -> GLGenExpr d (Mat p p Float)

    LessThan :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n Bool)
    LessThanEqual :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n Bool)
    GreaterThan :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n Bool)
    GreaterThanEqual :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n Bool)
    Equal :: (GLSingle t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n Bool)
    NotEqual :: (GLSingle t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLGenExpr d (Vec n Bool)
    Any :: GLType (Vec n Bool) =>
        GLExpr d (Vec n Bool) -> GLGenExpr d Bool
    All :: GLType (Vec n Bool) =>
        GLExpr d (Vec n Bool) -> GLGenExpr d Bool
    Compl :: GLType (Vec n Bool) =>
        GLExpr d (Vec n Bool) -> GLGenExpr d (Vec n Bool)


-- TODO: rename to GLDomain as not all domains relate to shaders
data ShaderDomain = ConstDomain | HostDomain | VertexDomain | FragmentDomain
    deriving (Eq, Ord)

shaderDomains :: [ShaderDomain]
shaderDomains = [VertexDomain, FragmentDomain]


-- * Internal auxillary types

data InterpolationType = Smooth | NoPerspective | Flat

instance Show InterpolationType where
    show Smooth = "smooth"
    show NoPerspective = "noperspective"
    show Flat = "flat"

type IOVarID = String

data GLCoord (m :: Nat) where
    CoordX :: GLCoord 1
    CoordY :: GLCoord 2
    CoordZ :: GLCoord 3
    CoordW :: GLCoord 4

data GLCoordList (l :: Nat) (m :: Nat) where
    CoordNil :: GLCoordList 0 0
    CoordCons :: GLCoord m1 -> GLCoordList l2 m2 -> GLCoordList (l2 + 1) (Max m1 m2)

instance Show (GLCoord m) where
    show CoordX = "x"
    show CoordY = "y"
    show CoordZ = "z"
    show CoordW = "w"

instance Show (GLCoordList l m) where
    show CoordNil = ""
    show (CoordCons c cs) = show c ++ show cs

data GLCol (m :: Nat) where
    Col0 :: GLCol 0
    Col1 :: GLCol 1
    Col2 :: GLCol 2
    Col3 :: GLCol 3

instance Show (GLCol m) where
    show Col0 = "[0]"
    show Col1 = "[1]"
    show Col2 = "[2]"
    show Col3 = "[3]"


-- * glExprID

instance HasExprID (GLExpr d t) where
    getID (GLAtom id _) = id
    getID (GLFunc fnID (GLFunc1 _ _ x0)) = 
        combineIDs [fnID, getID x0]
    getID (GLFunc fnID (GLFunc2 _ _ _ x0 y0)) = 
        combineIDs [fnID, getID x0, getID y0]
    getID (GLFunc fnID (GLFunc3 _ _ _ _ x0 y0 z0)) = 
        combineIDs [fnID, getID x0, getID y0, getID z0]
    getID (GLFunc fnID (GLFunc4 _ _ _ _ _ x0 y0 z0 w0)) = 
        combineIDs [fnID, getID x0, getID y0, getID z0, getID w0]
    getID (GLFunc fnID (GLFunc5 _ _ _ _ _ _ x0 y0 z0 w0 u0)) = 
        combineIDs [fnID, getID x0, getID y0, getID z0, getID w0, getID u0]
    getID (GLFunc fnID (GLFunc6 _ _ _ _ _ _ _ x0 y0 z0 w0 u0 v0)) = 
        combineIDs [fnID, getID x0, getID y0, getID z0, getID w0, getID u0, getID v0]
    getID (GLGenExpr id _) = id

instance DepMap.GenHashable (GLExpr d) where
    genHash = getID


-- * Exceptions for illegal expressions that are difficult to disallow at the type level

data GLExprException =
    UnsupportedRecCall |
    UnknownArraySize
    deriving Eq

instance Show GLExprException where
    show UnsupportedRecCall = "Unsupported recursive function call"
    show UnknownArraySize = "GLLift*: Function may only return a fixed-size list"

instance Exception GLExprException


-- * Synonyms

type ConstExpr = GLExpr ConstDomain
type HostExpr = GLExpr HostDomain
type VertExpr = GLExpr VertexDomain
type FragExpr = GLExpr FragmentDomain
