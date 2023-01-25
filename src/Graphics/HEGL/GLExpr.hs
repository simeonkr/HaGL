module Graphics.HEGL.GLExpr (
    GLExpr(..),
    GLExpr_(..),
    ShaderDomain(..),
    glExprId,
    HostVar(..),
    GLCoord(..),
    GLCoordList(..),
    GLCol(..)
) where

import GHC.TypeNats

import Graphics.HEGL.GLType
import Graphics.HEGL.Numerical
import Graphics.HEGL.ExprID (ExprID)
import Graphics.HEGL.Util.Types


-- * Expression definitions

data GLExpr :: ShaderDomain -> * -> * where
    -- This indirection helps us readily obtain the ID
    -- for an arbitrarily constructed expression
    GLExpr :: ExprID -> GLExpr_ d t -> GLExpr d t
    -- As an exception, these are defined here directly
    -- so as to enable partial application
    GLFunc1 :: (GLType t, GLType t1) =>
        ExprID -> GLExpr d t -> GLExpr d t1 -> 
                  GLExpr d t1 -> GLExpr d t
    GLFunc2 :: (GLType t, GLType t1, GLType t2) =>
        ExprID -> GLExpr d t -> GLExpr d t1 -> GLExpr d t2 -> 
                  GLExpr d t1 -> GLExpr d t2 -> GLExpr d t
    GLFunc3 :: (GLType t, GLType t1, GLType t2, GLType t3) =>
        ExprID -> GLExpr d t -> GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 ->
                  GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t

data GLExpr_ :: ShaderDomain -> * -> * where
    
    -- Special variables

    Const :: GLType t => 
        t -> GLExpr_ d t
    Uniform :: GLType t => 
        GLExpr HostDomain t -> GLExpr_ d t
    HostVar :: GLType t =>
        HostVar t -> GLExpr_ HostDomain t
    Inp :: GLPrim t => 
        [GLExpr ConstDomain t] -> GLExpr_ VertexDomain t
    Frag :: GLPrim t =>
        GLExpr VertexDomain t -> GLExpr_ FragmentDomain t
    FuncParam :: GLType t => GLExpr_ d t

    -- Ordinary expressions

    GLVec2 :: (GLPrim t, GLType (Vec 2 t)) =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d (Vec 2 t)
    GLVec3 :: (GLPrim t, GLType (Vec 3 t)) =>
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr_ d (Vec 3 t)
    GLVec4 :: (GLPrim t, GLType (Vec 4 t)) =>
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr_ d (Vec 4 t)
    GLMat2x2 :: (GLPrim t, GLType (Vec 2 t), GLType (Mat 2 2 t)) =>
        GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t) -> GLExpr_ d (Mat 2 2 t)
    GLMat3x3 :: (GLPrim t, GLType (Vec 3 t), GLType (Mat 3 3 t)) =>
        GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr_ d (Mat 3 3 t)
    GLMat4x4 :: (GLPrim t, GLType (Vec 4 t), GLType (Mat 4 4 t)) =>
        GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLExpr d (Vec 4 t) -> GLExpr_ d (Mat 4 4 t)
    Pre :: (GLPrim t, GLType (Vec n t), GLType (Vec (n + 1) t)) => 
        GLExpr d t -> GLExpr d (Vec n t) -> GLExpr_ d (Vec (n + 1) t)
    App :: (GLPrim t, GLType (Vec n t), GLType t, GLType (Vec (n + 1) t)) => 
        GLExpr d (Vec n t) -> GLExpr d t -> GLExpr_ d (Vec (n + 1) t)
    Conc :: (GLPrim t, GLType (Vec m t), GLType (Vec n t), GLType (Vec (m + n) t)) => 
        GLExpr d (Vec m t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec (m + n) t)
    HorConc :: (GLPrim t, GLType (Mat p q1 t), GLType (Mat p q2 t), GLType (Mat p (q1 + q2) t)) => 
        GLExpr d (Mat p q1 t) -> GLExpr d (Mat p q2 t) -> GLExpr_ d (Mat p (q1 + q2) t)
    GLArray :: (GLPrim t, GLType [t]) =>
        [GLExpr HostDomain t] -> GLExpr_ HostDomain [t]

    OpArrayElt :: (GLType [t], GLType t) =>
        GLExpr d [t] -> GLExpr d Int -> GLExpr_ d t
    OpCoord :: (GLPrim t, GLType (Vec n t), m <= n) =>
        GLCoord m -> GLExpr d (Vec n t) -> GLExpr_ d t
    OpCoordMulti :: (GLPrim t, GLType (Vec n t), GLType (Vec l t), m <= n) =>
        GLCoordList l m -> GLExpr d (Vec n t) -> GLExpr_ d (Vec l t)
    OpCol :: (GLPrim t, GLType (Mat r c t), GLType (Vec r t), m <= c) =>
        GLCol m -> GLExpr d (Mat r c t) -> GLExpr_ d (Vec r t)

    -- TODO: can these be defined on Mats?
    ToFloat :: GLPrim t =>
        GLExpr d t -> GLExpr_ d Float
    ToInt :: GLPrim t =>
        GLExpr d t -> GLExpr_ d Int
    ToUInt :: GLPrim t =>
        GLExpr d t -> GLExpr_ d UInt

    OpAdd :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpSubt :: (GLNumeric (Elt t), GLType t) =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpMult :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpDiv :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpMod :: (GLInteger (Elt t), GLType t) =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpNeg :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    OpLessThan :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d Bool
    OpLessThanEqual :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d Bool
    OpGreaterThan :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d Bool
    OpGreaterThanEqual :: GLNumeric t =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d Bool
    OpEqual :: GLType t =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d Bool
    OpNotEqual :: GLType t =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d Bool
    OpAnd :: GLExpr d Bool -> GLExpr d Bool -> GLExpr_ d Bool
    OpOr :: GLExpr d Bool -> GLExpr d Bool -> GLExpr_ d Bool
    OpXor :: GLExpr d Bool -> GLExpr d Bool -> GLExpr_ d Bool
    OpNot :: GLExpr d Bool -> GLExpr_ d Bool
    OpCond :: GLType t =>
        GLExpr d Bool -> GLExpr d t -> GLExpr d t -> GLExpr_ d t
    -- TODO: these should not be defined for non-vector matrices
    OpCompl :: (GLInteger (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    OpLshift :: (GLInteger (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpRshift :: (GLInteger (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpBitAnd :: (GLInteger (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpBitOr :: (GLInteger (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpBitXor :: (GLInteger (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    OpScalarMult :: (GLNumeric t, GLType (Mat p q t)) => 
        GLExpr d t -> GLExpr d (Mat p q t) -> GLExpr_ d (Mat p q t)
    OpMatrixMult :: (GLFloating t, GLType (Mat p q t), GLType (Mat q r t), GLType (Mat p r t)) => 
        GLExpr d (Mat p q t) -> GLExpr d (Mat q r t) -> GLExpr_ d (Mat p r t)

    Radians :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Degrees :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Sin :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Cos :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Tan :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Asin :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Acos :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Atan :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t

    Pow :: (Elt t ~ Float, GLType t) =>
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Exp :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Log :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Exp2 :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Log2 :: (Elt t ~ Float, GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Sqrt :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    -- TODO: missing Inversesqrt

    Abs :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Sign :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Floor :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Trunc :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Round :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    RoundEven :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Ceil :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Fract :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr_ d t
    Mod :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Min :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Max :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Clamp :: (GLNumeric (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Mix :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Step :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr_ d t
    Smoothstep :: (GLFloating (Elt t), GLType t) => 
        GLExpr d t -> GLExpr d t -> GLExpr d t -> GLExpr_ d t

    Length :: GLFloating t =>
        GLExpr d (Vec n t) -> GLExpr_ d t
    Distance :: GLFloating t =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d t
    Dot :: (GLFloating t, GLType (Vec n t))  =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d t
    Cross :: (GLFloating t, GLType (Vec 3 t)) =>
        GLExpr d (Vec 3 t) -> GLExpr d (Vec 3 t) -> GLExpr_ d (Vec 3 t)
    Normalize :: (GLFloating t, GLType (Vec n t))=>
        GLExpr d (Vec n t) -> GLExpr_ d (Vec n t)
    Faceforward :: (GLFloating t, KnownNat n, GLType (Vec n t)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n t)
    Reflect :: (GLFloating t, KnownNat n, GLType (Vec n t)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n t)
    Refract :: (GLFloating t, KnownNat n, GLType (Vec n t), GLType t) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr d t -> GLExpr_ d (Vec n t)

    MatrixCompMult :: (GLFloating t, GLType (Mat p q t), GLType (Mat p q t)) => 
        GLExpr d (Mat p q t) -> GLExpr d (Mat p q t) -> GLExpr_ d (Mat p q t)
    OuterProduct :: (GLFloating t, GLType (Vec q t), GLType (Mat p q t)) => 
        GLExpr d (Vec p t) -> GLExpr d (Vec q t) -> GLExpr_ d (Mat p q t)
    Transpose :: (GLFloating t, GLType (Mat p q t), GLType (Mat q p t)) => 
        GLExpr d (Mat p q t) -> GLExpr_ d (Mat q p t)
    Determinant :: GLType (Mat p p Float) => 
        GLExpr d (Mat p p Float) -> GLExpr_ d Float
    Inverse :: GLType (Mat p p Float) => 
        GLExpr d (Mat p p Float) -> GLExpr_ d (Mat p p Float)

    LessThan :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n Bool)
    LessThanEqual :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n Bool)
    GreaterThan :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n Bool)
    GreaterThanEqual :: (GLSingleNumeric t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n Bool)
    Equal :: (GLSingle t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n Bool)
    NotEqual :: (GLSingle t, KnownNat n, GLType (Vec n Bool)) =>
        GLExpr d (Vec n t) -> GLExpr d (Vec n t) -> GLExpr_ d (Vec n Bool)
    Any :: GLType (Vec n Bool) =>
        GLExpr d (Vec n Bool) -> GLExpr_ d Bool
    All :: GLType (Vec n Bool) =>
        GLExpr d (Vec n Bool) -> GLExpr_ d Bool
    Compl :: GLType (Vec n Bool) =>
        GLExpr d (Vec n Bool) -> GLExpr_ d (Vec n Bool)


data ShaderDomain = ConstDomain | HostDomain | VertexDomain | FragmentDomain
    deriving (Eq, Ord)

glExprId :: GLExpr d t -> ExprID
glExprId (GLExpr id _) = id
glExprId (GLFunc1 id _ _ _ ) = id
glExprId (GLFunc2 id _ _ _ _ _ ) = id
glExprId (GLFunc3 id _ _ _ _ _ _ _ ) = id


-- * Internal auxillary types

data HostVar t where
    IOFloat :: ExprID -> HostVar Float
    IODouble :: ExprID -> HostVar Double
    IOInt :: ExprID -> HostVar Int
    IOUInt :: ExprID -> HostVar UInt
    IOBool :: ExprID -> HostVar Bool
    HostPrec :: GLType t => ExprID -> GLExpr HostDomain t -> GLExpr HostDomain t -> HostVar t
    GLLift0 :: GLType t =>
        ExprID -> t -> HostVar t 
    GLLift1 :: (GLType t, GLType t1) =>
        ExprID -> (t1 -> t) -> GLExpr HostDomain t1 -> HostVar t
    GLLift2 :: (GLType t, GLType t1, GLType t2) =>
        ExprID -> (t1 -> t2 -> t) -> GLExpr HostDomain t1 -> GLExpr HostDomain t2 -> HostVar t

hostVarId :: HostVar t -> ExprID
hostVarId (IOFloat id) = id
hostVarId (IODouble id) = id
hostVarId (IOInt id) = id
hostVarId (IOUInt id) = id
hostVarId (HostPrec id _ _) = id
hostVarId (GLLift0 id _) = id
hostVarId (GLLift1 id _ _) = id
hostVarId (GLLift2 id _ _ _) = id

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
