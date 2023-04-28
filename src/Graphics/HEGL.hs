{-|
Module      : HEGL
Copyright   : (c) Simeon Krastnikov, 2022
License     : MIT
Maintainer  : skrastnikov@gmail.com
Stability   : experimental
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dth-dec-file #-}
-- required due to use of genID
{-# OPTIONS_GHC -fno-full-laziness #-}

module Graphics.HEGL (
    -- * Raw types
    UInt,
    Mat,
    Vec,
    RowVec,
    -- ** Raw vector/matrix constructors
    -- | Usually only useful for loading data from externally computed arrays via lifts.
    fromMapping,
    fromList,
    -- ** Classes
    GLType, GLInputType, GLElt,
    GLPrim, GLSingle, GLNumeric, GLFloating, GLSingleNumeric, GLInteger,
    GLSupportsSmoothInterp, GLSupportsBitwiseOps,
    -- * Expressions: Main definitions
    GLExpr,
    ShaderDomain(..),
    ConstExpr,
    HostExpr,
    VertExpr,
    FragExpr,
    -- * Expressions: Lifts from raw types
    glLift0, 
    glLift1, 
    glLift2,
    glLift3,
    glLift4,
    glLift5,
    glLift6,
    -- * Generic constructors
    Graphics.HEGL.const,
    true,
    false,
    uniform,
    prec,
    vert,
    frag,
    noperspFrag,
    flatFrag,
    -- * Vector, matrix, and array constructors
    vec2, vec3, vec4,
    mat2, mat3, mat4,
    mat2x2, mat2x3, mat2x4,
    mat3x2, mat3x3, mat3x4,
    mat4x2, mat4x3, mat4x4,
    pre,
    app,
    ($-),
    ($|),
    array,
    -- * Deconstruction and indexing
    Deconstructible(..),
    x_, y_, z_, w_,
    xy_, xz_, xw_,
    yx_, yz_, yw_,
    zx_, zy_, zw_,
    wx_, wy_, wz_,
    xyz_, xyw_, xzy_, xzw_, xwy_, xwz_,
    yxz_, yxw_, yzx_, yzw_, ywx_, ywz_,
    zxy_, zxw_, zyx_, zyw_, zwx_, zwy_,
    wxy_, wxz_, wyx_, wyz_, wzx_, wzy_,
    col0, col1, col2, col3,
    (.!),
    -- * Type conversion
    Graphics.HEGL.cast,
    matCast,
    -- * Custom function support
    glFunc1,
    glFunc2,
    glFunc3,
    glFunc4,
    glFunc5,
    glFunc6,
    -- * Builtin operators and functions
    -- ** Numeric operators
    (./),
    (.%),
    (.*),
    (.@),
    -- ** Boolean operators and comparison functions
    (.<),
    (.<=),
    (.>),
    (.>=),
    (.==),
    (./=),
    (.&&),
    (.||),
    (.^^),
    Graphics.HEGL.not,
    cond,
    -- ** Bitwise operators
    (.<<),
    (.>>),
    (.&),
    (.|),
    (.^),
    neg,
    -- ** Angle and trigonometry functions
    radians,
    degrees,
    Graphics.HEGL.sin,
    Graphics.HEGL.cos,
    Graphics.HEGL.tan,
    Graphics.HEGL.asin,
    Graphics.HEGL.acos,
    Graphics.HEGL.atan,
    Graphics.HEGL.sinh,
    Graphics.HEGL.cosh,
    Graphics.HEGL.tanh,
    Graphics.HEGL.asinh,
    Graphics.HEGL.acosh,
    Graphics.HEGL.atanh,
    -- ** Exponential functions
    pow,
    Graphics.HEGL.exp,
    Graphics.HEGL.log,
    exp2,
    log2,
    Graphics.HEGL.sqrt,
    inversesqrt,
    -- ** Common functions
    Graphics.HEGL.floor,
    trunc,
    Graphics.HEGL.round,
    roundEven,
    ceil,
    fract,
    Graphics.HEGL.mod,
    Graphics.HEGL.min,
    Graphics.HEGL.max,
    clamp,
    mix,
    step,
    smoothstep,
    -- ** Geometric functions
    Graphics.HEGL.length,
    distance,
    dot,
    cross,
    normalize,
    faceforward,
    reflect,
    refract,
    -- ** Matrix functions
    matrixCompMult,
    outerProduct,
    transpose,
    determinant,
    inverse,
    -- ** Vector relational functions
    lessThan,
    lessThanEqual,
    greaterThan,
    greaterThanEqual,
    equal,
    notEqual,
    Graphics.HEGL.any,
    Graphics.HEGL.all,
    compl,
    -- * Builtin I/O variables
    time,
    mouseLeft,
    mouseRight,
    mouseWheel,
    mouseX,
    mouseY,
    mousePos,
    -- * Drawables
    Drawable(..),
    GLObj(..),
    PrimitiveMode(..),
    points,
    Graphics.HEGL.lines,
    lineLoop,
    lineStrip,
    triangles,
    triangleStrip,
    triangleFan,
    quads,
    quadStrip,
    polygon,
    -- * Backends
    Backend(..),
    GlutOptions(..),
    GlutRunMode(..),
    drawGlut,
    drawGlutCustom,
    defaultGlutOptions,
) where

import qualified Graphics.Rendering.OpenGL as OpenGL

import Graphics.HEGL.TH.HEGL (gen2DCoordDecls, gen3DCoordDecls)
import Graphics.HEGL.Numerical (Mat, Vec, RowVec, fromMapping, fromList)
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID (genID)
import Graphics.HEGL.GLObj
import Graphics.HEGL.Eval
import Graphics.HEGL.Backend.GLUT


{-# NOINLINE mkExpr #-}
mkExpr con e = con (genID e) e


-- * Expressions: Main definitions

instance GLPrim t => Enum (ConstExpr t) where
    toEnum x = mkExpr GLAtom $ Const (toEnum x)
    fromEnum = fromEnum . constEval

instance (GLSigned (GLElt t), GLType t, Num t) => Num (GLExpr d t) where
    x + y = mkExpr GLGenExpr $ OpAdd x y
    x - y = mkExpr GLGenExpr $ OpSubt x y
    x * y = mkExpr GLGenExpr $ OpMult x y
    negate x = mkExpr GLGenExpr $ OpNeg x
    abs x = mkExpr GLGenExpr $ Abs x
    signum x = mkExpr GLGenExpr $ Sign x
    fromInteger x = mkExpr GLAtom $ Const (fromInteger x)

instance {-# OVERLAPPING #-} Num (GLExpr d UInt) where
    x + y = mkExpr GLGenExpr $ OpAdd x y
    x - y = mkExpr GLGenExpr $ OpSubt x y
    x * y = mkExpr GLGenExpr $ OpMult x y
    negate x = mkExpr GLGenExpr $ OpCompl x
    abs x = x
    signum x = mkExpr GLGenExpr $ Cast $ 
        mkExpr GLGenExpr $ OpGreaterThan x (mkExpr GLAtom $ Const 0)
    fromInteger x = mkExpr GLAtom $ Const (fromInteger x)

instance (GLFloating (GLElt t), GLType t, Fractional t) => Fractional (GLExpr d t) where
    x / y = mkExpr GLGenExpr $ OpDiv x y
    fromRational x = mkExpr GLAtom $ Const (fromRational x)

instance Floating (GLExpr d Float) where
    pi = 3.141592653589793238
    sin x = mkExpr GLGenExpr $ Sin x
    cos x = mkExpr GLGenExpr $ Cos x
    tan x = mkExpr GLGenExpr $ Tan x
    asin x = mkExpr GLGenExpr $ Asin x
    acos x = mkExpr GLGenExpr $ Acos x
    atan x = mkExpr GLGenExpr $ Atan x
    sinh x = mkExpr GLGenExpr $ Sinh x
    cosh x = mkExpr GLGenExpr $ Cosh x
    tanh x = mkExpr GLGenExpr $ Tanh x
    asinh x = mkExpr GLGenExpr $ Asinh x
    acosh x = mkExpr GLGenExpr $ Acosh x
    atanh x = mkExpr GLGenExpr $ Atanh x
    x ** y = mkExpr GLGenExpr $ Pow x y
    exp x = mkExpr GLGenExpr $ Exp x
    sqrt x = mkExpr GLGenExpr $ Sqrt x
    log x = mkExpr GLGenExpr $ Log x


-- * Expressions: Lifts from raw types

glLift0 x = mkExpr GLAtom $ GLLift0 x
glLift1 f x = mkExpr GLAtom $ GLLift1 f x
glLift2 f x y = mkExpr GLAtom $ GLLift2 f x y
glLift3 f x y z = mkExpr GLAtom $ GLLift3 f x y z
glLift4 f x y z w = mkExpr GLAtom $ GLLift4 f x y z w
glLift5 f x y z w u = mkExpr GLAtom $ GLLift5 f x y z w u
glLift6 f x y z w u v = mkExpr GLAtom $ GLLift6 f x y z w u v


-- * Generic expression constructors

const :: GLType t => ConstExpr t -> GLExpr d t
const x = mkExpr GLAtom $ Const (constEval x)

true, false :: GLExpr d Bool
true = Graphics.HEGL.const $ toEnum . fromEnum $ 1
false = Graphics.HEGL.const $ toEnum . fromEnum $ 0

uniform :: GLType t => HostExpr t -> GLExpr d t
uniform x = mkExpr GLAtom $ Uniform x

prec :: GLType t => HostExpr t -> HostExpr t -> HostExpr t
prec x0 x = mkExpr GLAtom $ IOPrec x0 x

vert :: GLInputType t => [ConstExpr t] -> VertExpr t
vert inp = mkExpr GLAtom $ Inp inp

frag :: GLSupportsSmoothInterp t => VertExpr t -> FragExpr t
frag x = mkExpr GLAtom $ Frag Smooth x

noperspFrag :: GLSupportsSmoothInterp t => GLInputType t => VertExpr t -> FragExpr t
noperspFrag x = mkExpr GLAtom $ Frag NoPerspective x

flatFrag :: GLInputType t => VertExpr t -> FragExpr t
flatFrag x = mkExpr GLAtom $ Frag Flat x


-- * Vector, matrix, and array constructors

vec2 x y = mkExpr GLGenExpr $ GLVec2 x y
vec3 x y z = mkExpr GLGenExpr $ GLVec3 x y z
vec4 x y z w = mkExpr GLGenExpr $ GLVec4 x y z w
mat2 x y = mat2x2 x y
mat3 x y z = mat3x3 x y z
mat4 x y z w = mat4x4 x y z w
mat2x2 x y = mkExpr GLGenExpr $ GLMat2x2 x y
mat2x3 x y z = mkExpr GLGenExpr $ GLMat2x3 x y z
mat2x4 x y z w = mkExpr GLGenExpr $ GLMat2x4 x y z w
mat3x2 x y = mkExpr GLGenExpr $ GLMat3x2 x y
mat3x3 x y z = mkExpr GLGenExpr $ GLMat3x3 x y z
mat3x4 x y z w = mkExpr GLGenExpr $ GLMat3x4 x y z w
mat4x2 x y = mkExpr GLGenExpr $ GLMat4x2 x y
mat4x3 x y z = mkExpr GLGenExpr $ GLMat4x3 x y z
mat4x4 x y z w = mkExpr GLGenExpr $ GLMat4x4 x y z w

pre x y = mkExpr GLGenExpr $ Pre x y
app x y = mkExpr GLGenExpr $ App x y

infixr 9 $|
infixr 8 $-

x $| y = mkExpr GLGenExpr $ HorConc x y
x $- y = mkExpr GLGenExpr $ Conc x y

array xs = mkExpr GLGenExpr $ GLArray xs


-- * Deconstruction and indexing

x_ v = mkExpr GLGenExpr $ OpCoord CoordX v
y_ v = mkExpr GLGenExpr $ OpCoord CoordY v
z_ v = mkExpr GLGenExpr $ OpCoord CoordZ v
w_ v = mkExpr GLGenExpr $ OpCoord CoordW v
$gen2DCoordDecls
$gen3DCoordDecls

col0 m = mkExpr GLGenExpr $ OpCol Col0 m
col1 m = mkExpr GLGenExpr $ OpCol Col1 m
col2 m = mkExpr GLGenExpr $ OpCol Col2 m
col3 m = mkExpr GLGenExpr $ OpCol Col3 m

arr .! i = mkExpr GLGenExpr $ OpArrayElt arr i

class Deconstructible t where
    type Decon t
    decon :: t -> Decon t

instance (GLPrim t, GLType (Vec 2 t)) => Deconstructible (GLExpr d (Vec 2 t)) where
    type Decon (GLExpr d (Vec 2 t)) = (GLExpr d t, GLExpr d t) 
    decon v = (x_ v, y_ v)
instance (GLPrim t, GLType (Vec 3 t)) => Deconstructible (GLExpr d (Vec 3 t)) where
    type Decon (GLExpr d (Vec 3 t)) = (GLExpr d t, GLExpr d t, GLExpr d t) 
    decon v = (x_ v, y_ v, z_ v)
instance (GLPrim t, GLType (Vec 4 t)) => Deconstructible (GLExpr d (Vec 4 t)) where
    type Decon (GLExpr d (Vec 4 t)) = (GLExpr d t, GLExpr d t, GLExpr d t, GLExpr d t) 
    decon v = (x_ v, y_ v, z_ v, w_ v)
instance (GLPrim t, GLType (Mat p 2 t), GLType (Vec p t)) => Deconstructible (GLExpr d (Mat p 2 t)) where
    type Decon (GLExpr d (Mat p 2 t)) = (GLExpr d (Vec p t), GLExpr d (Vec p t)) 
    decon m = (col0 m, col1 m)
instance (GLPrim t, GLType (Mat p 3 t), GLType (Vec p t)) => Deconstructible (GLExpr d (Mat p 3 t)) where
    type Decon (GLExpr d (Mat p 3 t)) = (GLExpr d (Vec p t), GLExpr d (Vec p t), GLExpr d (Vec p t)) 
    decon m = (col0 m, col1 m, col2 m)
instance (GLPrim t, GLType (Mat p 4 t), GLType (Vec p t)) => Deconstructible (GLExpr d (Mat p 4 t)) where
    type Decon (GLExpr d (Mat p 4 t)) = (GLExpr d (Vec p t), GLExpr d (Vec p t), GLExpr d (Vec p t), GLExpr d (Vec p t)) 
    decon m = (col0 m, col1 m, col2 m, col3 m)


-- * Expression type conversion

cast x = mkExpr GLGenExpr $ Cast x
matCast m = mkExpr GLGenExpr $ MatCast m


-- * Custom function support

makeGenVar _ = mkExpr GLAtom GenVar

{-# NOINLINE glFunc1 #-}
glFunc1 :: (GLType t, GLType t1) => 
    (GLExpr d t1 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t 
glFunc1 f = (GLFunc (genID f)) . GLFunc1 f x
    where x = makeGenVar "x"
{-# NOINLINE glFunc2 #-}
glFunc2 :: (GLType t, GLType t1, GLType t2) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t 
glFunc2 f = (GLFunc (genID f) .) . GLFunc2 f x y
    where (x, y) = (makeGenVar "x", makeGenVar "y")
{-# NOINLINE glFunc3 #-}
glFunc3 :: (GLType t, GLType t1, GLType t2, GLType t3) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t 
glFunc3 f = ((GLFunc (genID f) .) .) . GLFunc3 f x y z
    where (x, y, z) = (makeGenVar "x", makeGenVar "y", makeGenVar "z")
{-# NOINLINE glFunc4 #-}
glFunc4 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t 
glFunc4 f = (((GLFunc (genID f) .) .) .) . GLFunc4 f x y z w
    where (x, y, z, w) = (makeGenVar "x", makeGenVar "y", makeGenVar "z", makeGenVar "w")
{-# NOINLINE glFunc5 #-}
glFunc5 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5-> GLExpr d t 
glFunc5 f = ((((GLFunc (genID f) .) .) .) .) . GLFunc5 f x y z w v
    where (x, y, z, w, v) = (makeGenVar "x", makeGenVar "y", makeGenVar "z", makeGenVar "w", makeGenVar "u")
{-# NOINLINE glFunc6 #-}
glFunc6 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5, GLType t6) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 -> GLExpr d t 
glFunc6 f = (((((GLFunc (genID f) .) .) .) .) .) . GLFunc6 f x y z w v u
    where (x, y, z, w, v, u) = (makeGenVar "x", makeGenVar "y", makeGenVar "z", makeGenVar "w", makeGenVar "u", makeGenVar "v")


-- * Builtin operators and functions

infixl 7 ./
infixl 7 .%
infix 4 .<, .<=, .>, .>=, .==, ./=
infixl 3 .&&
infixl 2 .||
infixl 4 .^^
infixl 1 .<<, .>>
infixl 3 .&
infixl 2 .|
infixl 4 .^
infixl 7 .*
infixl 7 .@

x ./ y = mkExpr GLGenExpr $ OpDiv x y
x .% y = mkExpr GLGenExpr $ OpMod x y
x .< y = mkExpr GLGenExpr $ OpLessThan x y
x .<= y = mkExpr GLGenExpr $ OpLessThanEqual x y
x .> y = mkExpr GLGenExpr $ OpGreaterThan x y
x .>= y = mkExpr GLGenExpr $ OpGreaterThanEqual x y
x .== y = mkExpr GLGenExpr $ OpEqual x y
x ./= y = mkExpr GLGenExpr $ OpNotEqual x y
x .&& y = mkExpr GLGenExpr $ OpAnd x y
x .|| y = mkExpr GLGenExpr $ OpOr x y
x .^^ y = mkExpr GLGenExpr $ OpXor x y
not x = mkExpr GLGenExpr $ OpNot x
cond x y z = mkExpr GLGenExpr $ OpCond x y z
x .<< y = mkExpr GLGenExpr $ OpLshift x y
x .>> y = mkExpr GLGenExpr $ OpRshift x y
x .& y = mkExpr GLGenExpr $ OpBitAnd x y
x .| y = mkExpr GLGenExpr $ OpBitOr x y
x .^ y = mkExpr GLGenExpr $ OpBitXor x y
x .* y = mkExpr GLGenExpr $ OpScalarMult x y
x .@ y = mkExpr GLGenExpr $ OpMatrixMult x y
neg x = mkExpr GLGenExpr $ OpCompl x

radians x = mkExpr GLGenExpr $ Radians x
degrees x = mkExpr GLGenExpr $ Degrees x
sin x = mkExpr GLGenExpr $ Sin x
cos x = mkExpr GLGenExpr $ Cos x
tan x = mkExpr GLGenExpr $ Tan x
asin x = mkExpr GLGenExpr $ Asin x
acos x = mkExpr GLGenExpr $ Acos x
atan x = mkExpr GLGenExpr $ Atan x
sinh x = mkExpr GLGenExpr $ Sin x
cosh x = mkExpr GLGenExpr $ Cos x
tanh x = mkExpr GLGenExpr $ Tan x
asinh x = mkExpr GLGenExpr $ Asin x
acosh x = mkExpr GLGenExpr $ Acos x
atanh x = mkExpr GLGenExpr $ Atan x

pow x y = mkExpr GLGenExpr $ Pow x y
exp x = mkExpr GLGenExpr $ Exp x
log x = mkExpr GLGenExpr $ Log x
exp2 x = mkExpr GLGenExpr $ Exp2 x
log2 x = mkExpr GLGenExpr $ Log2 x
sqrt x = mkExpr GLGenExpr $ Sqrt x
inversesqrt x = mkExpr GLGenExpr $ Inversesqrt x

floor x = mkExpr GLGenExpr $ Floor x
trunc x = mkExpr GLGenExpr $ Trunc x
round x = mkExpr GLGenExpr $ Round x
roundEven x = mkExpr GLGenExpr $ RoundEven x
ceil x = mkExpr GLGenExpr $ Ceil x
fract x = mkExpr GLGenExpr $ Fract x
mod x y = mkExpr GLGenExpr $ Mod x y
min x y = mkExpr GLGenExpr $ Min x y
max x y = mkExpr GLGenExpr $ Max x y
clamp x y z = mkExpr GLGenExpr $ Clamp x y z
mix x y z = mkExpr GLGenExpr $ Mix x y z
step x y = mkExpr GLGenExpr $ Step x y
smoothstep x y z = mkExpr GLGenExpr $ Smoothstep x y z

length x = mkExpr GLGenExpr $ Length x
distance x y = mkExpr GLGenExpr $ Distance x  y
dot x y = mkExpr GLGenExpr $ Dot x y
cross x y = mkExpr GLGenExpr $ Cross x y
normalize x = mkExpr GLGenExpr $ Normalize x
faceforward x y z = mkExpr GLGenExpr $ Faceforward x y z
reflect x y = mkExpr GLGenExpr $ Reflect x y
refract x y z = mkExpr GLGenExpr $ Refract x y z

matrixCompMult x y = mkExpr GLGenExpr $ MatrixCompMult x y
outerProduct x y = mkExpr GLGenExpr $ OuterProduct x y
transpose x = mkExpr GLGenExpr $ Transpose x
determinant x = mkExpr GLGenExpr $ Determinant x
inverse x = mkExpr GLGenExpr $ Inverse x

lessThan x y = mkExpr GLGenExpr $ LessThan x y
lessThanEqual x y = mkExpr GLGenExpr $ LessThanEqual x y
greaterThan x y = mkExpr GLGenExpr $ GreaterThan x y
greaterThanEqual x y = mkExpr GLGenExpr $ GreaterThanEqual x y
equal x y = mkExpr GLGenExpr $ Equal x y
notEqual x y = mkExpr GLGenExpr $ NotEqual x y
any x = mkExpr GLGenExpr $ Any x
all x = mkExpr GLGenExpr $ All x
compl x = mkExpr GLGenExpr $ Compl x


-- * Builtin I/O variables

time :: HostExpr Float
time = mkExpr GLAtom $ IOFloat "time"

mouseLeft :: HostExpr Bool
mouseLeft = mkExpr GLAtom $ IOBool "mouseLeft"

mouseRight :: HostExpr Bool
mouseRight = mkExpr GLAtom $ IOBool "mouseRight"

mouseWheel :: HostExpr Float
mouseWheel = mkExpr GLAtom $ IOFloat "mouseWheel"

mouseX :: HostExpr Float
mouseX = mkExpr GLAtom $ IOFloat "mouseX"

mouseY :: HostExpr Float
mouseY = mkExpr GLAtom $ IOFloat "mouseY"

mousePos :: HostExpr (Vec 2 Float)
mousePos = mkExpr GLGenExpr $ GLVec2 mouseX mouseY


-- * Drawables

class Drawable a where
    draw :: Backend -> a -> IO ()

instance Drawable GLObj where
    draw (GlutBackend userInit) obj = runGlut userInit [obj]

instance Drawable [GLObj] where
    draw (GlutBackend userInit) objs = runGlut userInit objs

defaultObj = GLObj {
    primitiveMode = OpenGL.Points,
    indices = Nothing,
    position = vec4 0 0 0 0,
    color = vec4 0 0 0 0,
    discardWhen = false
}

points = defaultObj { primitiveMode = OpenGL.Points }
lines = defaultObj { primitiveMode = OpenGL.Lines }
lineLoop = defaultObj { primitiveMode = OpenGL.LineLoop }
lineStrip = defaultObj { primitiveMode = OpenGL.LineStrip }
triangles = defaultObj { primitiveMode = OpenGL.Triangles }
triangleStrip = defaultObj { primitiveMode = OpenGL.TriangleStrip }
triangleFan = defaultObj { primitiveMode = OpenGL.Points }
quads = defaultObj { primitiveMode = OpenGL.Quads }
quadStrip = defaultObj { primitiveMode = OpenGL.QuadStrip }
polygon = defaultObj { primitiveMode = OpenGL.Polygon }

-- * Backends

data Backend =
    GlutBackend GlutOptions

drawGlut :: Drawable a => a -> IO ()
drawGlut = draw (GlutBackend defaultGlutOptions)

drawGlutCustom :: Drawable a => GlutOptions -> a -> IO ()
drawGlutCustom options = draw (GlutBackend options)

defaultGlutOptions :: GlutOptions
defaultGlutOptions = GlutOptions {
    winPosition = Nothing,
    winSize = (768, 768),
    winFullscreen = False,
    winTitle = Nothing,
    glLineWidth = 3,
    runMode = GlutNormal
}
