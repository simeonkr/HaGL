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
{-# OPTIONS_GHC -fno-cse #-}

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
    flatFrag,
    noperpFrag,
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
    arr,
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
    (%!),
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
    (.%),
    -- ** Boolean operators
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
    neg,
    -- ** Bitwise operators
    (.<<),
    (.>>),
    (.&),
    (.|),
    (.^),
    (.*),
    (.@),
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
    drawGlut,
    drawGlutCustom,
    drawImage
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
import Graphics.HEGL.Backend.Image


-- * Expressions: Main definitions

instance GLPrim t => Enum (ConstExpr t) where
    toEnum x = GLAtom (genID ()) $ Const (toEnum x)
    fromEnum = fromEnum . constEval

instance (GLNumeric (GLElt t), GLType t, Num t) => Num (GLExpr d t) where
    x + y = GLGenExpr (genID ()) $ OpAdd x y
    x - y = GLGenExpr (genID ()) $ OpSubt x y
    x * y = GLGenExpr (genID ()) $ OpMult x y
    negate x = GLGenExpr (genID ()) $ OpNeg x
    abs x = GLGenExpr (genID ()) $ Abs x
    signum x = GLGenExpr (genID ()) $ Sign x
    fromInteger x = GLAtom (genID ()) $ Const (fromInteger x)

instance (GLFloating (GLElt t), GLType t, Fractional t) => Fractional (GLExpr d t) where
    x / y = GLGenExpr (genID ()) $ OpDiv x y
    fromRational x = GLAtom (genID ()) $ Const (fromRational x)

instance Floating (GLExpr d Float) where
    pi = 3.141592653589793238
    sin x = GLGenExpr (genID ()) $ Sin x
    cos x = GLGenExpr (genID ()) $ Cos x
    tan x = GLGenExpr (genID ()) $ Tan x
    asin x = GLGenExpr (genID ()) $ Asin x
    acos x = GLGenExpr (genID ()) $ Acos x
    atan x = GLGenExpr (genID ()) $ Atan x
    sinh x = GLGenExpr (genID ()) $ Sinh x
    cosh x = GLGenExpr (genID ()) $ Cosh x
    tanh x = GLGenExpr (genID ()) $ Tanh x
    asinh x = GLGenExpr (genID ()) $ Asinh x
    acosh x = GLGenExpr (genID ()) $ Acosh x
    atanh x = GLGenExpr (genID ()) $ Atanh x
    x ** y = GLGenExpr (genID ()) $ Pow x y
    exp x = GLGenExpr (genID ()) $ Exp x
    sqrt x = GLGenExpr (genID ()) $ Sqrt x
    log x = GLGenExpr (genID ()) $ Log x


-- * Expressions: Lifts from raw types

glLift0 x = GLAtom (genID ()) $ GLLift0 x
glLift1 f x = GLAtom (genID ()) $ GLLift1 f x
glLift2 f x y = GLAtom (genID ()) $ GLLift2 f x y
glLift3 f x y z = GLAtom (genID ()) $ GLLift3 f x y z
glLift4 f x y z w = GLAtom (genID ()) $ GLLift4 f x y z w
glLift5 f x y z w u = GLAtom (genID ()) $ GLLift5 f x y z w u
glLift6 f x y z w u v = GLAtom (genID ()) $ GLLift6 f x y z w u v


-- * Generic expression constructors

const :: GLType t => ConstExpr t -> GLExpr d t
const x = GLAtom (genID ()) $ Const (constEval x)

true, false :: GLExpr d Bool
true = Graphics.HEGL.const $ toEnum . fromEnum $ 1
false = Graphics.HEGL.const $ toEnum . fromEnum $ 0

uniform :: GLType t => HostExpr t -> GLExpr d t
uniform x = GLAtom (genID ()) $ Uniform x

prec :: GLType t => HostExpr t -> HostExpr t -> HostExpr t
prec x0 x = GLAtom (genID ()) $ IOPrec x0 x

vert :: GLInputType t => [ConstExpr t] -> VertExpr t
vert inp = GLAtom (genID ()) $ Inp inp

frag :: GLSupportsSmoothInterp t => VertExpr t -> FragExpr t
frag x = GLAtom (genID ()) $ Frag Smooth x

flatFrag :: GLInputType t => VertExpr t -> FragExpr t
flatFrag x = GLAtom (genID ()) $ Frag Flat x

noperpFrag :: GLInputType t => VertExpr t -> FragExpr t
noperpFrag x = GLAtom (genID ()) $ Frag NoPerspective x


-- * Vector, matrix, and array constructors

vec2 x y = GLGenExpr (genID ()) $ GLVec2 x y
vec3 x y z = GLGenExpr (genID ()) $ GLVec3 x y z
vec4 x y z w = GLGenExpr (genID ()) $ GLVec4 x y z w
mat2 x y = mat2x2 x y
mat3 x y z = mat3x3 x y z
mat4 x y z w = mat4x4 x y z w
mat2x2 x y = GLGenExpr (genID ()) $ GLMat2x2 x y
mat2x3 x y z = GLGenExpr (genID ()) $ GLMat2x3 x y z
mat2x4 x y z w = GLGenExpr (genID ()) $ GLMat2x4 x y z w
mat3x2 x y = GLGenExpr (genID ()) $ GLMat3x2 x y
mat3x3 x y z = GLGenExpr (genID ()) $ GLMat3x3 x y z
mat3x4 x y z w = GLGenExpr (genID ()) $ GLMat3x4 x y z w
mat4x2 x y = GLGenExpr (genID ()) $ GLMat4x2 x y
mat4x3 x y z = GLGenExpr (genID ()) $ GLMat4x3 x y z
mat4x4 x y z w = GLGenExpr (genID ()) $ GLMat4x4 x y z w

pre x y = GLGenExpr (genID ()) $ Pre x y
app x y = GLGenExpr (genID ()) $ App x y

infixr 9 $|
infixr 8 $-

x $| y = GLGenExpr (genID ()) $ HorConc x y
x $- y = GLGenExpr (genID ()) $ Conc x y

arr xs = GLGenExpr (genID ()) $ GLArray xs


-- * Deconstruction and indexing

x_ v = GLGenExpr (genID ()) $ OpCoord CoordX v
y_ v = GLGenExpr (genID ()) $ OpCoord CoordY v
z_ v = GLGenExpr (genID ()) $ OpCoord CoordZ v
w_ v = GLGenExpr (genID ()) $ OpCoord CoordW v
$gen2DCoordDecls
$gen3DCoordDecls

col0 m = GLGenExpr (genID ()) $ OpCol Col0 m
col1 m = GLGenExpr (genID ()) $ OpCol Col1 m
col2 m = GLGenExpr (genID ()) $ OpCol Col2 m
col3 m = GLGenExpr (genID ()) $ OpCol Col3 m

arr %! i = GLGenExpr (genID ()) $ OpArrayElt arr i

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

cast x = GLGenExpr (genID ()) $ Cast x
matCast m = GLGenExpr (genID ()) $ MatCast m


-- * Custom function support

makeGenVar () = GLAtom (genID ()) GenVar

glFunc1 :: (GLType t, GLType t1) => 
    (GLExpr d t1 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t 
glFunc1 f = \x0 -> GLFunc (genID ()) $ GLFunc1 f x x0
    where x = makeGenVar ()
glFunc2 :: (GLType t, GLType t1, GLType t2) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t 
glFunc2 f = \x0 y0 -> GLFunc (genID ()) $ GLFunc2 f x y x0 y0
    where (x, y) = (makeGenVar (), makeGenVar ())
glFunc3 :: (GLType t, GLType t1, GLType t2, GLType t3) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t 
glFunc3 f = \x0 y0 z0 -> GLFunc (genID ()) $ GLFunc3 f x y z x0 y0 z0
    where (x, y, z) = (makeGenVar (), makeGenVar (), makeGenVar ())
glFunc4 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t 
glFunc4 f = \x0 y0 z0 w0 -> GLFunc (genID ()) $ GLFunc4 f x y z w x0 y0 z0 w0
    where (x, y, z, w) = (makeGenVar (), makeGenVar (), makeGenVar (), makeGenVar ())
glFunc5 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5-> GLExpr d t 
glFunc5 f = \x0 y0 z0 w0 v0 -> GLFunc (genID ()) $ GLFunc5 f x y z w v x0 y0 z0 w0 v0
    where (x, y, z, w, v) = (makeGenVar (), makeGenVar (), makeGenVar (), makeGenVar (), makeGenVar ())
glFunc6 :: (GLType t, GLType t1, GLType t2, GLType t3, GLType t4, GLType t5, GLType t6) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t4 -> GLExpr d t5 -> GLExpr d t6 -> GLExpr d t 
glFunc6 f = \x0 y0 z0 w0 v0 u0 -> GLFunc (genID ()) $ GLFunc6 f x y z w v u x0 y0 z0 w0 v0 u0
    where (x, y, z, w, v, u) = (makeGenVar (), makeGenVar (), makeGenVar (), makeGenVar (), makeGenVar (), makeGenVar ())


-- * Builtin operators and functions

infixl 7 .%
infix 4 .<, .<=, .>, .>=, .==, ./=
infixl 7 .&&
infixl 4 .||
infixl 6 .^^
infixl 8 .<<, .>>
infixl 7 .&
infixl 4 .|
infixl 6 .^
infixl 7 .*
infixl 7 .@

x .% y = GLGenExpr (genID ()) $ OpMod x y
x .< y = GLGenExpr (genID ()) $ OpLessThan x y
x .<= y = GLGenExpr (genID ()) $ OpLessThanEqual x y
x .> y = GLGenExpr (genID ()) $ OpGreaterThan x y
x .>= y = GLGenExpr (genID ()) $ OpGreaterThanEqual x y
x .== y = GLGenExpr (genID ()) $ OpEqual x y
x ./= y = GLGenExpr (genID ()) $ OpNotEqual x y
x .&& y = GLGenExpr (genID ()) $ OpAnd x y
x .|| y = GLGenExpr (genID ()) $ OpOr x y
x .^^ y = GLGenExpr (genID ()) $ OpXor x y
not x = GLGenExpr (genID ()) $ OpNot x
cond x y z = GLGenExpr (genID ()) $ OpCond x y z
neg x = GLGenExpr (genID ()) $ OpCompl x
x .<< y = GLGenExpr (genID ()) $ OpLshift x y
x .>> y = GLGenExpr (genID ()) $ OpRshift x y
x .& y = GLGenExpr (genID ()) $ OpBitAnd x y
x .| y = GLGenExpr (genID ()) $ OpBitOr x y
x .^ y = GLGenExpr (genID ()) $ OpBitXor x y
x .* y = GLGenExpr (genID ()) $ OpScalarMult x y
x .@ y = GLGenExpr (genID ()) $ OpMatrixMult x y

radians x = GLGenExpr (genID ()) $ Radians x
degrees x = GLGenExpr (genID ()) $ Degrees x
sin x = GLGenExpr (genID ()) $ Sin x
cos x = GLGenExpr (genID ()) $ Cos x
tan x = GLGenExpr (genID ()) $ Tan x
asin x = GLGenExpr (genID ()) $ Asin x
acos x = GLGenExpr (genID ()) $ Acos x
atan x = GLGenExpr (genID ()) $ Atan x
sinh x = GLGenExpr (genID ()) $ Sin x
cosh x = GLGenExpr (genID ()) $ Cos x
tanh x = GLGenExpr (genID ()) $ Tan x
asinh x = GLGenExpr (genID ()) $ Asin x
acosh x = GLGenExpr (genID ()) $ Acos x
atanh x = GLGenExpr (genID ()) $ Atan x

pow x y = GLGenExpr (genID ()) $ Pow x y
exp x = GLGenExpr (genID ()) $ Exp x
log x = GLGenExpr (genID ()) $ Log x
exp2 x = GLGenExpr (genID ()) $ Exp2 x
log2 x = GLGenExpr (genID ()) $ Log2 x
sqrt x = GLGenExpr (genID ()) $ Sqrt x
inversesqrt x = GLGenExpr (genID ()) $ Inversesqrt x

floor x = GLGenExpr (genID ()) $ Floor x
trunc x = GLGenExpr (genID ()) $ Trunc x
round x = GLGenExpr (genID ()) $ Round x
roundEven x = GLGenExpr (genID ()) $ RoundEven x
ceil x = GLGenExpr (genID ()) $ Ceil x
fract x = GLGenExpr (genID ()) $ Fract x
mod x y = GLGenExpr (genID ()) $ Mod x y
min x y = GLGenExpr (genID ()) $ Min x y
max x y = GLGenExpr (genID ()) $ Max x y
clamp x y z = GLGenExpr (genID ()) $ Clamp x y z
mix x y z = GLGenExpr (genID ()) $ Mix x y z
step x y = GLGenExpr (genID ()) $ Step x y
smoothstep x y z = GLGenExpr (genID ()) $ Smoothstep x y z

length x = GLGenExpr (genID ()) $ Length x
distance x y = GLGenExpr (genID ()) $ Distance x  y
dot x y = GLGenExpr (genID ()) $ Dot x y
cross x y = GLGenExpr (genID ()) $ Cross x y
normalize x = GLGenExpr (genID ()) $ Normalize x
faceforward x y z = GLGenExpr (genID ()) $ Faceforward x y z
reflect x y = GLGenExpr (genID ()) $ Reflect x y
refract x y z = GLGenExpr (genID ()) $ Refract x y z

matrixCompMult x y = GLGenExpr (genID ()) $ MatrixCompMult x y
outerProduct x y = GLGenExpr (genID ()) $ OuterProduct x y
transpose x = GLGenExpr (genID ()) $ Transpose x
determinant x = GLGenExpr (genID ()) $ Determinant x
inverse x = GLGenExpr (genID ()) $ Inverse x

lessThan x y = GLGenExpr (genID ()) $ LessThan x y
lessThanEqual x y = GLGenExpr (genID ()) $ LessThanEqual x y
greaterThan x y = GLGenExpr (genID ()) $ GreaterThan x y
greaterThanEqual x y = GLGenExpr (genID ()) $ GreaterThanEqual x y
equal x y = GLGenExpr (genID ()) $ Equal x y
notEqual x y = GLGenExpr (genID ()) $ NotEqual x y
any x = GLGenExpr (genID ()) $ Any x
all x = GLGenExpr (genID ()) $ All x
compl x = GLGenExpr (genID ()) $ Compl x


-- * Builtin I/O variables

time :: HostExpr Float
time = GLAtom (genID ()) $ IOFloat "time"

mouseLeft :: HostExpr Bool
mouseLeft = GLAtom (genID ()) $ IOBool "mouseLeft"

mouseRight :: HostExpr Bool
mouseRight = GLAtom (genID ()) $ IOBool "mouseRight"

mouseWheel :: HostExpr Float
mouseWheel = GLAtom (genID ()) $ IOFloat "mouseWheel"

mouseX :: HostExpr Float
mouseX = GLAtom (genID ()) $ IOFloat "mouseX"

mouseY :: HostExpr Float
mouseY = GLAtom (genID ()) $ IOFloat "mouseY"

mousePos :: HostExpr (Vec 2 Float)
mousePos = GLGenExpr (genID ()) $ GLVec2 mouseX mouseY


-- * Drawables

class Drawable a where
    draw :: Backend -> a -> IO ()

instance Drawable GLObj where
    draw (GlutBackend userInit) obj = runGlut userInit [obj]
    draw ImageBackend obj = runImage [obj]

instance Drawable [GLObj] where
    draw (GlutBackend userInit) objs = runGlut userInit objs
    draw ImageBackend objs = runImage objs

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
    GlutBackend GlutOptions |
    ImageBackend

drawGlut :: Drawable a => a -> IO ()
drawGlut = draw (GlutBackend defaultOptions) where
    defaultOptions = GlutOptions {
        winPosition = Nothing,
        winSize = (768, 768),
        winFullscreen = False,
        winTitle = Nothing,
        glLineWidth = 3,
        captureFile = Nothing
    }

drawGlutCustom :: Drawable a => GlutOptions -> a -> IO ()
drawGlutCustom options = draw (GlutBackend options)

drawImage :: Drawable a => a -> IO ()
drawImage = draw ImageBackend
