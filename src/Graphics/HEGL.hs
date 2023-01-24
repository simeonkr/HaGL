{-|
Module      : HEGL
Copyright   : (c) Simeon Krastnikov, 2022
License     : MIT
Maintainer  : skrastnikov@gmail.com
Stability   : experimental
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    GLType(Elt),
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
    -- * Generic constructors
    Graphics.HEGL.const,
    uniform,
    prec,
    vert,
    frag,
    -- * Vector, matrix, and array constructors
    vec2,
    vec3,
    vec4,
    mat2x2,
    mat3x3,
    mat4x4,
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
    xyz_,
    col0, col1, col2, col3,
    -- * Type conversion
    toFloat,
    toInt,
    toUInt,
    -- * Custom function support
    glFunc1,
    glFunc2,
    glFunc3,
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
    -- ** Exponential functions
    pow,
    Graphics.HEGL.exp,
    Graphics.HEGL.log,
    exp2,
    log2,
    Graphics.HEGL.sqrt,
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
    drawGLUT,
    drawImage
) where

import Prelude

import Graphics.Rendering.OpenGL.GL.PrimitiveMode (PrimitiveMode(..))
import qualified Graphics.Rendering.OpenGL as OpenGL

import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.Numerical
import Graphics.HEGL.ExprID (genID)

type ConstExpr = GLExpr ConstDomain
type HostExpr = GLExpr HostDomain
type VertExpr = GLExpr VertexDomain
type FragExpr = GLExpr FragmentDomain


-- * Expressions: Main definitions

instance GLPrim t => Enum (GLExpr d t) where
    toEnum x = Const (genID ()) (toEnum x)
    fromEnum x = undefined

instance (GLNumeric (Elt t), GLType t, Num t) => Num (GLExpr d t) where
    x + y = OpAdd (genID ()) x y
    x - y = OpSubt (genID ()) x y
    x * y = OpMult (genID ()) x y
    negate x = OpNeg (genID ()) x
    abs x = Abs (genID ()) x
    signum x = Sign (genID ()) x
    fromInteger x = Const (genID ()) (fromInteger x)

instance (GLFloating (Elt t), GLType t, Fractional t) => Fractional (GLExpr d t) where
    x / y = OpDiv (genID ()) x y
    fromRational x = Const (genID ()) (fromRational x)

instance Floating (GLExpr d Float) where
    pi = 3.141592653589793238
    sin x = Sin (genID ()) x
    cos x = Cos (genID ()) x
    tan x = Tan (genID ()) x
    asin x = Asin (genID ()) x
    acos x = Acos (genID ()) x
    atan x = Atan (genID ()) x
    x ** y = Pow (genID ()) x y
    exp x = Exp (genID ()) x
    sqrt x = Sqrt (genID ()) x
    log x = Log (genID ()) x


-- * Expressions: Lifts from raw types

glLift0 x = HostVar $ GLLift0 (genID ()) x
glLift1 f x = HostVar $ GLLift1 (genID ()) f x
glLift2 f x y = HostVar $ GLLift2 (genID ()) f x y
-- TODO: add remaining lifts, up to glLift6


-- * Generic expression constructors

const :: GLType t => ConstExpr t -> GLExpr d t
const x = Const (genID ()) (evalConst x) where
    evalConst = undefined

uniform :: GLType t => HostExpr t -> GLExpr d t
uniform x = Uniform (genID ()) x

prec :: GLType t => HostExpr t -> HostExpr t -> HostExpr t
prec x0 x = HostVar $ HostPrec (genID ()) x0 x

vert :: GLPrim t => [ConstExpr t] -> VertExpr t
vert inp = Inp (genID ()) inp

frag :: GLPrim t => VertExpr t -> FragExpr t
frag x = Frag (genID ()) x


-- * Vector, matrix, and array constructors

vec2 x y = GLVec2 (genID ()) x y
vec3 x y z = GLVec3 (genID ()) x y z
vec4 x y z w = GLVec4 (genID ()) x y z w
-- TODO: synthesize all mat constructors using $- and $| operators
mat2x2 x y = GLMat2x2 (genID ()) x y
mat3x3 x y z = GLMat3x3 (genID ()) x y z
mat4x4 x y z w = GLMat4x4 (genID ()) x y z w

pre x y = Pre (genID ()) x y
app x y = App (genID ()) x y

infixr 9 $-
infixr 8 $|

x $- y = Conc (genID ()) x y
x $| y = HorConc (genID ()) x y

arr xs = GLArray (genID ()) xs

-- * Deconstruction and indexing

class Deconstructible t where
    type Decon t
    decon :: t -> Decon t

instance GLType (Vec 2 t) => Deconstructible (GLExpr d (Vec 2 t)) where
    type Decon (GLExpr d (Vec 2 t)) = (GLExpr d t, GLExpr d t) 
    decon = undefined

instance GLType (Vec 3 t) => Deconstructible (GLExpr d (Vec 3 t)) where
    type Decon (GLExpr d (Vec 3 t)) = (GLExpr d t, GLExpr d t, GLExpr d t) 
    decon = undefined

instance GLType (Vec 4 t) => Deconstructible (GLExpr d (Vec 4 t)) where
    type Decon (GLExpr d (Vec 4 t)) = (GLExpr d t, GLExpr d t, GLExpr d t, GLExpr d t) 
    decon = undefined

x_ v = OpCoord (genID ()) CoordX v
y_ v = OpCoord (genID ()) CoordY v
z_ v = OpCoord (genID ()) CoordZ v
w_ v = OpCoord (genID ()) CoordW v
xy_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordY `CoordCons` CoordNil)) v
xz_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordZ `CoordCons` CoordNil)) v
xw_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordW `CoordCons` CoordNil)) v
yx_ v = OpCoordMulti (genID ()) (CoordY `CoordCons` (CoordX `CoordCons` CoordNil)) v
yz_ v = OpCoordMulti (genID ()) (CoordY `CoordCons` (CoordZ `CoordCons` CoordNil)) v
yw_ v = OpCoordMulti (genID ()) (CoordY `CoordCons` (CoordW `CoordCons` CoordNil)) v
zx_ v = OpCoordMulti (genID ()) (CoordZ `CoordCons` (CoordX `CoordCons` CoordNil)) v
zy_ v = OpCoordMulti (genID ()) (CoordZ `CoordCons` (CoordY `CoordCons` CoordNil)) v
zw_ v = OpCoordMulti (genID ()) (CoordZ `CoordCons` (CoordW `CoordCons` CoordNil)) v
wx_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordX `CoordCons` CoordNil)) v
wy_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordY `CoordCons` CoordNil)) v
wz_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordZ `CoordCons` CoordNil)) v
xyz_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordY `CoordCons` (CoordZ `CoordCons` CoordNil))) v
-- TODO: remaining 23 cases

col0 m = OpCol (genID ()) Col0 m
col1 m = OpCol (genID ()) Col1 m
col2 m = OpCol (genID ()) Col2 m
col3 m = OpCol (genID ()) Col3 m


-- * Expression type conversion

toFloat x = ToFloat (genID ()) x
toInt x = ToInt (genID ()) x
toUInt x = ToUInt (genID ()) x


-- * Custom function support

makeFuncParam () = FuncParam (genID ())

glFunc1 :: (GLType t, GLType t1) => 
    (GLExpr d t1 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t 
glFunc1 f = GLFunc1 (genID ()) (f x) x
    where x = makeFuncParam ()

glFunc2 :: (GLType t, GLType t1, GLType t2) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t 
glFunc2 f = GLFunc2 (genID ()) (f x y) x y
    where (x, y) = (makeFuncParam (), makeFuncParam ())

glFunc3 :: (GLType t, GLType t1, GLType t2, GLType t3) => 
    (GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t) -> 
     GLExpr d t1 -> GLExpr d t2 -> GLExpr d t3 -> GLExpr d t 
glFunc3 f = GLFunc3 (genID ()) (f x y z) x y z
    where (x, y, z) = (makeFuncParam (), makeFuncParam (), makeFuncParam ())
-- TODO: add remaining lifts, up to glFunc6


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

x .% y = OpMod (genID ()) x y
x .< y = OpLessThan (genID ()) x y
x .<= y = OpLessThanEqual (genID ()) x y
x .> y = OpGreaterThan (genID ()) x y
x .>= y = OpGreaterThanEqual (genID ()) x y
x .== y = OpEqual (genID ()) x y
x ./= y = OpNotEqual (genID ()) x y
x .&& y = OpAnd (genID ()) x y
x .|| y = OpOr (genID ()) x y
x .^^ y = OpXor (genID ()) x y
not x = OpNot (genID ()) x
cond x y z = OpCond (genID ()) x y z
neg x = OpCompl (genID ()) x
x .<< y = OpLshift (genID ()) x y
x .>> y = OpRshift (genID ()) x y
x .& y = OpBitAnd (genID ()) x y
x .| y = OpBitOr (genID ()) x y
x .^ y = OpBitXor (genID ()) x y
x .* y = OpScalarMult (genID ()) x y
x .@ y = OpMatrixMult (genID ()) x y

radians x = Radians (genID ()) x
degrees x = Degrees (genID ()) x
sin x = Sin (genID ()) x
cos x = Cos (genID ()) x
tan x = Tan (genID ()) x
asin x = Asin (genID ()) x
acos x = Acos (genID ()) x
atan x = Atan (genID ()) x

pow x y = Pow (genID ()) x y
exp x = Exp (genID ()) x
log x = Log (genID ()) x
exp2 x = Exp2 (genID ()) x
log2 x = Log2 (genID ()) x
sqrt x = Sqrt (genID ()) x

floor x = Floor (genID ()) x
trunc x = Trunc (genID ()) x
round x = Round (genID ()) x
roundEven x = RoundEven (genID ()) x
ceil x = Ceil (genID ()) x
fract x = Fract (genID ()) x
mod x y = Mod (genID ()) x y
min x y = Min (genID ()) x y
max x y = Max (genID ()) x y
clamp x y z = Clamp (genID ()) x y z
mix x y z = Mix (genID ()) x y z
step x y = Step (genID ()) x y
smoothstep x y z = Smoothstep (genID ()) x y z

length x = Length (genID ()) x
distance x y = Distance (genID ()) x  y
dot x y = Dot (genID ()) x y
cross x y = Cross (genID ()) x y
normalize x = Normalize (genID ()) x
faceforward x y z = Faceforward (genID ()) x y z
reflect x y = Reflect (genID ()) x y
refract x y z = Refract (genID ()) x y z

matrixCompMult x y = MatrixCompMult (genID ()) x y
outerProduct x y = OuterProduct (genID ()) x y
transpose x = Transpose (genID ()) x
determinant x = Determinant (genID ()) x
inverse x = Inverse (genID ()) x

lessThan x y = LessThan (genID ()) x y
lessThanEqual x y = LessThanEqual (genID ()) x y
greaterThan x y = GreaterThan (genID ()) x y
greaterThanEqual x y = GreaterThanEqual (genID ()) x y
equal x y = Equal (genID ()) x y
notEqual x y = NotEqual (genID ()) x y
any x = Any (genID ()) x
all x = All (genID ()) x
compl x = Compl (genID ()) x


-- * Builtin I/O variables

time :: HostExpr Float
time = HostVar $ IOFloat undefined

mouseLeft :: HostExpr Bool
mouseLeft = HostVar $ IOBool undefined

mouseRight :: HostExpr Bool
mouseRight = HostVar $ IOBool undefined

mouseWheel :: HostExpr Float
mouseWheel = HostVar $ IOFloat undefined

mouseX :: HostExpr Float
mouseX = HostVar $ IOFloat undefined

mouseY :: HostExpr Float
mouseY = HostVar $ IOFloat undefined

mousePos :: HostExpr (Vec 2 Float)
mousePos = GLVec2 (genID ()) mouseX mouseY


-- * Drawables

class Drawable a where
    draw :: Backend -> a -> IO ()

data GLObj = GLObj {
    primitiveMode :: PrimitiveMode,
    indices :: [ConstExpr UInt],
    position :: VertExpr (Vec 4 Float),
    color :: FragExpr (Vec 4 Float),
    discardWhen :: FragExpr Bool
}

instance Drawable GLObj where
    draw GLUTBackend = undefined
    draw ImageBackend = undefined

instance Drawable [GLObj] where
    draw = undefined

points = GLObj OpenGL.Points
lines = GLObj OpenGL.Lines
lineLoop = GLObj OpenGL.LineLoop
lineStrip = GLObj OpenGL.LineStrip
triangles = GLObj OpenGL.Triangles
triangleStrip = GLObj OpenGL.TriangleStrip
triangleFan = GLObj OpenGL.Points
quads = GLObj OpenGL.Quads
quadStrip = GLObj OpenGL.QuadStrip
polygon = GLObj OpenGL.Polygon

-- * Backends

data Backend =
    GLUTBackend |
    ImageBackend

drawGLUT :: Drawable a => a -> IO()
drawGLUT = draw GLUTBackend

drawImage :: Drawable a => a -> IO()
drawImage = draw ImageBackend
