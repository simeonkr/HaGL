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


makeGL () = GLExpr (genID ())


-- * Expressions: Main definitions

instance GLPrim t => Enum (GLExpr d t) where
    toEnum x = makeGL () $ Const (toEnum x)
    fromEnum x = undefined

instance (GLNumeric (Elt t), GLType t, Num t) => Num (GLExpr d t) where
    x + y = makeGL () $ OpAdd x y
    x - y = makeGL () $ OpSubt x y
    x * y = makeGL () $ OpMult x y
    negate x = makeGL () $ OpNeg x
    abs x = makeGL () $ Abs x
    signum x = makeGL () $ Sign x
    fromInteger x = makeGL () $ Const (fromInteger x)

instance (GLFloating (Elt t), GLType t, Fractional t) => Fractional (GLExpr d t) where
    x / y = makeGL () $ OpDiv x y
    fromRational x = makeGL () $ Const (fromRational x)

instance Floating (GLExpr d Float) where
    pi = 3.141592653589793238
    sin x = makeGL () $ Sin x
    cos x = makeGL () $ Cos x
    tan x = makeGL () $ Tan x
    asin x = makeGL () $ Asin x
    acos x = makeGL () $ Acos x
    atan x = makeGL () $ Atan x
    x ** y = makeGL () $ Pow x y
    exp x = makeGL () $ Exp x
    sqrt x = makeGL () $ Sqrt x
    log x = makeGL () $ Log x


-- * Expressions: Lifts from raw types

glLift0 x = let id = genID () in \x -> GLExpr id $ HostVar (GLLift0 id x)
glLift1 f x = let id = genID () in \x -> GLExpr id $ HostVar (GLLift1 id f x)
glLift2 f x y = let id = genID () in \x -> GLExpr id $ HostVar (GLLift2 id f x y)
-- TODO: add remaining lifts, up to glLift6


-- * Generic expression constructors

const :: GLType t => ConstExpr t -> GLExpr d t
const x = makeGL () $ Const (evalConst x) where
    evalConst = undefined

uniform :: GLType t => HostExpr t -> GLExpr d t
uniform x = makeGL () $ Uniform x

prec :: GLType t => HostExpr t -> HostExpr t -> HostExpr t
prec x0 x = let id = genID () in GLExpr id $ HostVar $ HostPrec id x0 x

vert :: GLPrim t => [ConstExpr t] -> VertExpr t
vert inp = makeGL () $ Inp inp

frag :: GLPrim t => VertExpr t -> FragExpr t
frag x = makeGL () $ Frag x


-- * Vector, matrix, and array constructors

vec2 x y = makeGL () $ GLVec2 x y
vec3 x y z = makeGL () $ GLVec3 x y z
vec4 x y z w = makeGL () $ GLVec4 x y z w
-- TODO: synthesize all mat constructors using $- and $| operators
mat2x2 x y = makeGL () $ GLMat2x2 x y
mat3x3 x y z = makeGL () $ GLMat3x3 x y z
mat4x4 x y z w = makeGL () $ GLMat4x4 x y z w

pre x y = makeGL () $ Pre x y
app x y = makeGL () $ App x y

infixr 9 $-
infixr 8 $|

x $- y = makeGL () $ Conc x y
x $| y = makeGL () $ HorConc x y

arr xs = makeGL () $ GLArray xs

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

x_ v = makeGL () $ OpCoord CoordX v
y_ v = makeGL () $ OpCoord CoordY v
z_ v = makeGL () $ OpCoord CoordZ v
w_ v = makeGL () $ OpCoord CoordW v
xy_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordY `CoordCons` CoordNil)) v
xz_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordZ `CoordCons` CoordNil)) v
xw_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordW `CoordCons` CoordNil)) v
yx_ v = makeGL () $ OpCoordMulti (CoordY `CoordCons` (CoordX `CoordCons` CoordNil)) v
yz_ v = makeGL () $ OpCoordMulti (CoordY `CoordCons` (CoordZ `CoordCons` CoordNil)) v
yw_ v = makeGL () $ OpCoordMulti (CoordY `CoordCons` (CoordW `CoordCons` CoordNil)) v
zx_ v = makeGL () $ OpCoordMulti (CoordZ `CoordCons` (CoordX `CoordCons` CoordNil)) v
zy_ v = makeGL () $ OpCoordMulti (CoordZ `CoordCons` (CoordY `CoordCons` CoordNil)) v
zw_ v = makeGL () $ OpCoordMulti (CoordZ `CoordCons` (CoordW `CoordCons` CoordNil)) v
wx_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordX `CoordCons` CoordNil)) v
wy_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordY `CoordCons` CoordNil)) v
wz_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordZ `CoordCons` CoordNil)) v
xyz_ v = makeGL () $ OpCoordMulti (CoordX `CoordCons` (CoordY `CoordCons` (CoordZ `CoordCons` CoordNil))) v
-- TODO: remaining 23 cases

col0 m = makeGL () $ OpCol Col0 m
col1 m = makeGL () $ OpCol Col1 m
col2 m = makeGL () $ OpCol Col2 m
col3 m = makeGL () $ OpCol Col3 m


-- * Expression type conversion

toFloat x = makeGL () $ ToFloat x
toInt x = makeGL () $ ToInt x
toUInt x = makeGL () $ ToUInt x


-- * Custom function support

makeFuncParam () = makeGL () $ FuncParam

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

x .% y = makeGL () $ OpMod x y
x .< y = makeGL () $ OpLessThan x y
x .<= y = makeGL () $ OpLessThanEqual x y
x .> y = makeGL () $ OpGreaterThan x y
x .>= y = makeGL () $ OpGreaterThanEqual x y
x .== y = makeGL () $ OpEqual x y
x ./= y = makeGL () $ OpNotEqual x y
x .&& y = makeGL () $ OpAnd x y
x .|| y = makeGL () $ OpOr x y
x .^^ y = makeGL () $ OpXor x y
not x = makeGL () $ OpNot x
cond x y z = makeGL () $ OpCond x y z
neg x = makeGL () $ OpCompl x
x .<< y = makeGL () $ OpLshift x y
x .>> y = makeGL () $ OpRshift x y
x .& y = makeGL () $ OpBitAnd x y
x .| y = makeGL () $ OpBitOr x y
x .^ y = makeGL () $ OpBitXor x y
x .* y = makeGL () $ OpScalarMult x y
x .@ y = makeGL () $ OpMatrixMult x y

radians x = makeGL () $ Radians x
degrees x = makeGL () $ Degrees x
sin x = makeGL () $ Sin x
cos x = makeGL () $ Cos x
tan x = makeGL () $ Tan x
asin x = makeGL () $ Asin x
acos x = makeGL () $ Acos x
atan x = makeGL () $ Atan x

pow x y = makeGL () $ Pow x y
exp x = makeGL () $ Exp x
log x = makeGL () $ Log x
exp2 x = makeGL () $ Exp2 x
log2 x = makeGL () $ Log2 x
sqrt x = makeGL () $ Sqrt x

floor x = makeGL () $ Floor x
trunc x = makeGL () $ Trunc x
round x = makeGL () $ Round x
roundEven x = makeGL () $ RoundEven x
ceil x = makeGL () $ Ceil x
fract x = makeGL () $ Fract x
mod x y = makeGL () $ Mod x y
min x y = makeGL () $ Min x y
max x y = makeGL () $ Max x y
clamp x y z = makeGL () $ Clamp x y z
mix x y z = makeGL () $ Mix x y z
step x y = makeGL () $ Step x y
smoothstep x y z = makeGL () $ Smoothstep x y z

length x = makeGL () $ Length x
distance x y = makeGL () $ Distance x  y
dot x y = makeGL () $ Dot x y
cross x y = makeGL () $ Cross x y
normalize x = makeGL () $ Normalize x
faceforward x y z = makeGL () $ Faceforward x y z
reflect x y = makeGL () $ Reflect x y
refract x y z = makeGL () $ Refract x y z

matrixCompMult x y = makeGL () $ MatrixCompMult x y
outerProduct x y = makeGL () $ OuterProduct x y
transpose x = makeGL () $ Transpose x
determinant x = makeGL () $ Determinant x
inverse x = makeGL () $ Inverse x

lessThan x y = makeGL () $ LessThan x y
lessThanEqual x y = makeGL () $ LessThanEqual x y
greaterThan x y = makeGL () $ GreaterThan x y
greaterThanEqual x y = makeGL () $ GreaterThanEqual x y
equal x y = makeGL () $ Equal x y
notEqual x y = makeGL () $ NotEqual x y
any x = makeGL () $ Any x
all x = makeGL () $ All x
compl x = makeGL () $ Compl x


-- * Builtin I/O variables

makeHostVar con id = GLExpr id $ HostVar (con id)

time :: HostExpr Float
time = makeHostVar IOFloat undefined

mouseLeft :: HostExpr Bool
mouseLeft = makeHostVar IOBool undefined

mouseRight :: HostExpr Bool
mouseRight = makeHostVar IOBool undefined

mouseWheel :: HostExpr Float
mouseWheel = makeHostVar IOFloat undefined

mouseX :: HostExpr Float
mouseX = makeHostVar IOFloat undefined

mouseY :: HostExpr Float
mouseY = makeHostVar IOFloat undefined

mousePos :: HostExpr (Vec 2 Float)
mousePos = makeGL () $ GLVec2 mouseX mouseY


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
