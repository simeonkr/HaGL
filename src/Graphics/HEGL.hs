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
    -- * Generic constructors
    Graphics.HEGL.const,
    uniform,
    prec,
    vert,
    frag,
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
    xyz_, xyw_, xzy_, xzw_, xwy_, xwy_,
    yxz_, yxw_, yzx_, yzw_, ywx_, ywz_,
    zxy_, zxw_, zyx_, zyw_, zwx_, zwy_,
    wxy_, wxz_, wyx_, wyz_, wxz_, wxy_,
    col0, col1, col2, col3,
    (%!),
    -- * Type conversion
    glCast,
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

import qualified Graphics.Rendering.OpenGL as OpenGL

import Graphics.HEGL.TH.HEGL (gen2DCoordDecls, gen3DCoordDecls)
import Graphics.HEGL.Numerical (Mat, Vec, RowVec, fromMapping, fromList)
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.ExprID (genID)
import Graphics.HEGL.GLObj
import Graphics.HEGL.Eval
import Graphics.HEGL.Backend.GLUT


-- * Expressions: Main definitions

instance GLPrim t => Enum (GLExpr d t) where
    toEnum x = GLAtom (genID ()) $ Const (toEnum x)
    fromEnum x = undefined --FIXME

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
    x ** y = GLGenExpr (genID ()) $ Pow x y
    exp x = GLGenExpr (genID ()) $ Exp x
    sqrt x = GLGenExpr (genID ()) $ Sqrt x
    log x = GLGenExpr (genID ()) $ Log x


-- * Expressions: Lifts from raw types

glLift0 x = GLAtom (genID ()) $ GLLift0 x
glLift1 f x = GLAtom (genID ()) $ GLLift1 f x
glLift2 f x y = GLAtom (genID ()) $ GLLift2 f x y
-- TODO: add remaining lifts, up to glLift6


-- * Generic expression constructors

const :: GLType t => ConstExpr t -> GLExpr d t
const x = GLAtom (genID ()) $ Const (constEval x)

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


-- * Expression type conversion

glCast x = GLGenExpr (genID ()) $ Cast x


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
-- TODO: add remaining cases, up to glFunc6


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

pow x y = GLGenExpr (genID ()) $ Pow x y
exp x = GLGenExpr (genID ()) $ Exp x
log x = GLGenExpr (genID ()) $ Log x
exp2 x = GLGenExpr (genID ()) $ Exp2 x
log2 x = GLGenExpr (genID ()) $ Log2 x
sqrt x = GLGenExpr (genID ()) $ Sqrt x

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
    draw GLUTBackend obj = runGLUT [obj]
    draw ImageBackend obj = undefined

instance Drawable [GLObj] where
    draw GLUTBackend objs = runGLUT objs
    draw ImageBackend objs = undefined

points = GLObj { primitiveMode = OpenGL.Points }
lines = GLObj { primitiveMode = OpenGL.Lines }
lineLoop = GLObj { primitiveMode = OpenGL.LineLoop }
lineStrip = GLObj { primitiveMode = OpenGL.LineStrip }
triangles = GLObj { primitiveMode = OpenGL.Triangles }
triangleStrip = GLObj { primitiveMode = OpenGL.TriangleStrip }
triangleFan = GLObj { primitiveMode = OpenGL.Points }
quads = GLObj { primitiveMode = OpenGL.Quads }
quadStrip = GLObj { primitiveMode = OpenGL.QuadStrip }
polygon = GLObj { primitiveMode = OpenGL.Polygon }

-- * Backends

data Backend =
    GLUTBackend |
    ImageBackend

drawGLUT :: Drawable a => a -> IO()
drawGLUT = draw GLUTBackend

drawImage :: Drawable a => a -> IO()
drawImage = draw ImageBackend
