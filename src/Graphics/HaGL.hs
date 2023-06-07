{-|
Module      : HaGL
Copyright   : (c) Simeon Krastnikov, 2022-2023
License     : MIT
Maintainer  : Simeon Krastnikov <skrastnikov@gmail.com>
Stability   : experimental

This module exports everything that comprises the core language.

It is best used with the following extensions enabled: 
@GADTs@, @DataKinds@, @ViewPatterns@, @FlexibleContexts@.

Note that quite a few of the exported functions clash with unrelated
ones from Prelude ('max', 'length', 'mod', 'any', etc.) or class methods
with identical behaviour ('abs', 'sin', etc.), in an effort to prioritize
consistency with GLSL function naming.

In summary, this module can be imported as follows:

@
    &#x7b;-\# LANGUAGE GADTs \#-&#x7d;
    &#x7b;-\# LANGUAGE DataKinds \#-&#x7d;
    &#x7b;-\# LANGUAGE ViewPatterns \#-&#x7d;
    &#x7b;-\# LANGUAGE FlexibleContexts \#-&#x7d;

    import Prelude hiding (max, sin, cos, ...)

    import Graphics.HaGL
@

HaGL expressions have the type 'GLExpr' (d ::  'GLDomain') t,
where @d@ is the domain of computation and @t@ is the underlying numeric type, 
which is always an instance of 'GLType'. Here are some example expressions:

@
    -- A vertex attribute, constructed from three vertices
    x :: GLExpr VertexDomain Float
    x = vert [-1, 0, 1]

    -- Numeric operators and functions like (+) and sin can handle generic
    -- expressions. Note that in this example the domain of the inputs to
    -- these functions is VertexDomain, so we know that these functions will 
    -- be computed in a vertex shader.
    y :: GLExpr VertexDomain Float
    y = sin (2 * x + 1)

    -- 'frag x' is a a fragment variable corresponding to fragments obtained by 
    -- interpolating the vertices of x that define its containing primitive.
    -- Because it has the type 'GLExpr FragmentDomain Float', the addition
    -- will be computed in a fragment shader.
    z :: GLExpr FragmentDomain Float
    z = frag x + 3

    -- \'time\' is a built-in I/O variable and as such it is computed on the CPU
    time :: GLExpr HostDomain Float

    -- We can use \'uniform\' to lift a host variable to an arbitrary domain
    -- Here 'uniform time' is inferred to have type 'GLExpr VertexDomain Float':
    yPlusTime :: GLExpr VertexDomain Float
    yPlusTime = y + uniform time

    -- Here 'uniform time' is inferred to be of type 'GLExpr FragmentDomain Float'
    zPlusTime :: GLExpr FragmentDomain Float
    zPlusTime = z + uniform time

    -- A generic floating-point vector of length 4
    v :: GLExpr d (Vec 4 Float)
    v = vec4 1 1 1 1

    -- A vector can be initialized from a numeric literal, so long as its
    -- underlying type 'Vec n t' is specified or can be inferred.
    -- Here is another way to define the same vector v:
    v :: GLExpr d (Vec 4 Float)
    v = 1

    -- Matrices are constructed from their columns:
    m :: GLExpr d (Mat 2 3 Float)
    m = mat2x3 (vec2 1 2) (vec2 3 4) (vec2 5 6)

    -- Operators like (.+) and (.*) act component-wise on vectors and matrices:
    _ = mat2x2 1 1 .+ mat2x2 1 1 .== mat2x2 2 2

    -- Non-boolean primitives and vectors over such types are instances of Num;
    -- in such cases Num methods like (+) can be used instead.
    _ = vec2 1 1 + 1 .== vec2 2 2

    -- The operator (.#) performs scalar multiplication:
    _ = 3 .# v
    _ = 3 .# m

    -- The operator (.\@) performs matrix multiplication
    -- (including matrix-vector multiplication):
    m1 :: GLExpr d (Mat 2 3 Float)
    m1 = ...
    m2 :: GLExpr d (Mat 3 4 Float)
    m2 = ...
    m1m2 :: GLExpr d (Mat 2 4 Float)
    m1m2 = m1 .\@ m2

    -- All multiplications here will take place in a vertex shader:
    m1m2v :: GLExpr VertexDomain (Vec 2 Float)
    m1m2v = m1m2 .\@ v

    -- The inferred type of m1m2 in this expression is 
    -- 'GLExpr HostDomain (Mat 2 4 Float)' so the multiplication of m1 and m2 
    -- will take place on the CPU.
    -- The inferred type of uniform m1m2 is 'GLExpr VertexDomain (Mat 2 4 Float)'
    -- and that of v is 'GLExpr VertexDomain (Vec 2 Float)' so their
    -- multiplication will take place in a vertex shader.
    m1m2v' :: GLExpr VertexDomain (Vec 2 Float)
    m1m2v' = uniform m1m2 .\@ v

@

`GLExpr`s can be used to construct `GLObj`s, which being instances
of 'Drawable' can be interpreted by a given 'Backend' using 'draw'.
For example:

@
-- initialize pos from the vertices of some 3D object
pos :: GLExpr VertexDomain (Vec 4 Float)
pos = vert [vec4 1 0 0 1, ...]

red :: GLExpr FragmentDomain (Vec 4 Float)
red = vec4 1 0 0 1

redObj :: GLObj
redObj = GLObj {
    primitiveMode = TriangleStrip,
    indices = Nothing,
    position = pos,
    color = red,
    discardWhen = False
}

-- or equivalently,
redObj' :: GLObj
redObj' = triangleStrip { position = pos, color = red }

-- we are now ready to draw the object
main :: IO ()
main = draw GlutBackend redObj
@

A complete set of examples can be found in 
the ["Getting Started"](https://github.com/simeonkr/HaGL/Overview.md) guide.

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dth-dec-file #-}
-- required due to use of genID
{-# OPTIONS_GHC -fno-full-laziness #-}

module Graphics.HaGL (
    -- * @GLType@
    -- | Any instance of 'GLType' can be the underlying type @t@ of a 'GLExpr'
    -- These types are:
    --
    --  * Primitive types: 'Float', 'Double', 'Int', 'UInt', 'Bool'
    --
    --  * Vectors: 'Vec' @n@ @Float@, 'Vec' @n@ @Double@, 'Vec' @n@ @Int@,
    --   'Vec' @n@ @UInt@, 'Vec' @n@ @Bool@
    --
    --  * Matrices: 'Mat' @p@ @q@ @Float@, 'Mat' @p@ @q@ @Double@
    --
    --  * Arrays: Represented as [t], where @t@ is a primitive type or a vector
    GLType,
    Float, Double, Int, UInt, Bool,
    Mat, Vec,
    GLElt,
    -- ** Raw vector/matrix constructors
    -- | Though raw types are not usually constructed directly, the following
    -- functions can be used for loading data from externally computed arrays 
    -- via [lifts](#lifts).
    fromMapping,
    fromList,
    -- ** Subclasses of @GLType@
    GLPrim, GLSingle, GLNumeric, GLSigned, GLFloating, GLSingleNumeric, GLInteger,
    GLPrimOrVec, GLInputType,
    GLSupportsSmoothInterp, GLSupportsBitwiseOps,
    -- * @GLExpr@
    -- | A HaGL expression can be created in one of the following ways:
    --
    --   * Using one of the HaGL-specific [constructors](#constructors)
    --
    --   * Directly from a numeric literal, if its underlying type @t@ is one of
    --    'Int', 'UInt', 'Float', 'Double', or a 'Vec' of one of these types. 
    --    (A vector initialized from a value @c@ is the vector with all elements 
    --    equal to c.)
    --
    --   * Through the application of [built-in operators and functions](#builtins)
    --
    GLExpr,
    GLDomain(..),
    ConstExpr,
    HostExpr,
    VertExpr,
    FragExpr,
    -- * Constructors #constructors#
    cnst,
    true,
    false,
    uniform,
    prec,
    vert,
    frag,
    noperspFrag,
    flatFrag,
    -- ** Vector, matrix, and array constructors
    -- | A constructor of the form vec/n/ creates a column vector with 
    -- /n/ components; a constructor of the form mat/p/x/q/ creates a matrix 
    -- with /p/ rows and /q/ columns (mat/p/ is an alias for mat/p/x/p/).  
    vec2, vec3, vec4,
    mat2, mat3, mat4,
    mat2x2, mat2x3, mat2x4,
    mat3x2, mat3x3, mat3x4,
    mat4x2, mat4x3, mat4x4,
    pre,
    app,
    ($-),
    array,
    -- * Deconstruction and indexing
    -- | To deconstruct a vector into a tuple of primitive elements or to
    -- deconstruct a matrix into a tuple of column vectors use 'decon'. This
    -- approach pairs particularly well with view patterns:
    --
    -- @
    -- vec2Flip :: GLExpr d (Vec 2 t) -> GLExpr d (Vec 2 t)
    -- vec2Flip (decon v2 -> (v1, v2)) = vec2 v2 v1
    -- @ 
    --
    -- Alternatively, with the synonyms @x@, @y@, @z@, @w@, referring to the 
    -- components of a vector in that order, projection functions consisting of 
    -- an ordered selection of such names followed by an underscore (e.g., 'xyz_'), 
    -- can be used to extract the corresponding components. For matrices, the 
    -- projection functions are of the form col/n/. Note that the types of these
    -- functions constrains the length of their input so that the operation is
    -- well-defined.
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
    cast,
    matCast,
    -- * Built-in operators and functions #builtins#
    -- | Most definitions here strive to be consistent with the corresponding
    -- built-in functions provided by GLSL
    -- (cf. [The OpenGL Shading Language, Version 4.60.7]
    -- (https://registry.khronos.org/OpenGL/specs/gl/GLSLangSpec.4.60.pdf)), 
    -- in terms of semantics and typing constraints. Some notable exceptions 
    -- to this rule are:
    --
    --  * Typing restrictions may be stricter to prevent what would otherwise be
    --    runtime errors; for example, matrix multiplication is only defined on
    --    matrices with the correct dimensions.
    --
    --  * The operators @(+)@, @(-)@, @(*)@, as well as the function @negate@,
    --    being methods of @Num@, are only supported on expressions where the 
    --    underlying type is one of 'Int', 'UInt', 'Float', 'Double', or a 
    --    vector of one of these types.
    --    To perform these operations component-wise on matrices use the operators
    --    @(.+)@, @(.-)@, @(.*)@, or the function @neg@ respectively.
    --
    --  * The operator @(/)@ is only supported when the underlying type is
    --    'Float' or 'Double'. The more general operator @(./)@ additionally 
    --     supports integer and component-wise division.
    --
    --  * The operator @(.%)@ is the modulo operation on integers or
    --    integer-valued vectors.
    --
    --  * The operator @(.#)@ is used for scalar multiplication.
    --
    --  * The operator @(.\@)@ is used for matrix (including matrix-vector) multiplication.
    --
    --  * All boolean and bitwise operators are also prefixed with a single dot:
    --    @(.==)@, @(.<)@, @(.&&)@, @(.&)@, etc.

    -- ** Arithmetic operators
    (.+),
    (.-),
    (.*),
    (./),
    (.%),
    (.#),
    (.@),
    neg,
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
    nt,
    cond,
    -- ** Bitwise operators
    (.<<),
    (.>>),
    (.&),
    (.|),
    (.^),
    compl,
    -- ** Angle and trigonometry functions
    radians,
    degrees,
    Graphics.HaGL.sin,
    Graphics.HaGL.cos,
    Graphics.HaGL.tan,
    Graphics.HaGL.asin,
    Graphics.HaGL.acos,
    Graphics.HaGL.atan,
    Graphics.HaGL.sinh,
    Graphics.HaGL.cosh,
    Graphics.HaGL.tanh,
    Graphics.HaGL.asinh,
    Graphics.HaGL.acosh,
    Graphics.HaGL.atanh,
    -- ** Exponential functions
    pow,
    Graphics.HaGL.exp,
    Graphics.HaGL.log,
    exp2,
    log2,
    Graphics.HaGL.sqrt,
    inversesqrt,
    -- ** Common functions
    Graphics.HaGL.abs,
    sign,
    Graphics.HaGL.floor,
    trunc,
    Graphics.HaGL.round,
    roundEven,
    ceil,
    fract,
    Graphics.HaGL.mod,
    Graphics.HaGL.min,
    Graphics.HaGL.max,
    clamp,
    mix,
    step,
    smoothstep,
    -- ** Geometric functions
    Graphics.HaGL.length,
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
    Graphics.HaGL.any,
    Graphics.HaGL.all,
    Graphics.HaGL.not,
    -- * Custom function support
    -- | An n-ary @f@ function on @GLExpr@'s can be transported to an arbitrary
    -- domain using @glFunc/n/@. That is, @glFunc/n/ f@ will take in the same 
    -- arguments as f but will be evaluated in the domain of its return type 
    -- (in contrast to @f@, which being a native Haskell function, will always 
    -- be evaluated on the CPU).
    --
    -- However, due to the fact that GLSL does not allow recursion, attempting
    -- to call @glFunc/n/ f@, where @f@ is defined recursively (or mutually
    -- recursively in terms of other functions) will generally result in an
    -- exception being thrown. The one case where this is permissible is that of
    -- /tail-recursive/ functions of the form
    --
    -- @
    -- f x1 x2 ... = cond c b (f y1 y2 ...)
    -- @
    --
    -- where none of the expressions @c@, @b@, @y1@, @y2@, ... depend on @f@. Where
    -- applicable, such functions will be synthesized as GLSL loops. For example,
    -- the factorial function can be computed within a vertex shader as follows:
    --
    -- @
    -- fact = glFunc1 $ \\n -> fact' n 1 where
    --   fact' :: GLExpr VertexDomain Int -> GLExpr VertexDomain Int -> GLExpr VertexDomain Int
    --   fact' = glFunc2 $ \\n a -> cond (n .== 0) a (fact' (n - 1) (a * n))
    -- 
    -- x :: GLExpr VertexDomain Int
    -- x = fact 5
    -- @
    glFunc1,
    glFunc2,
    glFunc3,
    glFunc4,
    glFunc5,
    glFunc6,
    -- * Lifts from raw types #lifts#
    -- | If the need arises to use an n-ary native function @f@ that is not defined
    -- over @GLExpr@'s (for instance, to dynamically update array contents using
    -- functions defined on lists), such a function can be lifted to the
    -- @HostDomain@ using @glLift/n/@. @glLift/n/ f@ will then be defined over
    -- `HostExpr`s that agree with respective argument types of @f@. For example,
    -- the two expressions below compute the same array:
    --
    -- @
    -- a1 :: GLExpr HostDomain [Float]
    -- a1 = (glLift2 $ \\x y -> [x, y, x + y]) time time
    -- a2 :: GLExpr HostDomain [Float]
    -- a2 = array [time, 2 * time, 3 * time]
    -- @
    -- 
    glLift0, 
    glLift1, 
    glLift2,
    glLift3,
    glLift4,
    glLift5,
    glLift6,
    -- * Built-in I/O variables
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
    PrimitiveMode,
    points,
    Graphics.HaGL.lines,
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

import GHC.TypeNats (KnownNat)
import qualified Graphics.Rendering.OpenGL as OpenGL

import Graphics.HaGL.TH.HaGL (gen2DCoordDecls, gen3DCoordDecls)
import Graphics.HaGL.Numerical (Mat, Vec, fromMapping, fromList)
import Graphics.HaGL.GLType
import Graphics.HaGL.GLExpr
import Graphics.HaGL.ExprID (genID)
import Graphics.HaGL.GLObj
import Graphics.HaGL.Eval
import Graphics.HaGL.Backend.GLUT


{-# NOINLINE mkExpr #-}
mkExpr con e = con (genID e) e


-- Instance declarations

instance GLPrim t => Enum (ConstExpr t) where
    toEnum x = mkExpr GLAtom $ Const (toEnum x)
    fromEnum = fromEnum . constEval

instance {-# OVERLAPPING #-} (GLSigned (GLElt t), GLPrimOrVec t, Num t) => Num (GLExpr d t) where
    x + y = mkExpr GLGenExpr $ OpAdd x y
    x - y = mkExpr GLGenExpr $ OpSubt x y
    x * y = mkExpr GLGenExpr $ OpMult x y
    negate x = mkExpr GLGenExpr $ OpNeg x
    abs x = mkExpr GLGenExpr $ Abs x
    signum x = mkExpr GLGenExpr $ Sign x
    fromInteger x = mkExpr GLAtom $ Const (fromInteger x)

-- Unsigned integers need to be handled separately as GLSL does not
-- support all the operations corresponding to those required for Num

instance {-# OVERLAPPING #-} Num (GLExpr d UInt) where
    x + y = mkExpr GLGenExpr $ OpAdd x y
    x - y = mkExpr GLGenExpr $ OpSubt x y
    x * y = mkExpr GLGenExpr $ OpMult x y
    negate x = 
        mkExpr GLGenExpr $ Cast $ 
            mkExpr GLGenExpr $ OpNeg
                (mkExpr GLGenExpr $ Cast x :: GLExpr _ Int)
    abs x = x
    signum x = mkExpr GLGenExpr $ Cast 
        (mkExpr GLGenExpr $ Cast x :: GLExpr _ Bool)
    fromInteger x = mkExpr GLAtom $ Const (fromInteger x)

instance {-# OVERLAPPING #-} (GLType (Vec n UInt), GLType (Vec n Int), GLType (Vec n Bool), KnownNat n) => Num (GLExpr d (Vec n UInt)) where
    x + y = mkExpr GLGenExpr $ OpAdd x y
    x - y = mkExpr GLGenExpr $ OpSubt x y
    x * y = mkExpr GLGenExpr $ OpMult x y
    negate x =
        mkExpr GLGenExpr $ MatCast $ 
            mkExpr GLGenExpr $ OpNeg
                (mkExpr GLGenExpr $ MatCast x :: GLExpr _ (Vec _ Int))
    abs x = x
    signum x = 
        mkExpr GLGenExpr $ MatCast 
            (mkExpr GLGenExpr $ MatCast x :: GLExpr _ (Vec _ Bool))
    fromInteger x = mkExpr GLAtom $ Const (fromInteger x)

instance (GLFloating (GLElt t), GLPrimOrVec t, Fractional t) => Fractional (GLExpr d t) where
    x / y = mkExpr GLGenExpr $ OpDiv x y
    fromRational x = mkExpr GLAtom $ Const (fromRational x)

instance (GLElt t ~ Float, GLPrimOrVec t, Fractional t) => Floating (GLExpr d t) where
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


-- * Constructors

-- | Construct a @GLExpr@ from a raw type. Rarely useful as this can
-- be done implicitly; e.g., from a numeric literal.
cnst :: GLType t => ConstExpr t -> GLExpr d t
cnst x = mkExpr GLAtom $ Const (constEval x)

true, false :: GLExpr d Bool
-- | The boolean value @true@
true = cnst $ toEnum 1
-- | The boolean value @false@
false = cnst $ toEnum 0

-- | Lift a `HostExpr` to an arbitrary @GLExpr@ whose value is the same across
-- any primitive processed in a shader, if used in the context of one
uniform :: GLType t => HostExpr t -> GLExpr d t
uniform x = mkExpr GLAtom $ Uniform x

-- | @prec x0 x@ is used to obtain a reference to the value @x@ one "time-step"
-- in the past, or @x0@ at the zero-th point in time. The @prec@ operator is 
-- usually used to define expressions recurrently; for example:
-- @let x = prec 0 (x + 1)@ counts the total number of points in time. The
-- interpretation of a time-step in a given backend is normally an interval
-- that is on average equal to the length of time between two redraws.
prec :: GLType t => HostExpr t -> HostExpr t -> HostExpr t
prec x0 x = mkExpr GLAtom $ IOPrec x0 x


-- | A vertex input variable (attribute) constructed from a stream of per-vertex 
-- data. The number of vertices (the length of the stream) should be consistent
-- across all vertex attributes used to construct a given @GLObj@.
vert :: GLInputType t => [ConstExpr t] -> VertExpr t
vert inp = mkExpr GLAtom $ Inp inp

-- | A fragment input variable constructed from the output data of a vertex 
-- variable, interpolated in a perspective-correct manner over the primitive
-- being processed
frag :: GLSupportsSmoothInterp t => VertExpr t -> FragExpr t
frag x = mkExpr GLAtom $ Frag Smooth x

-- | A fragment input variable constructed from the output data of a vertex 
-- variable, interpolated linearly across the primitive being processed
noperspFrag :: GLSupportsSmoothInterp t => GLInputType t => VertExpr t -> FragExpr t
noperspFrag x = mkExpr GLAtom $ Frag NoPerspective x

-- | A fragment input variable constructed from the output data of a vertex
-- variable, having the same value across the primitive being processed
-- (cf. the OpenGL API for which vertex is used to determine its value)
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

-- | Extend a vector by prepending an element
pre x y = mkExpr GLGenExpr $ Pre x y
-- | Extend a vector by appending an element
app x y = mkExpr GLGenExpr $ App x y

infixr 8 $-

-- | Concatenate two vectors together
x $- y = mkExpr GLGenExpr $ Conc x y

-- | Create an array from a list of 'HostExpr's
array xs = mkExpr GLGenExpr $ GLArray xs


-- * Deconstruction and indexing

x_ v = mkExpr GLGenExpr $ OpCoord CoordX v
y_ v = mkExpr GLGenExpr $ OpCoord CoordY v
z_ v = mkExpr GLGenExpr $ OpCoord CoordZ v
w_ v = mkExpr GLGenExpr $ OpCoord CoordW v
$gen2DCoordDecls
$gen3DCoordDecls

-- | The first column of a matrix
col0 m = mkExpr GLGenExpr $ OpCol Col0 m
-- | The second column of a matrix
col1 m = mkExpr GLGenExpr $ OpCol Col1 m
-- | The third column of a matrix
col2 m = mkExpr GLGenExpr $ OpCol Col2 m
-- | The fourth column of a matrix
col3 m = mkExpr GLGenExpr $ OpCol Col3 m

-- | Array index operator, returning the @i@-th (0-indexed) element of the array
arr .! i = mkExpr GLGenExpr $ OpArrayElt arr i

-- | An expression that can be deconstructed into its components
class Deconstructible t where
    -- | The resulting type of the deconstruction
    type Decon t
    -- | Deconstruct the given expression
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

-- | Coerce the primitive type of a value to arbitrary primitive type
cast x = mkExpr GLGenExpr $ Cast x
-- | Coerce the element type of a matrix to an arbitrary primitive type
matCast m = mkExpr GLGenExpr $ MatCast m


-- * Built-in operators and functions

infixl 6  .+, .-
infixl 7  .*, ./, .%
infix 4 .<, .<=, .>, .>=, .==, ./=
infixl 3 .&&
infixl 1 .||
infixl 2 .^^
infixl 5 .<<, .>>
infixl 3 .&
infixl 1 .|
infixl 2 .^
infixl 7 .#
infixl 7 .@

x .+ y = mkExpr GLGenExpr $ OpAdd x y
x .- y = mkExpr GLGenExpr $ OpSubt x y
x .* y = mkExpr GLGenExpr $ OpMult x y
x ./ y = mkExpr GLGenExpr $ OpDiv x y
x .% y = mkExpr GLGenExpr $ OpMod x y
-- | Arithmetic negation 
neg x = mkExpr GLGenExpr $ OpNeg x
x .< y = mkExpr GLGenExpr $ OpLessThan x y
x .<= y = mkExpr GLGenExpr $ OpLessThanEqual x y
x .> y = mkExpr GLGenExpr $ OpGreaterThan x y
x .>= y = mkExpr GLGenExpr $ OpGreaterThanEqual x y
x .== y = mkExpr GLGenExpr $ OpEqual x y
x ./= y = mkExpr GLGenExpr $ OpNotEqual x y
x .&& y = mkExpr GLGenExpr $ OpAnd x y
x .|| y = mkExpr GLGenExpr $ OpOr x y
x .^^ y = mkExpr GLGenExpr $ OpXor x y
-- | Logical not
nt x = mkExpr GLGenExpr $ OpNot x
-- | Conditional operator, evaluating and returning its second or third argument
-- if the first evaluates to true or false, respectively
cond x y z = mkExpr GLGenExpr $ OpCond x y z
x .<< y = mkExpr GLGenExpr $ OpLshift x y
x .>> y = mkExpr GLGenExpr $ OpRshift x y
x .& y = mkExpr GLGenExpr $ OpBitAnd x y
x .| y = mkExpr GLGenExpr $ OpBitOr x y
x .^ y = mkExpr GLGenExpr $ OpBitXor x y
-- | One's complement
compl x = mkExpr GLGenExpr $ OpCompl x
-- | Scalar multiplication
x .# y = mkExpr GLGenExpr $ OpScalarMult x y
-- | Matrix multiplication
x .@ y = mkExpr GLGenExpr $ OpMatrixMult x y

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

abs x = mkExpr GLGenExpr $ Abs x
sign x = mkExpr GLGenExpr $ Sign x
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
not x = mkExpr GLGenExpr $ Not x


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
glFunc6 f = (((((GLFunc (genID f) .) .) .) .) .) . GLFunc6 f x y z w u v
    where (x, y, z, w, u, v) = (makeGenVar "x", makeGenVar "y", makeGenVar "z", makeGenVar "w", makeGenVar "u", makeGenVar "v")


-- Lifts from raw types

glLift0 x = mkExpr GLAtom $ GLLift0 x
glLift1 f x = mkExpr GLAtom $ GLLift1 f x
glLift2 f x y = mkExpr GLAtom $ GLLift2 f x y
glLift3 f x y z = mkExpr GLAtom $ GLLift3 f x y z
glLift4 f x y z w = mkExpr GLAtom $ GLLift4 f x y z w
glLift5 f x y z w u = mkExpr GLAtom $ GLLift5 f x y z w u
glLift6 f x y z w u v = mkExpr GLAtom $ GLLift6 f x y z w u v


-- * Built-in I/O variables

-- | Seconds elapsed since an initial point in time 
time :: HostExpr Float
time = mkExpr GLAtom $ IOFloat "time"

-- | True if and only if the left mouse button is pressed
mouseLeft :: HostExpr Bool
mouseLeft = mkExpr GLAtom $ IOBool "mouseLeft"

-- | True if and only if the right mouse button is pressed
mouseRight :: HostExpr Bool
mouseRight = mkExpr GLAtom $ IOBool "mouseRight"

-- | A pulse signal, equal to 1 at the moment the mouse wheel scrolls up, -1 when 
-- the mouse wheel scrolls down, and afterwards exponentially decaying to its 
-- otherwise default value of 0
mouseWheel :: HostExpr Float
mouseWheel = mkExpr GLAtom $ IOFloat "mouseWheel"

-- | The horizontal position of the mouse, not necessarily within the window bounds
mouseX :: HostExpr Float
mouseX = mkExpr GLAtom $ IOFloat "mouseX"

-- | The vertical position of the mouse, not necessarily within the window bounds
mouseY :: HostExpr Float
mouseY = mkExpr GLAtom $ IOFloat "mouseY"

-- | Equal to @vec2 mouseX mouseY@
mousePos :: HostExpr (Vec 2 Float)
mousePos = mkExpr GLGenExpr $ GLVec2 mouseX mouseY


-- * Drawables

-- | Anything that can be drawn using a given 'Backend'
class Drawable a where
    draw :: Backend -> a -> IO ()

-- | A 'GLObj' is drawn by constructing primitives from its 'position' and
-- 'indices' expressions, according to its 'primitiveMode', and coloring the
-- resulting fragments according to its 'color' expression.
instance Drawable GLObj where
    draw backend obj = draw backend [obj]

-- | A set of 'GLObj's is drawn by drawing each 'GLObj' individually and with the
-- same blending mode as that used to draw a single 'GLObj'.
instance Drawable [GLObj] where
    draw (GlutBackend userInit) objs = runGlut userInit objs

defaultObj = GLObj {
    primitiveMode = OpenGL.Points,
    indices = Nothing,
    position = vec4 0 0 0 0,
    color = vec4 0 0 0 0,
    discardWhen = false
}

-- | An incompletely specified object with 'PrimitiveMode' equal to 'Points'
points = defaultObj { primitiveMode = OpenGL.Points }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'Lines'
lines = defaultObj { primitiveMode = OpenGL.Lines }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'LineLoop'
lineLoop = defaultObj { primitiveMode = OpenGL.LineLoop }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'LineStrip'
lineStrip = defaultObj { primitiveMode = OpenGL.LineStrip }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'Triangles'
triangles = defaultObj { primitiveMode = OpenGL.Triangles }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'TriangleStrip'
triangleStrip = defaultObj { primitiveMode = OpenGL.TriangleStrip }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'TriangleFan'
triangleFan = defaultObj { primitiveMode = OpenGL.TriangleFan }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'Quads'
quads = defaultObj { primitiveMode = OpenGL.Quads }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'QuadStrip'
quadStrip = defaultObj { primitiveMode = OpenGL.QuadStrip }
-- | An incompletely specified object with 'PrimitiveMode' equal to 'Polygon'
polygon = defaultObj { primitiveMode = OpenGL.Polygon }


-- * Backends

-- | A backend that can interpret (draw) a 'Drawable'.
-- Unless overridden the following OpenGL options are set by default in all backends:
--
--  * Clear color equal to black
--
--  * Depth testing enabled
--
--  * Blending enabled with blend equation equal to GL_FUNC_ADD 
--
--  * Source blending factor equal to GL_SRC_ALPHA
--
--  * Destination blending factor equal to  GL_ONE_MINUS_SRC_ALPHA
data Backend =

    GlutBackend GlutOptions

-- | Draw in a GLUT backend using default options
drawGlut :: Drawable a => a -> IO ()
drawGlut = draw (GlutBackend defaultGlutOptions)

-- | Draw in a GLUT backend using specified options
drawGlutCustom :: Drawable a => GlutOptions -> a -> IO ()
drawGlutCustom options = draw (GlutBackend options)

-- | Default options for a GLUT backend
--
-- * @winSize = (768, 768)@
-- * @clearCol = (0, 0, 0, 0)@
-- * @runMode = GlutNormal@
defaultGlutOptions :: GlutOptions
defaultGlutOptions = GlutOptions {
    winPosition = Nothing,
    winSize = (768, 768),
    winFullscreen = False,
    winTitle = Nothing,
    clearCol = (0, 0, 0, 0),
    runMode = GlutNormal,
    openGLSetup = return ()
}
