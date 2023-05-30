# Getting Started With HaGL

HaGL consists of expressions having the parameterized type `GLExpr d t`,
where `d` is a label which specifies where the expression will be computed
(its computational "domain") and `t` specifies the raw, underlying type that the
expression wraps. The following table categorizes `GLExpr`'s based on their domain:

| `GLExpr d t`              | Synonym       | Semantics                                                              |
| ------------------------- | ------------- | ---------------------------------------------------------------------- |
| `GLExpr ConstDomain t`    | `ConstExpr t` | A constant of type `t` computed on the CPU host                        |
| `GLExpr HostDomain t`     | `HostExpr t`  | A potentially I/O-dependent value of type `t` computed on the CPU host |
| `GLExpr VertDomain t`     | `VertExpr t`  | A value of type `t` computed on the GPU, in a vertex shader            |
| `GLExpr FragmentDomain t` | `FragExpr t`  | A value of type `t` computed on the GPU, in a fragment shader          | 

The underlying type `t` can be one of the following:

* A primitive type: `Float`, `Double`, `Int`, `UInt`, `Bool`
* A vector `Vec n t` where $1 \leq n \leq 4$ is its length and `t` is a primitive type
* A matrix `Mat p q t` with $1 \leq p \leq 4$ rows and $1 \leq q \leq 4$ columns 
  and `t` is either `Float` or `Double`

Note that the `Vec` and `Mat` types are specific to HaGL but there is usually no
need to work with them directly.

HaGL provides different ways to construct and convert between specific `GLExpr`
types and at the same time allows them to be manipulated polymorphically through
the use of generic functions. 

For example, the following specifies a vertex corresponding to four input 
points:

```
x :: VertExpr (Vec 4 Float)
x = vert [vec4 (-1) (-1) 0 1, vec4 (-1) 1 0 1, vec4 1 (-1) 0 1, vec4 1 1 0 1]
```

Suppose that we call a built-in function such as
```
length :: GLFloating t => GLExpr d (Vec n t) -> GLExpr d t 
```
on `x` to obtain `y = length x`. This is well-typed because `Float` is an instance of
`GLFloating` and `VertExpr (Vec 4 Float)` matches the input type `GLExpr d (Vec n t)`
of `length`. So from the type signature, we can tell that `y` will have type
`VertExpr Float`. 

What does `y` represent? Because `x` specifies four vertices, we can think
of `length x` as a parallel computation of the length of each vertex represented
by `x`. So in some sense `y` is equivalent to
```
y' :: VertExpr Float
y' = vert [length (vec4 (-1) (-1) 0 1), length (vec4 (-1) 1 0 1), ...]
```
However, `y` and `y'` will be computed in different ways since,
based on the type signature `[ConstExpr t] -> VertExpr t ` of `vert`, we know 
that type of the input vertex `length (vec4 (-1) (-1) 0 1)` must be `ConstExpr Float`
and hence computed on the CPU. So we see that types, often implicitly inferred,
play a very important role in determining how a HaGL expression is computed (and
may in many situations also influence its semantics, as we shall see).


## First Example

First, we import the module and enable some useful extensions:
```
{-# LANGUAGE GADTs GADTs -#}
{-# LANGUAGE GADTs DataKinds -#}
{-# LANGUAGE GADTs ViewPatterns -#}

import Graphics.HaGL
```

To draw an object, all we need to is specify a `GLObj` that bundles all the
`GLExpr`'s that specify its properties. A `GLObj` also includes a choice for
a `PrimitiveMode`, which specifies how the input vertices in its `position` field
are to be interpreted. For example, in the primitive mode `Triangles` every
three consecutive position vertices are the points of a triangle primitive. The
easiest way to specify such a `GLObj` is using Haskell's record syntax on the
pre-defined object `triangles`. The only fields we will consider in this guide are
`position :: VertExpr (Vec 4 Float)` and `color :: FragExpr (Vec 4 Float)`:
```
triangles :: GLObj
triangles { position = ..., color = ... }
```
Let us draw two blue triangles, as in the opening example in Ch. 1 of the 
*OpenGL Programming Guide*. To do so, we need to specify the 
[homogenous coordinates](https://en.wikipedia.org/wiki/Homogeneous_coordinates)
`vec4 x y z w` of two triangles; in the `Triangles` primitive mode, this means 
that the first three inputs to `position` will specify the first triangle and 
the next three -- the second:
```
pos :: VertExpr (Vec 4 Float) 
pos = vert 
    [vec4 (-0.9) (-0.9) 0 1, 
     vec4 0.85 (-0.9) 0 1, 
     vec4 (-0.9) 0.85 0 1, 
     vec4 0.9 (-0.85) 0 1, 
     vec4 0.9 0.9 0 1, 
     vec4 (-0.85) 0.9 0 1]
```
Here we are working in a clip coordinate system where all positions
satisfy $-1 \leq x/w, y/w, z/w \leq 1$ or are otherwise clipped.
Each of the two triangles will be colored in the same way, using the color blue:
```
blue :: FragExpr (Vec 4 Float)
blue = vert4 0 0 1 0
```
We can then define our desired object as
```
blueTriangles = triangles { position = pos, color = blue }
```
and readily draw it using:
```
> drawGlut blueTriangles
```
<img src="images/blue_triangles.png" alt="blue_triangles" width=50% height=50% />

To make this example more meaningful, let us try to color the fragments in a 
position-dependent way. We can achieve this using the `frag` function which takes 
in as input a vertex expression and outputs a fragment expression that represents 
an arbitrary interpolation of the input values at the vertices generating the 
corresponding primitive:

```
fpos :: FragExpr (Vec 4 Float)
fpos = frag pos
```

Now let's make the color vary as a function of the `x` coordinate of `pos`, 
using the function `mix` to produce a red-blue gradient:

```
-- normalize the x coordinate to lie in [0, 1]
s = 0.5 * (x_ fpos + 1)

redBlue = mix red blue (vec4 s s s s)

redBlueTriangles = triangles { position = pos, color = redBlue }
```

<img src="images/red_blue_triangles.png" alt="red_blue_triangles" width=50% height=50% />

Note that all examples on this page are drawn on a non-default white background 
(by setting the `openGLSetup` field of `GlutOptions` passed to `draw`).

## Drawing Images

Fragment shaders are interesting in their own right, and producing interesting 
visual and animations by coloring a 2D image is an art in itself.

One approach we can take is similar to the ideas outlined in Ch. 7 of 
*The Fun of Programming* ("Functional images" by Conal Elliott), where an image 
is defined as a mapping from 2D points in the plane to a color (in RGBA space):

```
type ImagePos = FragExpr (Vec 2 Float)
type ImageColor = FragExpr (Vec 4 Float)
type Image = ImagePos -> ImageColor
```

Given an `Image` we can create a `GLObj` as follows:
```
fromImage :: Image -> GLObj
fromImage im = 
    let pos = vert 
            [vec4 (-1) (-1) 0 1, 
             vec4 (-1) 1 0 1, 
             vec4 1 (-1) 0 1, 
             vec4 1 1 0 1]
        fpos = frag quadPos
    in triangleStrip { position = pos, color = im fpos }
```
In the `TriangleStrip` primitive mode every sliding window of three vertices 
defines a triangle; in this case we use two triangles to draw a quad which we 
use as a canvas with endpoints `(-1, -1), (-1, 1), (1, 1), (1, -1)` which we 
color by mapping an interior point `fpos` to `im fpos`.

We can now draw any `Image`:
```
blueCircle :: GLObj
blueCircle = fromImage $ \pos ->
    cast (length pos .<= 0.5) .# vec4 0 0 1 1
```

<img src="images/blue_circle.png" alt="blue_circle" width=50% height=50% />

Note the `cast` from `FragExpr Bool` to `FragExpr Float`, as well as the use of 
the operator (.<) for comparing expressions of type `GLExpr d Bool` and the 
operator (.#) for scalar multiplication.

Suppose that we have two `Image`s and would like to combine them in some way;
this to lifting an operator to the color co-domains of both images via helper
functions of the form:
```
liftToImage1 :: (ImageColor -> ImageColor) -> Image -> Image
liftToImage1 f im x = f (im x)

liftToImage2 :: (ImageColor -> ImageColor -> ImageColor) -> Image -> Image -> Image
liftToImage2 f im1 im2 x = f (im1 x) (im2 x)

...
```

For instance we can define
```
bluePlusRedCircle :: GLObj
bluePlusRedCircle = fromImage $ liftToImage2 (+) blueCircIm redCircIm where
    blueCircIm pos = cast (length (pos + vec2 0.25 0) .<= 0.5) .# vec4 0 0 1 1
    redCircIm pos = cast (length (pos - vec2 0.25 0) .<= 0.5) .# vec4 1 0 0 1
```

<img src="images/blue_plus_red_circle.png" alt="blue_plus_red_circle" width=50% height=50% />

Note that one other way to combine images is to make use of their alpha component 
and the fact that one can `draw` multiple objects at once:

```
blueOverRedCircle :: [GLObj]
blueOverRedCircle = [redCircle, blueCircle] where
    blueCircle = circle (vec2 (-0.25) 0) (vec4 0 0 1 1)
    redCircle = circle (vec2 0.25 0) (vec4 1 0 0 1)
    circle center color = fromImage $ \pos ->
        cast (length (pos - center) .<= 0.5) .# color

-- a [GLObj] is also an instance of Drawable
> drawGlut blueOverRedCircle
```

<img src="images/blue_over_red_circle.png" alt="blue_over_red_circle" width=50% height=50% />

### Transformations of Images

A transformation of an image is simply a function that remaps its domain:

```
type ImageTransform = ImagePos -> ImagePos
```

For example `rotate ang` will rotate an image by `ang` radians:

```
rotate :: FragExpr Float -> ImageTransform
rotate ang pos@(decon -> (x, y)) = 
    let r = length pos
        theta = atan (y / x)
    in r .# vec2 (cos $ theta + ang) (sin $ theta + ang)
```

Here we've introduced the deconstruction operator `decon`, which combined with
view pattern syntax allows us to "pattern match" on a vector.

Let us apply this transformation on a simple checkboard image:

```
checkboardImage :: Image
checkboardImage (decon -> (x, y)) = c .# vec4 1 1 1 1 where
    c = cast $ (floor (10 * x) + floor (10 * y)) `mod` 2 .== 0

rotatedCheckboard :: FragExpr Float -> GLObj
rotatedCheckboard angle = fromImage $ checkboardImage . rotate angle

> drawGlut $ rotatedCheckboard (pi / 4)
```

<img src="images/rotated_checkboard.png" alt="rotated_checkboard" width=50% height=50% />

### Using Uniforms to Animate Images

Many times we need to compute a value on the CPU and load it as an input-independent
value in a shader. The `uniform` function takes in a `HostExpr` and produces
an expression in an arbitrary domain:

```
uniform :: GLType t => HostExpr t -> GLExpr d t 
```

For example, in this expression, the value of `2 * 5` will be computed on the CPU,
whereas the multiplication of the resulting value `10` with the input vertices
will occur in a vertex shader:
```
x :: VertExpr Float
x = uniform (2 * 5) * vert [1, 2, 3] 
```

HaGL provides a few built-in `HostExpr`'s for accessing the I/O state of the
current window being drawn such as `time`, `mouseX`, `mouseY`. This makes it
fairly simple to define interactive behaviour and animation. We can animate our
previous example by passing in `uniform time` as the input angle to `rotatedCheckboard`
instead of a fixed constant:

```
> drwGlut $ rotatedCheckboard (uniform time)
```

<img src="images/rotating_checkboard.png" alt="rotating_checkboard" width=50% height=50% />

Here is a slightly more interesting example:

```
windingsPaths GLObj
windingsPaths = fromImage $ \(decon -> (x, y)) ->
    let curve x t = 0.2 * sin (x + 4 * t)
        distPlot scale y' = 
            smoothstep (y' - 0.05) y' y -
            smoothstep y' (y' + 0.05) y
        greenish = vec4 0.1 0.7 0.3 1 
        redish = vec4 0.7 0.1 0.1 1 
        bluish = vec4 0.1 0.1 0.7 1
        t = uniform time
    in distPlot 150 (curve x t) .# greenish +
       distPlot 250 (curve x (2 * t + 0.5)) .# redish +
       distPlot 600 (curve x (0.5 * t - 0.5)) .# bluish
```

<img src="images/winding_paths.png" alt="winding_paths" width=50% height=50% />

### More Fragment Shading

There are many interesting directions to explore with fragment shaders. 
With the right equations, we can even draw 3D objects:

*[fragSphere](examples/Graphics/HaGL/Examples/Images.hs)*

<img src="images/frag_sphere.png" alt="frag_sphere" width=50% height=50% />

<!-- add an SDF example here !-->

The evolving HaGL library includes a small number of noise-generating 
functions such as `perlinNoise` and `fbm` (fractal Brownian motion). Given a 
seed `s` and a 3D coordinate `xyz`, `perlinNoise s (scale .* xyz)` returns a 
smoothly varying pseudorandom value in the range [-1, 1]:

```
noiseGrid :: GLObj
noiseGrid = fromImageInteractive $ \pos ->
    let xyz = app pos (uniform time / 200)
        nv = perlinNoise 1 (32 .# xyz)
        rgb = nv .# 0.5 + 0.5
    in app rgb 1
```

<img src="images/noise_grid.png" alt="noise_grid" width=50% height=50% />

(The library provides `fromImageInteractive` as an alternative to `fromImage`,
 which additionally supports interactive panning and zooming of the image.)

By combining appropriately scaled noise in various ways, we can create
procedurally generated content such as this terrain map:

```
procgen2DWorld :: GLObj
procgen2DWorld = fromImageInteractive $ \pos ->
    let perlin amp freq = amp * perlinNoise2D 1 (freq .* pos)
        tot = perlin 4 4 + perlin 2 8 + perlin 1 32
        rgb1 = (flip app) 1
    in rgb1 $ cond (tot .<= 0)  (vec3 0 0 1) $
              cond (tot .< 0.5) (vec3 1 1 0) $
                                (vec3 0 1 0)
```

<img src="images/procgen_2d_world.png" alt="procgen_2d_world" width=50% height=50% />


## Vertex Processing

So far we have shown how HaGL can be used to draw objects by operations at the 
`FragmentDomain` level that define colors at individual points of an image. 
In this section, we show how operations at the `VertexDomain` level can be used
to manipulate the underlying geometry in interesting ways.

### Three-dimensional Drawing

If we think of a 3D object as a mesh consisting of vertices and triangle faces, 
then one way to draw it is to use the `Triangles` primitive mode, where the we
specify the positions of all the triangles vertices via the `position` field
of the `GLObj` and specify the faces of each triangle via the `indices` field.
Every three consecutive indices determines the position of a triangle primitive.
Note that when we omitted the indices before, we were implicitly defining them as
```
    [0,1,2, 3,4,5, 6,7,8, ...]
```
but this is inadequate when vertices have to be shared.

To draw something like a cube we can therefore start by defining the vertices
and faces of the twelve triangles it's comprised of (two triangles for every 
face of the cube):

```
vertices :: [ConstExpr (Vec 4 Float)]
vertices =
    [vec4 1 1 1 1,
     vec4 (-1) 1 1 1,
     vec4 (-1) (-1) 1 1, 
     vec4 1 (-1) 1 1,
     vec4 1 (-1) (-1) 1, 
     vec4 1 1 (-1) 1, 
     vec4 (-1) 1 (-1) 1, 
     vec4 (-1) (-1) (-1) 1]

faces :: [ConstExpr UInt]
faces = [0,1,2, 0,2,3, 0,3,4, 0,4,5, 0,5,6, 0,6,1,
         1,6,7, 1,7,2, 7,4,3, 7,3,2, 4,7,6, 4,6,5]
```

The position of a vertex is then the expression

```
pos :: VertExpr (Vec 4 Float)
pos = vert vertices
```

Note that we defined `vertices` and hence also `pos` in terms of *world space*
coordinates; that is, our intention is to draw a cube with side length equal to 
two, centered at the origin of our world's coordinate system.

To transform `pos` to *view space* (or eye space), we need to define a
transformation matrix `view :: HostExpr` (assumed to lie in `HostDomain` since 
we normally expect it to be updated in some interactive manner). We can use
convenience functions defined in `Graphics.HaGL.Lib.Math` to create affine
transformation matrices, for instance `rotate`, which creates a rotation matrix
from a given axis and angle, and `translate`, which creates a translation matrix
from a given translation vector:
```
rotate :: _ => GLExpr d (Vec 3 t) -> GLExpr d t -> GLExpr d (Mat 4 4 t)
translate :: _ => GLExpr d (Vec 3 t) -> GLExpr d (Mat 4 4 t)
```
Let us define `view` to simulate a camera orbiting the cube around the line $x=y=z$
(with an orbital radius of five units):
```
view :: HostExpr (Mat 4 4 Float)
view = 
    let initialEye = vec3 0 0 5
        axis = normalize $ vec3 1 1 1
        angle = time
    in translate (-initialEye) .@ rotate axis angle
```
Transforming `pos` to view space, we obtain:
```
vpos :: VerteExpr (Vec 4 Float)
vpos = uniform view .@ pos
```

Finally, we need to project `vpos` to the *clip coordinates* suitable for
specifying the `position` field of a `GLObj`. `Graphics.HaGL.Lib.Math` provides 
the functions `perspective`, `perspective'`, and `orthogonal` for creating 
projection matrices. For instance, we can define `proj` in terms of some chosen
field of view angle (in the y direction), aspect, and near and far values:
```
proj :: HostExpr (Mat 4 4 Float)
proj = perspective (pi / 6) 1 1 10
```

Transforming `vpos` to clip space, we obtain
```
cpos :: VertExpr (Vec 4 Float)
cpos = uniform proj .@ vpos
```

Note that `cpos` is equivalent to the expression
```
(uniform proj .@ uniform view) .@ pos
```
where `unform proj` and `uniform view` are both of type `VertExpr (Mat 4 4 Float)`.
This means that the two matrices will be multiplied in the vertex shader, which
is not quite what we want since they do not depend on any vertex
and can thus be pre-computed on the CPU. We can instead define `cpos` in terms
of the equivalent but more efficient expression
```
uniform (proj .@ view) .@ pos
```

We are now ready to draw an animation of an orbit around a cube:
```
cube = triangles { indices = Just faces, position = pos, color = color }
drawGlut cube
```
<img src="images/rotating_cube.png" alt="rotating_cube" width=50% height=50% />

### Adding Interactivity

The function `interactiveView` defined in `Graphics.HaGL.Lib.Camera` creates a 
view that can be panned and zoomed by the user. So to convert the above example
to an interactive one, all need to do is modify our definition of `view` to be
`interactiveView initialEye` for some chosen position of `initialEye`.

### Drawing Particles

The `GLObj` `points` corresponds to the `Points` primitive mode, where each
input position specifies that of a single point. With the help of a few library
functions for pseudorandom number generation, we can use it to create simple
particle systems:

```
explosion :: GLObj
explosion = 
    let s = vert [vec2 i j | i <- [-30..30], j <- [-30..30]]
        speed = randFloat21 s ** 2
        pos = (uniform time * speed / 10) .# randDir s
        cpos = uniform (interactiveView $ vec3 0 0 1) .@ app pos 1
        col = mix (vec4 1 0 0 1) (vec4 1 1 0 1) (frag speed)
    in points { position = cpos, color = col }
```

<img src="images/explosion.png" alt="explosion" width=50% height=50% />

### Drawing Curves and Surfaces

### Shading

