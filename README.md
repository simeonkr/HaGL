# HaGL: Haskell-embedded OpenGL

HaGL (Haskell-embedded OpenGL, pronounced "haggle") aims to provide a simple,
elegant, and composable way to create lightweight OpenGL visualizations in Haskell.

By unifying the different types of numerical computations
that comprise an OpenGL program — whether intended to run on the CPU or as a 
shader program on the GPU — into a high-level language consisting of pure, 
composable primitives, HaGL makes it easy to experiment with and prototype visual 
graphical demos in a modular way.

Since the numerical functions provided by HaGL are highly consistent with those
comprising GLSL, writing a HaGL program can be intuitively similar to writing a 
shader. Nonetheless, a HaGL program can easily specify not just a single 
shader but an entire application, while at the same time making explicit where its
constituent parts are computed through the use of Haskell's expressive type system.

## Installation

With the latest version of Haskell installed:

```
cabal update
cabal install HaGL
```

## Usage

To learn how to use HaGL please refer to the [Getting started guide](Tutorial.md), 
as well as the the [demo examples](examples/Graphics/HaGL/Examples) making use of 
the [demo library](src/Graphics/HaGL/Lib). The complete documentation can be found 
on [Hackage](https://hackage.haskell.org/package/HaGL).

## Running the Test Suite

The test suite can be run using:

```
cabal test
```

Note that since some tests require launching a graphical window, the test suite
cannot be run on a non-GUI system.

## Intended Use and Alternatives

HaGL is best suited for the production of simple non-reactive visual animations; 
in this regard, its use-cases somewhat resemble those of the [VisPy](https://vispy.org/) 
and [Glumpy](https://glumpy.github.io/) Python libraries.

At the moment, the only way to interpret HaGL code is as GLUT application but 
other backends will be added in the future, as well as a backend-agnostic interface.

HaGL prioritizes approachability and simplicity over completeness and is likely 
unsuitable for use in a game engine, certainly not in its current state. For 
more advanced graphics programming consider using, for example: 
[LambdaCube 3D](http://lambdacube3d.com/), [FIR](https://gitlab.com/sheaf/fir),
or the Haskell [OpenGL](https://hackage.haskell.org/package/OpenGL) bindings. 

## Current Status and Roadmap

Implementation-wise the core of HaGL is comprised of

* A high-level API consisting of parameterized expressions with strict
type constraints
* A code generator for vertex and fragment shaders, supporting variants of all 
applicable GLSL built-in operators and functions
* A numerical evaluator that can process these same functions on the CPU, thus
enabling the use of arbitrary uniforms
* Various functions that enable initialization and transfer of 
data, the specification of user-defined shader functions, and the use of basic 
interactivity

An "engine" for running HaGL code is provided in the form of GLUT backend built
using the the [OpenGL](https://hackage.haskell.org/package/OpenGL) and 
[GLUT](https://hackage.haskell.org/package/GLUT) bindings for Haskell. 

In the future, support will hopefully be added for:

* Textures in fragment shaders
* Tessellation shaders
* Geometry shaders
* Compute shaders
* New backends and a backend-agnostic interface
* Integration with an FRP framework such as [Yampa](https://hackage.haskell.org/package/Yampa)

## Contributing

Contributions in the form of pull requests, suggestions, or feedback are welcome
and appreciated.

The best way to keep the project alive is to demonstrate its usefulness through
a wide range of interesting examples. Another potential area to explore is the
creation of high-level libraries for specific types of visual applications.
