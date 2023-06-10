# HaGL: Haskell-embedded OpenGL

HaGL (Haskell-embedded OpenGL, pronounced "haggle") aims to provide a simple,
elegant, and modular way to create lightweight OpenGL visualizations in Haskell.

By unifying vertex, fragment, and uniform processing into an expressively
typed GLSL-like language consisting of pure, composable primitives, HaGL makes
it easy to experiment with and prototype visual graphical demos.

To see it in action, please skip straight to the 
["Getting Started" guide](https://github.com/simeonkr/HaGL/blob/master/doc/Overview.md).

## Installation

Please make sure that you have installed the correct headers and static libraries 
for OpenGL development on your system, as well as freeglut for running the GLUT
backend. (For example, in Debian/Ubuntu, ensure that the packages `libgl-dev`, 
`lib-glu1-mesa-dev`, and `freeglut3` are installed.)

Then with the latest version of [Haskell Cabal](https://www.haskell.org/cabal/) installed:

```
cabal update
cabal install HaGL
```

To instead build locally from the sources, issue:
```
cabal build HaGL
```
from the top-level directory.

The supporting library and examples can be built using:
```
cabal build HaGL-lib
cabal build HaGL-examples
```

(It is recommended that these packages be built not installed, as they are not
yet stable and complete enough to be used without modifications.)

To check that all the packages have been installed correctly, run a single
example as follows:
```
cabal run hagl-example interactive_cube
```

## Usage

To learn how to use HaGL please refer to the 
["Getting Started" guide](https://github.com/simeonkr/HaGL/blob/master/doc/Overview.md).
The complete documentation can be found on 
[Hackage](https://hackage.haskell.org/package/HaGL).

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

At the moment, the only way to interpret HaGL code is as a GLUT application but 
other backends will be added in the future, as well as a backend-agnostic interface.

HaGL prioritizes approachability and simplicity over completeness and is likely 
unsuitable for use in a game engine. 
For more advanced graphics programming consider using, for example, 
[LambdaCube 3D](http://lambdacube3d.com/), [FIR](https://gitlab.com/sheaf/fir),
or the Haskell [OpenGL](https://hackage.haskell.org/package/OpenGL) bindings. 

## Scope of OpenGL Support

HaGL models a simple OpenGL pipeline that supports basic vertex and fragment 
processing and exposes a generic API similar to that of GLSL that can be 
used for both shader programming and host numerical computation (of uniforms).
In addition, it provides various operations of its own to model certain 
imperative constructs in a functional manner.

An "interpreter" for running HaGL code is provided in the form of GLUT backend 
built using the [OpenGL](https://hackage.haskell.org/package/OpenGL) and 
[GLUT](https://hackage.haskell.org/package/GLUT) bindings for Haskell. 

Features that are currently missing but could possibly be added in the future:

* Texture mapping using sampler objects
* Instanced rendering
* Tessellation shading
* Geometry shading
* Compute shading
* Non-GLUT backends and a backend-agnostic interface
* Integration with an FRP framework such as [Yampa](https://hackage.haskell.org/package/Yampa)

## Contributing

Contributions in the form of pull requests, suggestions, or feedback are welcome
and appreciated.

The best way to keep the project alive is to demonstrate its usefulness through
a wide range of interesting examples and tutorials. Another potential area to
explore is the creation of high-level libraries for specific types of visual 
applications, as well as improving the supporting library that ships with HaGL.
