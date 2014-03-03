IO in Elm
=========

This repo provides a library for writing console-based programs in
Elm.

Getting Started
---------------

Before you begin, you must pull in both the Elm and node.js dependencies:

```
$ cabal install elm-get # If you don't already have it installed
$ cabal install
$ elm-get install evancz/automaton
$ npm install jsdom
```

(On Windows, `jsdom` is somewhat difficult to install. [Refer to this blog post](http://www.steveworkman.com/node-js/2012/installing-jsdom-on-windows/) for detailed instructions)

Example
-------
An elm Program:
```haskell
import open IO.IO
import IO.Runner (Request, Response)
import IO.Runner as Run

hello : IO ()
hello = putStrLn "Hello, Console!" >>
        putStrLn "I'll echo your input:" >>
        (getLine >>= putStrLn) >>
        putStrLn "That's all, folks!" >>
        exit 0
```
with some boilerplate
```haskell
-- | Can't use a type alias in ports, yet :/
port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses hello

port responses : Signal (Maybe String)
```
link in some javascript and then run:
```
$ cabal run Test.elm hello.js
$ node hello.js
Hello, Console!
I'll echo your input:
hooray
hooray
That's all, folks!
```

Command Line Interface
----------------------
The basic interface is `elm-io infile outfile`, where `infile` is an
Elm source file to compile and `outfile` is the resulting JavaScript
file to be run with node. `elm-io` will automatically include its own
data directory as a `--src-dir` argument to the Elm compiler, meaning
you don't have to manually copy the IO library to your project's directory.
The `infile` module MUST be `Main`, that is to say that the source file must
have `module Main where` as the first line. This restriction may be lifted
in the future.

There is currently one optional flag: `--default-ports`. Usage is like this:
`elm-io --default-ports infile outfile`. If this flag is present, the port
boilerplate seen in the above example will be appended to the end of your Elm
source file. In this configuration, you must define a function `console : IO ()`
which effectively takes the place of the `main : Signal Element` function in
normal graphical Elm programs. A full, yet basic example of a program written
with this flag is:
```haskell
module Main where

import open IO.IO

console : IO ()
console = putStrLn "This is a test" >>
          exit 0
```
When this file is compiled like `elm-io --default-ports Test.elm test.js`, the
following code will be inserted after the module delcaration:
```haskell
import IO.Runner (Request, Response)
import IO.Runner as Run
```
and the following code will be appended to the end of the file:
```haskell
port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses console

port responses : Signal (Maybe String)
```
The file will then be compiled to `test.js`, which is runnable with node.
Keep in mind that the `evancz/automaton` and `jsdom` dependencies must still
be installed in each project directory!

A working example can be seen in the [Elm-Test Travis CI configuration](https://github.com/deadfoxygrandpa/Elm-Test/blob/master/.travis.yml).

Design and Implementation
-------------------------
The basic IO construct is a free monad, inspired by the
[IOSpec](http://hackage.haskell.org/package/IOSpec) haskell
library. Building something of type `IO a` makes an IO program that
can be run by a runtime system, yielding an `a`. The `run` function in
`IO.Runner` turns an `IO` program into an actor that communicates with
a handler. The handler can then communicate with the `IO` actor if the
ports are set up correctly. See the `Test.elm`, `mkExe`,
`prescript.js` and `handler.js` files to see how this is implemented.

The implementation currently uses putChar/getChar/exit as its
primitives, and other actions like putStrLn are built on top of
those. This is not optimal performance-wise, but is a proof-of-concept
that the `IO` type can be used to build complex programs from smaller
ones.
