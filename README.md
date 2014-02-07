IO in Elm
=========

This repo provides a library for writing console-based programs in
Elm.

Getting Started
---------------

Before you begin, you must pull in both the Elm and node.js dependencies:

```elm-get install evancz/automaton```

```npm install jsdom```
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
port requests = Run.run responses echo

port responses : Signal (Maybe String)
```

link in some javascript and then run:

```
$ ./mkExe
$ ./runTest
Hello, Console!                   
I'll echo your input:
hooray
hooray
That's all, folks!
```

Or on Windows:

```
>runhaskell mkExe.hs
>node runTest.js
Hello, Console!                   
I'll echo your input:
hooray
hooray
That's all, folks!
```

Design and Implementation
-------------------------
The basic IO construct is a free monad, inspired by the
[IOSpec](http://hackage.haskell.org/package/IOSpec) haskell
library. Building something of type `IO a` makes an IO program that
can be run by a runtime system, yielding an `a`. The `run` function in
`IO.Runner` turns an `IO` program into an actor that communicates with
a handler. The handler can then communicate with the `IO` actor if the
ports are set up correctly. See the `Test.elm`, `mkExe`,
`prescript.sh` and `handler.js` files to see how this is implemented.

The implementation currently uses putChar/getChar/exit as its
primitives, and other actions like putStrLn are built on top of
those. This is not optimal performance-wise, but is a proof-of-concept
that the `IO` type can be used to build complex programs from smaller
ones.
