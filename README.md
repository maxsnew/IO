IO in Elm
=========

This repo provides a library for writing console-based programs in
Elm.

An elm Program:
```haskell
hello : IO ()
hello = putStrLn "Hello, Console!" >>
        putStrLn "I'll echo your input:" >>
        (getLine >>= putStrLn) >>
        putStrLn "That's all, folks!" >>
        exit 0
```
when run:
```
$ ./mkExe
$ ./runTest
Hello, Console!                   
I'll echo your input:
hooray
hooray
That's all, folks!
```

Implementation/Design
---------------------
The basic IO construct is a free monad, as inspired by the
[IOSpec](http://hackage.haskell.org/package/IOSpec) haskell
library. Building something of type `IO a` makes an IO program that
can be run by a runtime system. The `run` function in `IO.Runner`
turns an `IO` program into an actor that communicates with a
handler. The handler can then communicate with the `IO` actor if the
ports are set up correctly. See the `mkExe` file and the files it uses
to see how this is implemented.

The implementation currently uses putChar/getChar/exit as its
primitives, and other actions like putStrLn are built on top of
those. This is not optimal performance-wise, but is a proof-of-concept
that the `IO` type can be used to build complex programs from smaller
ones.
