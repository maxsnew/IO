IO in Elm [![Build Status](https://travis-ci.org/maxsnew/IO.png?branch=master)](https://travis-ci.org/maxsnew/IO)
=========

This repo provides a library for writing console-based programs in
Elm.

Getting Started
---------------

Before you begin, compile and pull in node.js dependencies:

```
$ npm install jsdom
```

(On Windows, `jsdom` is somewhat difficult to install. [Refer to this blog post](http://www.steveworkman.com/node-js/2012/installing-jsdom-on-windows/) for detailed instructions)

Example
-------
An elm Program:
```haskell
module Main where

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

import List
import Maybe
import String

echo : IO ()
echo = forever (getLine >>= putStrLn)

loop : IO ()
loop = getLine >>= \s ->
       if s == "exit"
       then pure ()
       else putStrLn s >>> loop
       
hello : IO ()
hello = putStrLn "Hello, Console!" >>>
        putStrLn "I'll echo your input until you say \"exit\":" >>>
        loop >>>
        putStrLn "That's all, folks! Here's some blahs:"  >>>
        putStrLn (String.concat <| List.repeat 100000 "blah ") >>>
        exit 0

port requests : Signal Request
port requests = Run.run responses hello

port responses : Signal Response
```

link in some javascript and then run:
```
$ elm-make --yes Test.elm test.js
...
$ ./elm-io.sh test.js
Hello, Console!
I'll echo your input:
hooray
hooray
That's all, folks!
```

Command Line Interface
----------------------

The basic interface is `elm-io infile outfile`, where `infile` is a
compiled Elm file with requests and response signals set up as above
and `outfile` is the desired filename for the runnable JavaScript file
to be run with node. `elm-io` will automatically include its own data

Troubleshooting
---------------
If you have a problem first make sure:

  1. You have jsdom installed

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
