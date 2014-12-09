IO in Elm [![Build Status](https://travis-ci.org/maxsnew/IO.png?branch=master)](https://travis-ci.org/maxsnew/IO)
=========

This repo provides a library for writing console-based programs in
Elm.

Getting Started
---------------

Before you begin, compile and pull in the `jsdom` dependency. node.js
by default installs dependencies locally so you should do this for
each elm project you have using `IO`.

```
$ npm install jsdom
```

(On Windows, `jsdom` is somewhat difficult to install. [Refer to this
blog
post](http://www.steveworkman.com/node-js/2012/installing-jsdom-on-windows/)
for detailed instructions)

Example
-------
An elm Program:
```haskell
module Main where

import IO.IO (..)
import IO.Runner (Request, Response, run)

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
port requests = run responses hello

port responses : Signal Response
```

link in some javascript and then run:
```
$ elm-make --yes Test.elm raw-test.js
...
$ ./elm-io.sh raw-test.js test.js
$ node test.js
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
and `outfile` is the desired filename for the compiled output.

Troubleshooting
---------------
If you have a problem first make sure:

  1. You have jsdom installed

Design and Implementation
-------------------------
The implementation is based on the
[IOSpec](http://hackage.haskell.org/package/IOSpec) haskell library.