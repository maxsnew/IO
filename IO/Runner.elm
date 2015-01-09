module IO.Runner(run) where

{-| Once you've constructed your IO computation `foo : IO ()`, you
can run it by adding the following to the file (Foo.elm) you're running:

port requests : Signal IO.Runner.Request
port requests = IO.Runner.run responses foo

port responses : Signal IO.Runner.Response

Then run `elm-make Foo.elm --output foo.js` to compile the program and
`elm-io.sh foo.js` to run it. You will need `node` installed to run
the program. Also you will need `npm install jsdom` in the directory
where you are running the file or globally. The `elm-io.sh` script can
be downloaded here:
https://raw.githubusercontent.com/maxsnew/IO/master/elm-io.sh

-}

import Graphics.Element (Element)
import Trampoline

import IO.IO as IO
import IO.IO (IO)
import Native.IO

-- type alias IOState  = { buffer : String }

-- start : IOState 
-- start = { buffer = "" }

-- | What should the return type be?
run : IO () -> Signal ()
run io =
    let loop fuel io =
            if fuel <= 0
            then Trampoline.Continue (\_ -> loop 100 io)
            else
                case io of
                  IO.Pure _ -> Trampoline.Done (Native.IO.exit 0)
                  IO.Impure iof -> case iof of
                      IO.PutS s k ->
                          loop (fuel - 1) (k (Native.IO.putS s))
                      IO.Exit n -> Trampoline.Done (Native.IO.exit n)
                  
    in Trampoline.trampoline (loop 100 io)

-- | State monad
-- type alias State s a = s -> (s, a)

-- pure : a -> State s a
-- pure x = \s -> (s, x)

-- mapSt : (a -> b) -> State s a -> State s b
-- mapSt f sf = sf >>= (pure << f)

-- (>>=) : State s a -> (a -> State s b) -> State s b
-- f >>= k = \s -> let (s', y) = f s
--                 in k y s'

-- ask : State s s
-- ask = \s -> (s, s)

-- put : s -> State s ()
-- put s = \_ -> (s, ())

-- mapFst : (a -> b) -> (a,c) -> (b,c)
-- mapFst f (x,y) = (f x, y)
