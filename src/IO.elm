module IO (putChar, putStr, putStrLn, getChar, getLine, readUntil, writeFile,
           exit, map, mapIO, forEach, pure, apply, (<*>), andThen, (>>=),
           seq, (>>>), forever, IO, run) where

{-|

A library for writing terminal-based scripts in elm.  The IO type
provides an interface for constructing "computations" that may perform
IO effects. Something with type `IO a` is a lazy computation that when
run will produce an `a`, possibly IO side effects. See IO.Runner for
how to run such a computation.

# IO Type
@docs IO, run

# Stdout
@docs putChar, putStr, putStrLn

# Stdin
@docs getChar, getLine, readUntil

# File Operations
@docs writeFile

# Exit code
@docs exit

# Plumbing
@docs map, mapIO, forEach, pure, apply,
      (<*>), andThen, (>>=), seq, (>>>), forever
-}

import Dict
import Json.Decode exposing ((:=))
import Json.Decode as JSD
import Json.Encode as JSE
import List exposing ((::))
import List
import Result
import Signal exposing (Signal, (<~), foldp)
import String
import Task exposing (Task)
import Trampoline

import IO.NativeCom as NativeCom
import IO.NativeCom as NC
import IO.NativeCom exposing (IRequest, IResponse)

-- User-facing API

-- IO Actions
{-| Print a character to stdout -}
putChar : Char -> IO ()
putChar c = Impure (PutS (String.cons c "") (\_ -> Pure ()))

{-| Read a character from stdin -}
getChar : IO Char
getChar = Impure (GetC Pure)

{-| Exit the program with the given exit code. -}
exit : Int -> IO ()
exit = Impure << Exit

{-| Print a string to stdout. -}
putStr : String -> IO ()
putStr s = Impure (PutS s (\_ -> Pure ()))

{-| Print a string to stdout, followed by a newline. -}
putStrLn : String -> IO ()
putStrLn s = putStr s >>> putChar '\n'

{-| Read characters from stdin until one matches the given character. -}
readUntil : Char -> IO String
readUntil end = let go s = getChar >>= \c ->
                           if c == end
                           then pure s
                           else go (String.append s (String.cons c ""))
                in go ""

{-| Write content to a file -}
writeFile : { file : String, content : String } -> IO ()
writeFile obj = Impure (WriteF obj (\_ -> Pure ()))

{-| Read a line from stdin -}
getLine : IO String
getLine = readUntil '\n'

-- | IO Combinators

{-| Apply a pure function to an IO value -}
map : (a -> b) -> IO a -> IO b
map f io = case io of
  Pure   a   -> Pure (f a)
  Impure iof -> Impure (mapF (map f) iof)

{-| Alternative interface to forEach  -}
mapIO : (a -> IO ()) -> List a -> IO ()
mapIO f xs = List.foldr (seq << f) (pure ()) xs

{-| Run an IO computation for each element of a list -}
forEach : List a -> (a -> IO ()) -> IO ()
forEach xs f = mapIO f xs

{-| Use a pure value where an IO computation is expected. -}
pure : a -> IO a
pure = Pure

{-| Apply an IO function to an IO value -}
apply : IO (a -> b) -> IO a -> IO b
apply iof iom = iof >>= \f ->
                iom >>= \m ->
                pure (f m)

{-| Convenient operator for apply, similar to ~ in the Signal module -}
(<*>) : IO (a -> b) -> IO a -> IO b
(<*>) = apply

{-| Chain together IO computations -}
andThen : IO a -> (a -> IO b) -> IO b
andThen io f = case io of
  Pure x     -> f x
  Impure iof -> Impure (mapF (flip andThen f) iof)

{-| Operator version of andThen -}
(>>=) : IO a -> (a -> IO b) -> IO b
(>>=) = andThen

{-| Run one computation and then another, ignoring the first's output -}
seq : IO a -> IO b -> IO b
seq x y = x >>= \_ -> y

{-| Operator version of seq -}
(>>>) : IO a -> IO b -> IO b
(>>>) = seq

-- Has to be >>= not >>> because of strictness!
{-| Run the same computation over and over again forever. -}
forever : IO a -> IO ()
forever m = m >>= (\_ -> forever m)

{-| IMPLEMENTATION DETAILS!!! -}
type IOF a = PutS String (() -> a)    -- ^ the a is the next computation
           | GetC (Char -> a) -- ^ the (Char -> a) is the continuation
           | Exit Int
           | WriteF { file : String, content : String} (() -> a)

-- The type of I/O computations.
{-| An `IO a` is a computation that does some I/O and eventually
    returns an `a` -}
type IO a = Pure a
          | Impure (IOF (IO a))

mapF : (a -> b) -> IOF a -> IOF b
mapF f iof = case iof of
  PutS p k -> PutS p (f << k)
  GetC k   -> GetC (f << k)
  Exit n   -> Exit n
  WriteF obj k -> WriteF obj (f << k)

{- *****************************************
   run
   *****************************************
-}

type alias IOState  = { buffer : String }

start : IOState
start = { buffer = "" }

{-| Run an IO computation as a Task -}
run : IO () -> Signal (Task x ())
run io =
  let init               = (\_ -> io, start, [NC.Init])
      f resp (io, st, _) = step resp io st
      third (_, _, z)    = z
  in NativeCom.sendRequests (third <~ foldp f init NativeCom.responses)

putS : String -> IRequest
putS = NC.Put

exit' : Int -> IRequest
exit' = NC.Exit

getS : IRequest
getS = NC.Get

writeF : { file : String, content : String } -> IRequest
writeF = NC.WriteFile

-- | Extract all of the requests that can be run now
extractRequests : IO a -> State IOState (List IRequest, () -> IO a)
extractRequests io =
  case io of
    Pure x -> spure ([exit' 0], \_ -> Pure x)
    Impure iof -> case iof of
      PutS s k     -> mapSt (mapFst (\rs -> putS s :: rs)) <| spure ([], k)
      WriteF obj k -> mapSt (mapFst (\rs -> writeF obj :: rs)) <| spure ([], k)
      Exit n       -> spure ([exit' n], \_ -> io)
      GetC k       ->
        get >>=# \st ->
        case String.uncons st.buffer of
          Nothing -> spure ([getS], \_ -> io)
          Just (c, rest) ->
            put ({ buffer = rest }) >>=# \_ ->
            extractRequests (k c)

flattenReqs : List IRequest -> List IRequest
flattenReqs rs =
  let loop rs acc n =
    if n >= 100
    then Trampoline.Continue (\_ -> loop rs acc 0)
    else
      case rs of
        []  -> Trampoline.Done <| List.reverse acc
        [r] -> loop [] (r::acc) (n+1)
        r1 :: r2 :: rs' ->
        case (r1, r2) of
          (NC.Exit n, _)      -> loop [] (r1::acc) (n+1)
          (NC.Put s1, NC.Put s2) -> loop (putS (s1++s2) :: rs') acc (n+1)
          _                -> loop (r2::rs') (r1::acc) (n+1)
    in Trampoline.trampoline <| loop rs [] 0

-- | We send a batch job of requests, all requests until IO blocks
step : IResponse ->
       (() -> IO a) ->
       IOState ->
       (() -> IO a, IOState, List IRequest)
step resp io st =
  let newST = case resp of
        Nothing -> st
        Just s  -> { st | buffer <- String.append st.buffer s }
      (newST', (rs, k)) = extractRequests (io ()) newST
  in (k, newST', rs)

-- | State monad
type alias State s a = s -> (s, a)

spure : a -> State s a
spure x = \s -> (s, x)

mapSt : (a -> b) -> State s a -> State s b
mapSt f sf = sf >>=# (spure << f)

(>>=#) : State s a -> (a -> State s b) -> State s b
f >>=# k = \s -> let (s', y) = f s
                in k y s'

get : State s s
get = \s -> (s, s)

put : s -> State s ()
put s = \_ -> (s, ())

mapFst : (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x, y)
