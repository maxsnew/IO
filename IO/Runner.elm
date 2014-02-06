module IO.Runner where

import Automaton as Auto
import open Either

import IO.IO as IO
import IO.IO (IO)

type Request  = { mPut  : Maybe String
                , mExit : Maybe Int
                }
type Response = ()
data IOState  = Waiting | Going | Done

orSig : Signal a -> Signal b -> Signal (Either a b)
orSig s1 s2 = merge (Left <~ s1) (Right <~ s2)

run : IO () -> Signal Response -> Signal (Maybe Request)
run io resps = let pump = orSig ((\_ -> ()) <~ every millisecond) resps
               in Auto.run (Auto.hiddenState (io, Going) step) Nothing pump

empty : Request
empty = { mPut = Nothing, mExit = Nothing }

putS : String -> Request
putS s = { empty | mPut <- Just s }

exit : Int -> Request
exit n = { empty | mExit <- Just n }

step : Either () Response -> (IO a, IOState) -> ((IO a, IOState), Maybe Request)
step resp (io, st) = 
  case st of
    Waiting -> case resp of
      Left  () -> ((io, st), Nothing)
      Right _  -> step resp (io, Going)
    Done -> ((io, st), Just <| exit 0)
    Going ->
      case io of
        IO.Pure _ -> ((io, Done), Just <| exit 0)
        IO.Impure iof -> case iof of
          IO.PutC c k -> ((k, Going), Just . putS <| String.cons c "")
          IO.Exit n   -> ((io, Done), Just <| exit n)
