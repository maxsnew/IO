module IO.Runner where

import Automaton as Auto

import IO.IO as IO
import IO.IO (IO)

data Request  = PutC Char
data Response = Start
data IOState  = Going | Done

run : IO () -> Signal Response -> Signal (Maybe Request)
run io = Auto.run (Auto.hiddenState (io, Going) step) Nothing

step : Response -> (IO a, IOState) -> ((IO a, IOState), Maybe Request)
step resp (io, st) = case st of
  Going -> case resp of
    Start -> case io of
      IO.Pure _ -> ((io, Done), Nothing)
      IO.Impure iof -> case iof of
        IO.PutC c k -> ((k, Going), Just <| PutC c)
  Done -> ((io, st), Nothing)
