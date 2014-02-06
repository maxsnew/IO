module IO.Runner where

import Automaton as Auto
import open Either

import IO.IO as IO
import IO.IO (IO)

type Request  = String
type Response = ()
data IOState  = Going | Done

-- toAuto : IO () -> Auto.Automaton Respone (Maybe Request)
-- toAuto io = Auto.hiddenState (io, Going)

orSig : Signal a -> Signal b -> Signal (Either a b)
orSig s1 s2 = merge (Left <~ s1) (Right <~ s2)

run : IO () -> Signal Response -> Signal (Maybe Request)
run io resps = let pump = orSig ((\_ -> ()) <~ every millisecond) resps
               in Auto.run (Auto.hiddenState (io, Going) step) Nothing pump

step : Either () Response -> (IO a, IOState) -> ((IO a, IOState), Maybe Request)
step resp (io, st) = 
  case st of
    Done -> ((io, st), Nothing)
    Going ->
      case io of
        IO.Pure _ -> ((io, Done), Nothing)
        IO.Impure iof -> case iof of
          IO.PutC c k -> ((k, Going), Just <| String.cons c "")
    
