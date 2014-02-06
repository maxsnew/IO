module IO.Runner where

import Automaton as Auto
import open Either

import IO.IO as IO
import IO.IO (IO)

type Request  = { mPut  : Maybe String
                , mExit : Maybe Int
                , mGet  : Bool
                }
type Response = Maybe String
type IOState  = { state  : BehState
                , buffer : String
                }
data BehState = Waiting | Going | Done


orSig : Signal a -> Signal b -> Signal (Either a b)
orSig s1 s2 = merge (Left <~ s1) (Right <~ s2)

start : IOState 
start = { state = Waiting, buffer = "" }

run : Signal Response -> IO () -> Signal Request
run resps io = let pump = orSig ((\_ -> ()) <~ every millisecond) resps
               in Auto.run (Auto.hiddenState (io, start) step) empty pump

empty : Request
empty = { mPut = Nothing, mExit = Nothing, mGet = False }

putS : String -> Request
putS s = { empty | mPut <- Just s }

exit : Int -> Request
exit n = { empty | mExit <- Just n }

getS : Request
getS = { empty | mGet <- True }

(.~) : IOState -> BehState -> IOState
st .~ b = { st | state <- b }

step : Either () Response -> (IO a, IOState) -> ((IO a, IOState), Request)
step resp (io, st) = 
  case st.state of
    Waiting -> case resp of
      Left  () -> ((io, st), empty)
      Right _  -> step resp (io, st .~ Going)
    Done -> ((io, st), exit 0)
    Going -> case resp of
      Right (Just s) ->
        let newST = { st | buffer <- String.append st.buffer s }
        in ((io, newST), empty)
      _ -> case io of
        IO.Pure _ -> ((io, st .~ Done), exit 0)
        IO.Impure iof -> case iof of
          IO.PutC c k -> ((k, st .~ Going), putS <| String.cons c "")
          IO.Exit n   -> ((io, st .~ Done), exit n)
          IO.GetC k   -> case String.uncons st.buffer of
            Nothing        -> ((io, st), getS)
            Just (c, rest) -> let newST = { st | buffer <- rest }
                              in ((k c, newST), empty)
