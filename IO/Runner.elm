module IO.Runner where

import Automaton as Auto
import Dict
import Either (..)
import Json as JSON
import Trampoline
    
import IO.IO as IO
import IO.IO (IO)

data Request = Put String
             | Exit Int
             | Get

type Response = Maybe String
type IOState  = { buffer : String }

orSig : Signal a -> Signal b -> Signal (Either a b)
orSig s1 s2 = merge (Left <~ s1) (Right <~ s2)

start : IOState 
start = { buffer = "" }

run : Signal Response -> IO () -> Signal JSON.Value
run resps io = serialize <~ Auto.run (Auto.hiddenState (io, start) step) [] resps

serialize : [Request] -> JSON.Value
serialize =
  let mkObj = JSON.Object . Dict.fromList
      serReq req = 
        case req of
          Put s -> mkObj [ ("ctor", JSON.String "Put")
                         , ("val",  JSON.String s)
                         ]
          Get -> mkObj [ ("ctor", JSON.String "Get") ]
          Exit n -> mkObj [ ("ctor", JSON.String "Exit")
                          , ("val", JSON.Number . toFloat <| n )
                          ]
    in JSON.Array . map serReq

putS : String -> Request
putS = Put

exit : Int -> Request
exit = Exit

getS : Request
getS = Get

-- | Extract all of the requests that can be run now
extractRequests : IO a -> State IOState ([Request], IO a)
extractRequests io = 
  mapSt (mapFst flattenReqs) <| case io of
    IO.Pure x -> pure ([exit 0], IO.Pure x)
    IO.Impure iof -> case iof of
      IO.PutS s k -> mapSt (mapFst (\rs -> putS s :: rs)) <| extractRequests k
      IO.Exit n   -> pure ([exit n], io)
      IO.GetC k   ->
        ask >>= \st ->
        case String.uncons st.buffer of
          Nothing -> pure ([getS], io)
          Just (c, rest) ->
            put ({ buffer = rest }) >>= \_ ->
            extractRequests (k c)

flattenReqs : [Request] -> [Request]
flattenReqs rs =
  let loop rs acc n =
    if n >= 100
    then Trampoline.Continue (\_ -> loop rs acc 0)
    else 
      case rs of
        []  -> Trampoline.Done <| reverse acc
        [r] -> loop [] (r::acc) (n+1)
        r1 :: r2 :: rs' ->
        case (r1, r2) of
          (Exit n, _)      -> loop [] (r1::acc) (n+1)
          (Put s1, Put s2) -> loop (putS (s1++s2) :: rs') acc (n+1)
          _                -> loop (r2::rs') (r1::acc) (n+1)
    in Trampoline.trampoline <| loop rs [] 0
                         
-- | We send a batch job of requests, all requests until IO blocks
step : Response -> (IO a, IOState) -> ((IO a, IOState), [Request])
step resp (io, st) = 
  let newST = case resp of 
        Nothing -> st
        Just s  -> { st | buffer <- String.append st.buffer s }
      (newST', (rs, k)) = extractRequests io newST
  in ((k, newST'), rs)

-- | State monad
type State s a = s -> (s, a)

pure : a -> State s a
pure x = \s -> (s, x)

mapSt : (a -> b) -> State s a -> State s b
mapSt f sf = sf >>= (pure . f)

(>>=) : State s a -> (a -> State s b) -> State s b
f >>= k = \s -> let (s', y) = f s
                in k y s'

ask : State s s
ask = \s -> (s, s)

put : s -> State s ()
put s = \_ -> (s, ())

mapFst : (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x, y)