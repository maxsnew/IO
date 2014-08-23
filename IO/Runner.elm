module IO.Runner where

import Dict
import Json as JSON
import String
import Trampoline
    
import IO.IO as IO
import IO.IO (IO)

-- Internal Request representation
data IRequest
    = Put String
    | Exit Int
    | Get
    | WriteFile { file : String, content : String }

type IResponse = Maybe String


-- User-facing Request representation
type Request = JSON.Value
type Response = JSON.Value
type IOState  = { buffer : String }

start : IOState 
start = { buffer = "" }

run : Signal Response -> IO () -> Signal Request
run responses io = 
    let init = (\_ -> io, start, [])

        update response (io, st, _) =
            step (deserialize response) io st

        toJson (_, _, requests) =
            serialize requests
    in
        toJson <~ foldp update init responses

deserialize : Response -> IResponse
deserialize resp = 
    case resp of
        JSON.Object d -> 
            case Dict.get "Just" d of
                Just (JSON.String s) -> Just s
                _ -> Nothing

        _ -> Nothing

serialize : [IRequest] -> JSON.Value
serialize requests =
    let mkObj pairs =
            JSON.Object (Dict.fromList pairs)

        toJson request = 
            case request of
                Put string ->
                    mkObj [ ("ctor", JSON.String "Put")
                          , ("val",  JSON.String string)
                          ]
                Get ->
                    mkObj [ ("ctor", JSON.String "Get") ]

                Exit code ->
                    mkObj [ ("ctor", JSON.String "Exit")
                          , ("val", JSON.Number (toFloat code) )
                          ]

                WriteFile { file, content } ->
                    mkObj [ ("ctor", JSON.String "WriteFile")
                          , ("file", JSON.String file)
                          , ("content", JSON.String content)
                          ]
      in
          JSON.Array (map toJson requests)

putS : String -> IRequest
putS = Put

exit : Int -> IRequest
exit = Exit

getS : IRequest
getS = Get

writeF : { file : String, content : String } -> IRequest
writeF = WriteFile

-- | Extract all of the requests that can be run now
extractRequests : IO a -> State IOState ([IRequest], () -> IO a)
extractRequests io = 
    case io of
        IO.Pure x ->
            pure ([exit 0], \_ -> IO.Pure x)

        IO.Impure iof ->
            case iof of
                IO.PutS s k ->
                    mapSt (mapFst (\rs -> putS s :: rs)) <| pure ([], k)

                IO.WriteF obj k ->
                    mapSt (mapFst (\rs -> writeF obj :: rs)) <| pure ([], k)

                IO.Exit code ->
                    pure ([exit code], \_ -> io)

                IO.GetC k ->
                    ask >>= \st ->
                    case String.uncons st.buffer of
                      Nothing -> pure ([getS], \_ -> io)
                      Just (c, rest) ->
                          put { buffer = rest } >>= \_ ->
                          extractRequests (k c)

flattenReqs : [IRequest] -> [IRequest]
flattenReqs requests =
    let loop rs acc n =
            if  | n >= 100 ->
                    Trampoline.Continue (\_ -> loop rs acc 0)
                | otherwise ->
                    case rs of
                      []  -> Trampoline.Done <| reverse acc
                      [r] -> loop [] (r::acc) (n+1)
                      r1 :: r2 :: rs' ->
                          case (r1, r2) of
                              (Exit n, _) ->
                                  loop [] (r1::acc) (n+1)

                              (Put s1, Put s2) ->
                                  loop (putS (s1++s2) :: rs') acc (n+1)

                              _ ->
                                  loop (r2::rs') (r1::acc) (n+1)
    in
        Trampoline.trampoline <| loop requests [] 0
                         
-- | We send a batch job of requests, all requests until IO blocks
step : IResponse -> (() -> IO a) -> IOState -> (() -> IO a, IOState, [IRequest])
step resp io st =
    let newST =
            case resp of 
                Nothing -> st
                Just s ->
                    { st | buffer <- String.append st.buffer s }

        (newST', (rs, k)) = extractRequests (io ()) newST
    in
        (k, newST', rs)

-- | State monad
type State s a = s -> (s, a)

pure : a -> State s a
pure x = \s -> (s, x)

mapSt : (a -> b) -> State s a -> State s b
mapSt f sf =
    sf >>= (pure << f)

(>>=) : State s a -> (a -> State s b) -> State s b
f >>= k = \s -> let (s', y) = f s
                in k y s'

ask : State s s
ask = \s -> (s, s)

put : s -> State s ()
put s = \_ -> (s, ())

mapFst : (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x, y)
