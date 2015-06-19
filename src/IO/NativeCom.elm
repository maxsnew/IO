module IO.NativeCom where

import Task exposing (Task)

import Native.NativeCom

type IRequest = Put String
              | Exit Int
              | Get
              | WriteFile { file : String, content : String }
              | Init

type alias IResponse = Maybe String

sendRequests : Signal (List IRequest) -> Signal (Task x ())
sendRequests requests =
    Signal.map sendRequestBatch requests

sendRequestBatch : List IRequest -> Task x ()
sendRequestBatch requests =
    Native.NativeCom.sendRequestBatch requests

responses : Signal IResponse
responses =
    Native.NativeCom.responses
