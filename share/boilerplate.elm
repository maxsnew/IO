
-- IO boilerplate
port requests : Signal Json.Value
port requests = Run.run responses console

port responses : Signal (Maybe String)