
-- IO boilerplate
port requests : Signal Request
port requests = Run.run responses console

port responses : Signal Response