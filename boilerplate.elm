
-- IO boilerplate
port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses console

port responses : Signal (Maybe String)