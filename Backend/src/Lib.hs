{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant

data Employee = Employee
  { id        :: Int
  , name :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Employee)

data TodoState = Open | Discarded | Done deriving (Eq, Show)

$(deriveJSON defaultOptions ''TodoState)

data Todo = Todo
  { text :: String
  , state :: TodoState
  , note :: Maybe String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Todo)

data Topic = Topic
  { question :: String
  , answer :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Topic)

data OneOnOne = OneOnOne
    { employeeId :: Int
    , date :: Int
    , topics :: [Topic]
    } deriving (Eq, Show)
    
$(deriveJSON defaultOptions ''OneOnOne)

type API = "employees" :> Get '[JSON] [Employee]
        :<|> "employee" :> Capture "employeeId" Int :> "oneOnOnes" :> Get '[JSON] [OneOnOne]
        :<|> "employee" :> Capture "employeeId" Int :> "todos" :> Get '[JSON] [Todo]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = simpleCors (serve api server)

api :: Proxy API
api = Proxy

server :: Server API
server = return employees
      :<|> oneOnOnes
      :<|> todos

employees :: [Employee]
employees = [ Employee 1 "Johannes Klepp"
        , Employee 2 "Nicole Banse"
        , Employee 3 "Peter Hart"
        , Employee 4 "Marta Pörs"
        ]

oneOnOnes :: Int -> Handler [OneOnOne]
oneOnOnes 1 = return [OneOnOne 1 1647039600000 [Topic "Was macht dich zufrieden?" "Nix", Topic "Welche Fähigkeiten würdest du gerne lernen / verbessern?" "Vieles"], OneOnOne 1 1645225200000 []]
oneOnOnes 3 = return [OneOnOne 1 1645052400000 [Topic "Welche Fähigkeiten würdest du gerne lernen / verbessern?" "Nix", Topic "Was macht dich zufrieden?" "Vieles"]]
oneOnOnes _ = return []

todos :: Int -> Handler [Todo]
todos 1 = return [Todo "Mit Tessa über kommende Projekte sprechen" Open Nothing, Todo "Peter nach Codequalität fragen" Done Nothing , Todo "Buchempfehlung als Link schicken" Discarded (Just "Nicht mehr relevant")]
todos 2 = return [Todo "Peter nach Codequalität fragen" Open Nothing]
todos _ = return []