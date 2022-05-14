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
import qualified Enums

data Employee = Employee
  { id        :: Int
  , name :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Employee)

$(deriveJSON defaultOptions ''Enums.Source)

data TodoState = Open | Discarded | Done deriving (Eq, Show)

$(deriveJSON defaultOptions ''TodoState)

data Todo = Todo
  { text :: String
  , state :: TodoState
  , note :: Maybe String
  , toBeImplementedBy :: Enums.Target
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Todo)

data Topic = Topic
  { question :: String
  , answer :: String
  , putOnAgendaBy :: Enums.Source
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Topic)

data OneOnOne = OneOnOne
    { employeeId :: Int
    , date :: Int
    , topics :: [Topic]
    } deriving (Eq, Show)
    
$(deriveJSON defaultOptions ''OneOnOne)

data TopicSuggestion = TopicSuggestion
    { suggestedQuestion :: String
    , lastAsked :: Maybe Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''TopicSuggestion)

type API = "employees" :> Get '[JSON] [Employee]
        :<|> "employee" :> Capture "employeeId" Int :> "oneOnOnes" :> Get '[JSON] [OneOnOne]
        :<|> "employee" :> Capture "employeeId" Int :> "todos" :> Get '[JSON] [Todo]
        :<|> "employee" :> Capture "employeeId" Int :> "topicSuggestions" :> Get '[JSON] [TopicSuggestion]

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
      :<|> topicSuggestions

employees :: [Employee]
employees = [ Employee 1 "Johannes Klepp"
        , Employee 2 "Nicole Banse"
        , Employee 3 "Peter Hart"
        , Employee 4 "Marta Pörs"
        ]

oneOnOnes :: Int -> Handler [OneOnOne]
oneOnOnes 1 = return [OneOnOne 1 1647039600000 [Topic "Was macht dich zufrieden?" "Nix" Enums.Manager, Topic "Welche Fähigkeiten würdest du gerne lernen / verbessern?" "Vieles" Enums.Manager], OneOnOne 1 1645225200000 []]
oneOnOnes 3 = return [OneOnOne 1 1645052400000 [Topic "Welche Fähigkeiten kann ich verbessern?" "Nix" Enums.Employee, Topic "Was macht dich zufrieden?" "Vieles" Enums.Manager]]
oneOnOnes _ = return []

todos :: Int -> Handler [Todo]
todos 1 = return [Todo "Mit Tessa über kommende Projekte sprechen" Open Nothing Enums.Manager,
                  Todo "Peter nach Codequalität fragen" Done Nothing Enums.Manager,
                  Todo "Buchempfehlung als Link schicken" Discarded (Just "Nicht mehr relevant") Enums.Employee]
todos 2 = return [Todo "Peter nach Codequalität fragen" Open Nothing Enums.Manager]
todos _ = return []

topicSuggestions :: Int -> Handler [TopicSuggestion]
topicSuggestions 1 = return [TopicSuggestion "Was sind die größten Herausforderungen in deinem Job?" (Just 1645225200000)]
topicSuggestions 3 = return [TopicSuggestion "Welche Fähigkeiten würdest du gerne lernen / verbessern?" Nothing, TopicSuggestion "Was sind die größten Herausforderungen in deinem Job?" (Just 1645225200000)]
topicSuggestions _ = return []