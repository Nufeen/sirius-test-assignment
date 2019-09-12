{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import qualified DB as DB

import Servant
import Servant.API.Generic (genericApi, ToServantApi, (:-))
import Servant.Server.Generic (genericServer, AsServer)
import Network.Wai.Handler.Warp (run)
import Data.Swagger (Swagger, title, info)
import Servant.Swagger (toSwagger)
import Control.Lens ((.~), (&))
import GHC.Generics (Generic)
import GHC.Int (Int64)

data Routes route = Routes
  { _nodes :: route :- "nodes" :> Get '[JSON] [Node]
  , _node :: route :- "node" :> Capture "id" Int64 :> Get '[JSON] Node
  , _new ::  route :- "graph" :> "node" :> ReqBody '[JSON] Label :> Put '[JSON] NodeId
  , _delete :: route :- "graph" :> "node" :> Capture "id" Int64 :> Get '[JSON] Status
  , _neighbours :: route :- "node" :> Capture "id" Int64 :> "neighbours" :> Get '[JSON] [Node]
  , _rename :: route :- "graph" :> "node" :> Capture "id" Int64 :> "label" :> ReqBody '[JSON] Label :> Put '[JSON] Status
  , _connect :: route :- "graph" :> "link" :> Capture "idFrom" Int64 :> Capture "idTo" Int64 :> Get '[JSON] Status
  } deriving (Generic)

routes :: Routes AsServer
routes = Routes
  { _nodes = DB.getNodes
  , _node = DB.getNodeByID
  , _neighbours = DB.getRelatedNodes
  , _new = DB.newNode
  , _delete = DB.deleteNode
  , _rename = DB.renameNode
  , _connect = DB.connectNodes
  }

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type API = ToServantApi Routes :<|> SwaggerAPI

app :: Application
app = serve api (graphServer :<|> swaggerServer)
  where
    api = Proxy :: Proxy API
    graphServer = genericServer routes
    swaggerServer = return swaggerData
    graphApi = genericApi (Proxy :: Proxy Routes)
    swaggerData = toSwagger graphApi
      & info.title .~ "Graph DB API"

main :: IO ()
main = run 8081 app
