{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Swagger
import Servant.API.Generic
import Data.Text (Text)
import GHC.Int (Int64)

data Node = Node
  { nodeId :: Int64
  , nodeLabel :: Text
  , relations :: [Int64]
  } deriving (Eq, Show, Generic)

instance ToJSON Node where
  toJSON (Node _id _label _) =
    object [ "id" .= _id
           , "label" .= _label
           ]

instance ToSchema Node

data Label =  Label {
  label :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Label
instance ToSchema Label

data NodeId = NodeId {
  _id :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON NodeId where
  toJSON (NodeId _id) = object [ "id" .= _id ]

instance ToSchema NodeId

data Status = Status {
  success :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Status
instance ToSchema Status
