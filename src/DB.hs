{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DB where

import Types

import Servant (Handler, err404)
import Control.Monad.Except (throwError, liftIO)
import Hasql.Session (Session, statement, run, QueryError)
import Hasql.Statement (Statement(..))
import qualified Hasql.Connection as Connection
import qualified Hasql.Encoders as E
import Hasql.Decoders
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras.Contrazip (contrazip2)
import GHC.Int (Int64)

noPrepare :: Bool
noPrepare = False

nodeDecoder :: Row Node
nodeDecoder = Node
  <$> column (nonNullable int8)
  <*> column (nonNullable text)
  <*> column (nonNullable (listArray (nonNullable int8)))

connect :: Session a -> IO (Either QueryError a)
connect session  = do
  Right connection <- Connection.acquire settings
  result <- run (session) connection
  return result
  where -- TODO config
    settings = Connection.settings "localhost" 5432 "postgres" "" "graphdb"

runSession :: Session b -> Handler b
runSession session = do
  res <- liftIO $ connect session
  case res of
    Left _ -> throwError err404
    Right r -> return r

getNodeByID :: Int64 -> Handler Node
getNodeByID nId = runSession $ statement nId s
  where
    s = Statement query encoder decoder noPrepare
    query = "SELECT * FROM nodes WHERE id=$1"
    encoder = (E.param $ E.nonNullable E.int8)
    decoder = singleRow nodeDecoder

getNodes :: Handler [Node]
getNodes = runSession $ statement () s
  where
    s = Statement query encoder decoder noPrepare
    query = "SELECT * FROM nodes"
    encoder = E.noParams
    decoder = rowList nodeDecoder

getRelatedNodes :: Int64 -> Handler [Node]
getRelatedNodes nId = runSession $ statement nId s
  where
    s = Statement query encoder decoder noPrepare
    query = "SELECT * FROM nodes WHERE $1 = ANY (relations)"
    encoder = (E.param $ E.nonNullable E.int8)
    decoder = rowList nodeDecoder

newNode ::  Label -> Handler NodeId
newNode value = runSession $ statement value s
  where
    s = Statement query encoder decoder noPrepare
    query = "INSERT INTO nodes (label, relations) VALUES ($1, '{}') RETURNING id;"
    encoder = label >$< (E.param $ E.nonNullable E.text)
    decoder = NodeId <$> singleRow (column (nonNullable int8))

deleteNode :: Int64 -> Handler Status
deleteNode nId = do
  _ <- liftIO $ removeConnectedFrom nId
  res <- liftIO $ connect $ statement nId s
  case res of
    Left _ -> return $ Err "Failed to delete"
    Right _ -> return $ Confirm "delete"
  where
    s = Statement query encoder decoder noPrepare
    query = "DELETE FROM nodes WHERE id = $1;"
    encoder = (E.param $ E.nonNullable E.int8)
    decoder = noResult

removeConnectedFrom :: Int64 -> IO (Either QueryError ())
removeConnectedFrom x = do
  connect $ statement x s
  where
    s = Statement query encoder decoder noPrepare
    query = "UPDATE nodes SET relations = relations - $1::int"
    encoder = E.param $ E.nonNullable E.int8
    decoder = noResult

renameNode :: Int64 -> Label -> Handler Status
renameNode nId newLabel = do
  res <- liftIO $ connect $ statement (nId, newLabel) s
  case res of
    Left _ -> return $ Err "Failed to rename. Possible reason: node does not exist"
    Right _ -> return $ Confirm "rename"
  where
    s = Statement query encoder decoder noPrepare
    query = "UPDATE nodes SET label = $2 WHERE id = $1;"
    encoder = contrazip2
      (E.param $ E.nonNullable E.int8)
      (label >$< (E.param $ E.nonNullable E.text))
    decoder = noResult

connectNodes :: Int64 -> Int64 -> Handler Status
connectNodes idFrom idTo = do
  if (idFrom == idTo)
     then (return $ Err "Circular links forbidden")
  else do
    res <- liftIO $ connect $ statement (idFrom, idTo) s
    case res of
      Left _ -> return $ Err "Failed to connect"
      Right _ -> return $ Confirm "connect"
    where
      s = Statement query encoder decoder noPrepare
      query =
        "UPDATE nodes SET relations = CASE \
           \ WHEN id = $1 THEN uniq((relations || $2::int)::int[]) \
           \ WHEN id = $2 THEN uniq((relations || $1::int)::int[]) \
        \ END \
        \ WHERE id IN ($1, $2)"
      encoder = contrazip2
        (E.param $ E.nonNullable E.int8)
        (E.param $ E.nonNullable E.int8)
      decoder = noResult
