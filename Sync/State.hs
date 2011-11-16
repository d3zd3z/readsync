-- Sync State stored across multiple sessions.

module Sync.State (
   -- HDBC exports
   Connection, withTransaction, disconnect,

   open,
   serverInfo,
   getFolders,
   updateValidity,
   getUIDMap,
   setUIDMapping
) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Text.IMAPParsers (UID)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Map as Map
import Data.Map (Map)

open :: IO Connection
open = connectSqlite3 "rs-state.db"

serverInfo :: Connection -> IO (String, String, String)
serverInfo con = do
   result <- quickQuery con "select * from server" []
   case result of
      [[user, pass, host]] -> return (fromSql user, fromSql pass, fromSql host)
      (_:_:_) -> error "server table has multiple rows"
      _ -> error "Database doesn't have a proper server table."

getFolders :: Connection -> IO [(String, UID)]
getFolders con = do
   result <- quickQuery con "select * from folders" []
   return $ map (\ [a, b] -> (fromSql a, fromSql b)) result

updateValidity :: Connection -> String -> UID -> IO ()
updateValidity con name validity = do
   [[oldValidity]] <- quickQuery con "select validity from folders where name=?"
      [toSql name]
   1 <- run con "update folders set validity=? where name=?"
      [toSql validity, toSql name]
   _ <- run con "delete from idmap where validity=?" [oldValidity]
   return ()

getUIDMap :: Connection -> UID -> IO (Map UID String)
getUIDMap con validity = do
   vals1 <- quickQuery con "select uid, messageid from idmap where validity=?"
      [toSql validity]
   let vals = map (\ [a, b] -> (fromSql a, fromSql b)) vals1
   return $ Map.fromList vals

setUIDMapping :: Connection -> UID -> UID -> String -> IO ()
setUIDMapping con validity uid mid = do
   1 <- run con "insert or replace into idmap (validity, uid, messageid) values (?,?,?)"
      [toSql validity, toSql uid, toSql mid]
   return ()
