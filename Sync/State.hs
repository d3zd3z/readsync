-- Sync State stored across multiple sessions.

module Sync.State (
   -- HDBC exports
   Connection, withTransaction, disconnect,

   open,
   serverInfo,
   getFolders,
   updateValidity,
   getUIDMap,
   getUIDSet,
   setUIDMapping,
   setSeen,
   findReadElsewhere,
   getFirstUnread
) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Text.IMAPParsers (UID)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

open :: IO Connection
open = connectSqlite3 "rs-state.db"

serverInfo :: Connection -> IO (String, String, String)
serverInfo con = do
   result <- quickQuery con "select * from server" []
   case result of
      [[user, pass, host]] -> return (fromSql user, fromSql pass, fromSql host)
      (_:_:_) -> error "server table has multiple rows"
      _ -> error "Database doesn't have a proper server table."

getFolders :: Connection -> IO [(Int, String, UID)]
getFolders con = do
   result <- quickQuery con "select key, name, validity from folders" []
   return $ map (\ [a, b, c] -> (fromSql a, fromSql b, fromSql c)) result

updateValidity :: Connection -> String -> UID -> IO ()
updateValidity con name validity = do
   [[oldValidity, key]] <- quickQuery con "select validity, key from folders where name=?"
      [toSql name]
   1 <- run con "update folders set validity=? where name=?"
      [toSql validity, toSql name]
   _ <- run con "delete from idmap where folderKey=?" [key]
   return ()

getUIDMap :: Connection -> Int -> IO (Map UID String)
getUIDMap con folderKey = do
   vals1 <- quickQuery con "select uid, messageid from idmap where folderKey=?"
      [toSql folderKey]
   let vals = map (\ [a, b] -> (fromSql a, fromSql b)) vals1
   return $ Map.fromList vals

getUIDSet :: Connection -> Int -> IO (Set UID)
getUIDSet con folderKey = do
   vals1 <- quickQuery con "select uid from idmap where folderKey=?"
      [toSql folderKey]
   let vals = map (\ [a] -> fromSql a) vals1
   return $ Set.fromList vals

setUIDMapping :: Connection -> Int -> UID -> String -> Bool -> IO ()
setUIDMapping con folderKey uid mid seen = do
   1 <- run con "insert or replace into idmap (folderkey, uid, messageid, seen) values (?,?,?,?)"
      [toSql folderKey, toSql uid, toSql mid, boolToSql seen]
   return ()

-- Set the seen value for a row that is present.
setSeen :: Connection -> Int -> UID -> Bool -> IO ()
setSeen con folderKey uid seen = do
   1 <- run con "update idmap set seen=? where folderKey=? and uid=?"
      [boolToSql seen, toSql folderKey, toSql uid]
   return ()

boolToSql :: Bool -> SqlValue
boolToSql seen = toSql $ if seen then (1::Int) else 0

getFirstUnread :: Connection -> Int -> IO UID
getFirstUnread con folderKey = do
   result <- quickQuery con "select uid from idmap where folderKey=? and seen=0 \
         \ order by uid limit 1" [toSql folderKey]
   case result of
      [] -> do
         -- If there are no rows, then we should return '1' to start
         -- with.  Otherwise, we should just return one past the
         -- highest one we know about.
         r2 <- quickQuery con "select uid from idmap where folderKey=? \
               \ order by uid desc limit 1" [toSql folderKey]
         return $ case r2 of
            [] -> 1
            [[a]] -> 1 + fromSql a
            _ -> error "SQL returned multiple rows despite 'limit 1'"
      [[a]] -> return $ fromSql a
      _ -> error "SQL returned multiple rows despite 'limit 1'"

-- The meat.  Returns pairs of folder name and the UID of any messages
-- that have been marked as read in one folder but not in another.
findReadElsewhere :: Connection -> IO [(String, UID)]
findReadElsewhere con = do
   result <- quickQuery con "select folders.name, aa.uid\
               \ from idmap as aa \
               \ inner join idmap as bb \
               \   on aa.messageid == bb.messageid \
               \ inner join folders \
               \   on folders.key == aa.folderKey \
               \ where aa.folderKey != bb.folderKey \
               \   and aa.seen = 0 and bb.seen = 1 \
               \ order by aa.folderKey, aa.uid" []
   return $ map (\ [a, b] -> (fromSql a, fromSql b)) result
