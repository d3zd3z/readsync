module Main where

import Network.HaskellNet.IMAP
import Network.HaskellNet.BSStream
import qualified Text.Mime as M
import Text.IMAPParsers (UID)

import Control.Monad
import qualified Data.ByteString.Char8 as B8
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

import qualified Sync.State as S

-- TODO: Purge message IDs that are no longer present.

main :: IO ()
main = do
   db <- S.open
   (user, pass, imapServer) <- S.serverInfo db
   imap <- connectIMAP imapServer
   login imap user pass
   folders <- S.getFolders db

   S.withTransaction db $ \db -> do
      putStrLn $ "Pass 1: Scanning " ++ (show $ length folders) ++ " folders"
      forM_ folders $ scanFolder db imap

-- Note that the parsing lib is incorrect, and won't properly parse
-- fetch results that contain a space.  This means that we have to
-- fetch all of the headers, even if we only want one.
-- TODO: Fix the parser in HaskellNet.
-- To compensate, concatenate an empty message body, and use the
-- partial mime parser.
--
-- The mime library needs a way to just parse headers, not entire mime
-- messages.

scanFolder :: BSStream s => S.Connection -> IMAPConnection s -> (String, UID) -> IO ()
scanFolder db imap (name, startUID) = do
   putStrLn $ "  " ++ name
   select imap name
   validity <- uidValidity imap
   when (validity /= startUID) $ do
      S.updateValidity db name validity

   -- Get all of the flags.
   seens <- fetchSeens imap
   -- forM_ seens $ \seen ->
   --    putStrLn $ show seen

   uidMap <- S.getUIDMap db validity
   let missingIDs = filter (\k -> Map.notMember k uidMap) $ map fst seens
   putStrLn $ "    Updating " ++ (show $ length missingIDs) ++ " message ids"
   mids <- zipWithM (askMessageID imap) [1..] missingIDs
   mapM_ (uncurry $ S.setUIDMapping db validity) $ zip missingIDs mids

-- Fetch all of the messages and seen flags.
fetchSeens :: BSStream s => IMAPConnection s -> IO [(UID, Bool)]
fetchSeens imap = do
   lastUid <- uidNext imap
   flags <- fetchByStringR imap (1, lastUid-1) "(UID FLAGS)"
   return $ map (\ (a, b) -> (a, decodeSeen b)) flags
   where
      decodeSeen :: [(String, String)] -> Bool
      decodeSeen = ("\\Seen" `isInfixOf`) . fromJust . lookup "FLAGS"

-- This is very slow, because it does a round-trip to the server for
-- each unknown id.  It's probably usable in normal situations when
-- run frequently, but will be very painful on the first run.
askMessageID :: BSStream s => IMAPConnection s -> Int -> UID -> IO String
askMessageID imap index uid = do
   putStr $ show index ++ "\r"
   hFlush stdout
   fields <- fetchByString imap uid "BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)]"
   let Just header = lookup "BODY[HEADER.FIELDS (MESSAGE-ID)]" fields
   let msg = M.message (B8.pack $ header ++ "empty\r\n")
   return $! getMessageId msg

scanBox :: BSStream s => IMAPConnection s -> String -> IO ()
scanBox con box = do
   select con box
   validity <- uidValidity con
   lastUid <- uidNext con
   putStrLn $ show validity
   putStrLn $ show lastUid
   -- info <- fetchByStringR con (1, lastUid-1) "(BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)])"
   -- info <- fetchByStringR con (1, 2) "(BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)])"
   -- info <- fetchByStringR con (1, 2) "(UID BODY.PEEK[HEADER.FIELDS (DATE)])"
   info <- fetchByStringR con (1, lastUid-1) "(BODY.PEEK[HEADER])"
   forM info $ \(uid, fields) -> do
      let Just header = lookup "BODY[HEADER]" fields
      let msg = M.message (B8.pack $ header ++ "empty\r\n")
      let msgId = getMessageId msg
      putStrLn $ show uid ++ ": " ++ show msgId
   -- item <- fetchHeaderFields con 4295 ["(MESSAGE-ID)"]
   -- putStrLn $ show item
   close con

-- The mime parser canonicalizes the case of the headers.
getMessageId :: M.Message -> String
getMessageId m = case lookup "Message-Id" $ getHeader m of
   Just x -> x
   Nothing -> error "Message doesn't contain a message id"

getHeader :: M.Message -> [M.Header]
getHeader (h, _) = h
