module Main where

import Network.HaskellNet.IMAP
import Network.HaskellNet.BSStream
import qualified Text.Mime as M
import Text.IMAPParsers (UID)

import Control.Monad
import qualified Data.ByteString.Char8 as B8
import Data.List (intercalate, isInfixOf)
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
   S.disconnect db
   logout imap

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
   mids <- fetchMessageIDs imap missingIDs
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

fetchMessageIDs :: BSStream s => IMAPConnection s -> [UID] -> IO [String]
fetchMessageIDs imap uids = do
   groups <- forM (zip (chopList 100 uids) [1, 101..]) $ \(group, idx) -> do
      putStr $ show idx ++ "-" ++ show (idx + 99) ++ "\r"
      hFlush stdout
      let query = intercalate "," $ groupUIDs group
      answer <- fetchByStringT imap query "BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)]"
      forM (zip group answer) $ \ (uid, (altID, fields)) -> do
         when (uid /= altID) $ error "ID message in IMAP FETCH"
         let Just header = lookup "BODY[HEADER.FIELDS (MESSAGE-ID)]" fields
         let msg = M.message (B8.pack $ header ++ "empty\r\n")
         return $! getMessageId msg
   return $ concat groups

chopList :: Int -> [a] -> [[a]]
chopList n [] = []
chopList n l =
   let (pre, post) = splitAt n l in
   pre : chopList n post

-- The mime parser canonicalizes the case of the headers.
getMessageId :: M.Message -> String
getMessageId m = case lookup "Message-Id" $ getHeader m of
   Just x -> x
   Nothing -> error "Message doesn't contain a message id"

getHeader :: M.Message -> [M.Header]
getHeader (h, _) = h

-- Convert a sorted list of UID's into groups appropriate for
-- concatenating with ',' into queries for fetch.
groupUIDs :: [UID] -> [String]
groupUIDs [] = []
groupUIDs (a:as) = subGroup (a, a) as

subGroup :: (UID, UID) -> [UID] -> [String]
subGroup (l, h) aa@(a:as)
   | a == h+1 = subGroup (l, a) as
   | otherwise = groupDecode (l, h) : groupUIDs aa
subGroup grp [] = [groupDecode grp]

groupDecode :: (UID, UID) -> String
groupDecode (a, b)
   | a == b = show a
   | otherwise = show a ++ ":" ++ show b
