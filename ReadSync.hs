module Main where

import qualified Network.HaskellNet.IMAP as I
import Network.HaskellNet.IMAP (IMAPConnection)
import Network.HaskellNet.BSStream
import qualified Text.Mime as M
import Text.IMAPParsers (UID)

import Control.Arrow (second)
import Control.Monad
import qualified Data.ByteString.Char8 as B8
import Data.List (groupBy, intercalate, isInfixOf, partition)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import System.IO (hFlush, stdout)

import qualified Sync.State as S

-- TODO: Purge message IDs that are no longer present.

main :: IO ()
main = do
   db <- S.open
   (user, pass, imapServer) <- S.serverInfo db
   imap <- I.connectIMAP imapServer
   I.login imap user pass
   folders <- S.getFolders db

   S.withTransaction db $ \_ -> do
      putStrLn $ "Pass 1: Scanning " ++ show (length folders) ++ " folders"
      forM_ folders $ scanFolder db imap
      updates <- S.findReadElsewhere db
      putStrLn $ "Pass 2: Marking " ++ show (length updates) ++ " messages as read"
      markRead imap updates

   S.disconnect db
   I.logout imap

markRead :: BSStream s => IMAPConnection s -> [(String, UID)] -> IO ()
markRead imap updates =
   forM_ (groupBy (\ (a, _) (b, _) -> a == b) updates) $ \upd2 -> do
      let folder = fst $ head upd2
      let ids = map snd upd2
      putStrLn $ "Updating " ++ folder ++ " (" ++ show (length ids) ++ " messages)"
      I.select imap folder
      forM_ ids $ \i ->
         I.store imap i (I.PlusFlags [I.Seen])
      I.close imap

-- The mime library needs a way to just parse headers, not entire mime
-- messages.

scanFolder :: BSStream s => S.Connection -> IMAPConnection s -> (Int, String, UID) -> IO ()
scanFolder db imap (folderKey, name, startUID) = do
   putStrLn $ "  " ++ name
   I.select imap name
   validity <- I.uidValidity imap
   when (validity /= startUID) $
      S.updateValidity db name validity

   -- Get all of the flags.
   start <- S.getFirstUnread db folderKey
   seens <- fetchSeens imap start

   uidSet <- S.getUIDSet db folderKey
   let (present, missing) = partition (\ (k, _) -> Set.member k uidSet) seens
   let missingIDs = map fst missing
   let missingSeens = map snd missing
   when (missingIDs /= []) $
      putStrLn $ "    Updating " ++ show (length missingIDs) ++ " message ids"
   mids <- fetchMessageIDs imap missingIDs

   -- Write out the new UIDs that we haven't seen before.
   mapM_ (\ (u, m, s) -> S.setUIDMapping db folderKey u m s) $ zip3 missingIDs mids missingSeens

   -- Update the seen state of the messages we have seen.
   mapM_ (uncurry (S.setSeen db folderKey)) present

   I.close imap

-- Fetch all of the messages and seen flags.
fetchSeens :: BSStream s => IMAPConnection s -> UID -> IO [(UID, Bool)]
fetchSeens imap firstUnread = do
   lastUid <- I.uidNext imap

   flagGroups <- forM (chopList 1000 [firstUnread .. lastUid-1]) $ \group -> do
      let query = intercalate "," $ groupUIDs group
      flags <- I.fetchByStringT imap query "(UID FLAGS)"
      return $ map (second decodeSeen) flags
   return $ concat flagGroups

   where
      decodeSeen :: [(String, String)] -> Bool
      -- I suspect this isInfixOf is where much of the time is being
      -- spent.  It might be better to use ByteStrings instead of
      -- Strings for the results, since they can be scanned faster.
      decodeSeen = ("\\Seen" `isInfixOf`) . fromJust . lookup "FLAGS"

fetchMessageIDs :: BSStream s => IMAPConnection s -> [UID] -> IO [String]
fetchMessageIDs imap uids = do
   groups <- forM (zip (chopList 100 uids) [1 :: Int, 101..]) $ \(group, idx) -> do
      putStr $ show idx ++ "-" ++ show (idx + 99) ++ "\r"
      hFlush stdout
      let query = intercalate "," $ groupUIDs group
      answer <- I.fetchByStringT imap query "BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)]"
      forM (zip group answer) $ \ (uid, (altID, fields)) -> do
         when (uid /= altID) $ error "ID message in IMAP FETCH"
         let Just header = lookup "BODY[HEADER.FIELDS (MESSAGE-ID)]" fields
         let msg = M.message (B8.pack $ header ++ "empty\r\n")
         return $! getMessageId msg
   return $ concat groups

chopList :: Int -> [a] -> [[a]]
chopList _ [] = []
chopList n l =
   let (pre, post) = splitAt n l in
   pre : chopList n post

-- The mime parser canonicalizes the case of the headers.
getMessageId :: M.Message -> String
getMessageId m =
   fromMaybe (error "Message doesn't contain a message id")
      (lookup "Message-Id" $ getHeader m)

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
