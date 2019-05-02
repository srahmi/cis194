{-# OPTIONS_GHC -Wall -v#-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":lvl:ts:msg) -> LogMessage (Error (read lvl :: Int)) (read ts :: Int) (unwords msg)
  ("I":ts:msg)     -> LogMessage Info (read ts :: Int) (unwords msg)
  ("W":ts:msg)     -> LogMessage Warning (read ts :: Int) (unwords msg)
  _                -> Unknown s

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert newmsg Leaf = Node Leaf newmsg Leaf
insert newmsg (Node left oldmsg right)
      | time newmsg > time oldmsg = Node left oldmsg (insert newmsg right)
      | otherwise                 = Node (insert newmsg left) oldmsg right
insert (Unknown _) tree = tree

time :: LogMessage -> TimeStamp
time (LogMessage _ ts _)         = ts
time (LogMessage (Error _) ts _) = ts

build :: [LogMessage] -> MessageTree
build []   = Leaf
build logs = map (\l -> insert l Leaf) logs
{--timestamp :: LogMessage -> Maybe TimeStamp
timestamp (Unknown _)        = Nothing
timestamp (LogMessage t _ _) = case t of
  (Error _) -> Just time
  Info      -> Just time
  Warning   -> Just time--}
