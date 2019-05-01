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

{--insert :: LogMessage -> MessageTree -> MessageTree
insert message (Node l m r)
      | (LogMessage _ ts _) > (getTimeStamp m) = Leaf--}

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage _ ts _) = ts
