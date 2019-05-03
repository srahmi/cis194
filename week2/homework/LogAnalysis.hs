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
build logs = foldr insert Leaf logs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ (msg : inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter (severe 50) . inOrder . build

message :: LogMessage -> String
message (Unknown _)          = []
message (LogMessage _ _ msg) = msg

severe :: Int -> LogMessage -> Bool
severe minLvl (Unknown _)            = False
severe minLvl (LogMessage mType _ _) = case mType of
                                            (Error lvl) -> lvl > minLvl
                                            otherwise   -> False


{--severe minLvl (LogMessage Info _ _)    = False
severe minLvl (LogMessage Warning _ _) = False
severe minLvl (Unknown _)              = False
severe minLvl (LogMessage (Error lvl) _ _)
    | lvl > minLvl = True
    | otherwise    = False--}
