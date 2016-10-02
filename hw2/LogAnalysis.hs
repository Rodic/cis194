{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg =
  case words msg of
    "E" : severity  : timestamp : message ->
      LogMessage (Error (read severity :: Int)) (read timestamp :: Int) (unwords message)
    "I" : timestamp : message ->
      LogMessage Info (read timestamp :: Int) (unwords message)
    "W" : timestamp : message ->
      LogMessage Warning (read timestamp :: Int) (unwords message)
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse fileContent = map parseMessage (lines fileContent)

insert :: LogMessage -> MessageTree -> MessageTree
insert _ (Node _ (Unknown _) _) = error "Invalid tree given"
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ timestamp _) tree@(Node leftTree node@(LogMessage _ timestamp' _) rightTree)
  | timestamp' > timestamp = Node (insert logMsg leftTree) node rightTree
  | timestamp' < timestamp = Node leftTree node (insert logMsg rightTree)
  | True                   = Node tree logMsg Leaf

build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree node rightTree) = (inOrder leftTree) ++ ( node : inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map logMsg (filter withHighSeverity (sorted msgs))
                     where
                       withHighSeverity (LogMessage (Error s) _ _) = s >= 50
                       withHighSeverity _ = False
                       sorted = inOrder . build
                       logMsg (LogMessage _ _ msg) = msg
                       logMsg (Unknown msg) = msg



-- $ testWhatWentWrong parse whatWentWrong "error.log"

-- [
--   "M ustardwatch opened, please close for proper functioning!",
--   "A ll backup mustardwatches are busy",
--   "D epletion of mustard stores detected!",

--   "H ard drive failure: insufficient mustard",
--   "A ll backup mustardwatches are busy",
--   "T wenty seconds remaining until out-of-mustard condition",
--   "T en seconds remaining until out-of-mustard condition",
--   "E mpty mustard reservoir! Attempting to recover...",
--   "R ecovery failed! Initiating shutdown sequence"
--  ]
