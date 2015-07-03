{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = 
    case words s
      of "I" : timeStr : msg -> LogMessage Info    (read timeStr) (unwords msg)
         "W" : timeStr : msg -> LogMessage Warning (read timeStr) (unwords msg)
         "E" : levelStr : timeStr : msg
             -> LogMessage (Error (read levelStr)) (read timeStr) (unwords msg)
         _   -> Unknown s

parse :: String -> [LogMessage]
parse f = map parseMessage (lines f)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ time' _) (Node l n@(LogMessage _ time _) r)
    | time' < time = Node (insert lm l) n r
    | otherwise    = Node l n (insert lm r)
insert _ mt@(Node _ (Unknown _) _) = mt        -- this can't happen

build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf  = []
inOrder (Node l n r) = inOrder l ++ n : inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map getMsg (filter isSevereError (inOrder (build lms)))

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error n) _ _) = n > 50
isSevereError _                        = False

getMsg :: LogMessage -> String
getMsg (Unknown _) = "Unknown msg"
getMsg (LogMessage _ _ m) = m
