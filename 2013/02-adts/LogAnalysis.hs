{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

getMessageType :: [String] -> MessageType
getMessageType ("I" x _) = Info (read x)
getMessageType ("W" x _) = Warning (read x)
getMessageType ("E" x y) = (Error (read x)) (read y)

parseMessage :: String -> LogMessage
parseMessage s = LogMessage (getMessageType (take 3 s))
