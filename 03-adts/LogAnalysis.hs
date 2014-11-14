{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1 The first step is figuring out how to parse an individual
-- message. Define a function
--   parseMessage :: String -> LogMessage
-- which parses an individual line from the log file.

getMessageType :: Char -> MessageType
getMessageType 'I' = Info
getMessageType 'W' = Info
getMessageType ('E' x) = Error x

{-parseMessage :: String -> LogMessage-}
