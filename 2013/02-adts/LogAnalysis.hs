{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- convert a string to a logmessage
-- string -> [words]
-- [words] -> MessageType

-- was confused so i stole from
-- https://github.com/BerndSchwarzenbacher/cis194-solutions/blob/master/02-adt/LogAnalysis.hs
parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
                       case wordList of
                         ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
                         ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
                         ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
                         _ -> Unknown (unwords wordList)
