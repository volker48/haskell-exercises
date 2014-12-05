{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage line = split $ words $ line
    where split :: [String] -> LogMessage
          split (messageType:severityOrTimestamp:maybeTimestamp:rest)
              | messageType == "E" = LogMessage (Error (read $ severityOrTimestamp)) (read $ maybeTimestamp) (unwords $ rest)
              | messageType == "I" = LogMessage Info (read $ severityOrTimestamp) (unwords $ maybeTimestamp:rest)
              | messageType == "W" = LogMessage Warning (read $ severityOrTimestamp) (unwords $ maybeTimestamp:rest)
              | otherwise          = Unknown "Invalid MessageType"
          split _ = (Unknown "Invalid log line")


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s
