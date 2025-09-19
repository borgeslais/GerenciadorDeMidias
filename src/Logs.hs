module Logs where

import Data.Time

logMessage :: String -> IO()
logMessage msg = do
    tempo <- getCurrentTime
    let entradaLog = show tempo ++ "," ++ msg ++ "\n"
    appendFile "appLog.csv" entradaLog

