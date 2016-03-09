module Main where

import Hive.Server.App (startApp)

main :: IO ()
main = do
    putStrLn "starting the server on 31337..."
    startApp
