module Main where

import Lib

port :: Int
port = 8080

main :: IO ()
main = do
    putStrLn $ "Listen on " ++ show port
    startApp 8080
