module Utils where

runMany :: [String] -> IO ()
runMany exs = mapM_ putStrLn exs

printLn :: (Show a) => a -> IO ()
printLn a = putStrLn $ show a

title :: String -> IO ()
title str = putStrLn $ "\n\ESC[31m" ++ str ++ "\ESC[0m"