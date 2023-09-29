module Engine where

import Card

import System.Time.Extra ( sleep )

userSortedDeck :: IO Deck
userSortedDeck = do
    putStrLn "Sort cards by suit and value, or just value? (sv/v)"
    c <- getLine
    case c of
        "sv" -> do
            putStrLn "Input the list as follows: suit-value,suit-value,...suit-value"
            
        "v" -> do
            putStrLn "Input the list as follows: value,value,...,value"
        _ -> do
            putStrLn "Invalid input"
            sleep 1
            userSortedDeck
        