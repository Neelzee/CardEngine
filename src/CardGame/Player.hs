module CardGame.Player (
    Player (..)
    , createPlayers
    , deal
    , resetMoves
    , getMoveFromString
    , standardMoves
    , parsePlayerMovesExpr
    , parsePlayerMove
    , parsePlayerMoves
    , prettyPrintMoves
) where

import CardGame.Card ( Card )
import Data.CircularList (CList, focus, size, rotR, update)
import Data.Maybe (mapMaybe)
import Functions (splitAndTrim)
import CardGame.PlayerMove (Move (PlayCard, DrawCard, Pass, DiscardCard), prettyShow)
import Data.List (intercalate)
import Control.Monad (replicateM)

data Player = Player {
    name :: String
    , hand :: [Card]
    , moves :: [(Move, Bool)]
    , movesHistory :: [(Move, Bool)]
    , pScore :: Int
}
    deriving (Show, Eq)

instance Ord Player where
    p1 `compare` p2 = pScore p1 `compare` pScore p2

-- Creates players
createPlayers :: Int -> IO [Player]
createPlayers 0 = return []
createPlayers n = replicateM n createPlayer

createPlayer :: IO Player
createPlayer = do
    putStrLn "Enter name:"
    n <- getLine
    return $ Player n [] [] [] 0

resetMoves :: Player -> [(Move, Bool)] -> Player
resetMoves plr mv = plr { moves = mv }

deal :: Int -> [Card] -> CList Player -> (CList Player, [Card])
deal n deck players = deal' (n * size players) deck players

deal' :: Int -> [Card] -> CList Player -> (CList Player, [Card])
deal' 0 dck plrs = (plrs, dck)
deal' _ [] plrs = (plrs, [])
deal' n dck plrs = (giveCards plrs (take n dck), drop n dck) 


giveCards :: CList Player -> [Card] -> CList Player
giveCards plrs [] = plrs
giveCards plrs (x:xs) = case focus plrs of
    Just p -> giveCards (rotR (update (p { hand = x : hand p }) plrs)) xs
    Nothing -> giveCards (rotR plrs) (x:xs)



-- Gets move from string
getMoveFromString :: String -> Maybe Move
getMoveFromString "play" = Just PlayCard
getMoveFromString "draw" = Just DrawCard
getMoveFromString "pass" = Just Pass
getMoveFromString _ = Nothing


standardMoves :: [(Move, Bool)]
standardMoves = [
    (PlayCard, False),
    (DrawCard, True),
    (DrawCard, True),
    (DrawCard, True),
    (Pass, False)]


parsePlayerMovesExpr [] = []
parsePlayerMovesExpr (_:xs) = parsePlayerMovesExpr xs



parsePlayerMoves :: String -> [(Move, Bool)]
parsePlayerMoves s = mapMaybe parsePlayerMove (splitAndTrim s)
  

parsePlayerMove :: String -> Maybe (Move, Bool)
parsePlayerMove x = case words x of
    ["PLAY_CARD", b] -> Just (PlayCard, b == "TRUE")
    ["DRAW_CARD", b] -> Just (DrawCard, b == "TRUE")
    ["PASS", b] -> Just (Pass, b == "TRUE")
    ["DISCARD_CARD", b] -> Just (DiscardCard, b == "TRUE")
    _ -> Nothing


prettyPrintMoves :: [(Move, Bool)] -> IO ()
prettyPrintMoves mv = putStrLn (intercalate ", " (map (\(a, _) -> prettyShow a) mv))

