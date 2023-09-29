module CardGame.Game (
    GameState (..)
    , Game (..)
    , dealCards
    , createEmptyGame
    , gameActions
) where


import CardGame.Player ( deal, Player )
import CardGame.Card ( Card )

import Data.CircularList ( CList, empty )
import Feature (Feature)
import CardGame.PlayerMove (Move)

data GameState = Start | TurnStart | TurnEnd | RoundStart | RoundEnd | End
    deriving (Show, Eq)

data Game = Game {
    gameName :: String
    , playerMoves :: [(Move, Bool)]
    , cardGen :: [Card]
    , discard :: [Card]
    , turnOrder :: [Game]
    , cardSuits :: [Game]
    , cardRanks :: [Game]
    , cardEffects :: [((Feature, Maybe [Game]), [Game])]
    , deck :: [Card]
    , pile :: [(Card, Maybe Card)]
    , players :: CList Player
    , endCon :: [Game -> Bool]
    , winCon :: [Game -> [Player]]
    , state :: GameState
    , rules :: [(Feature, [Game])]
    , actions :: [(GameState, [(Game -> IO Game, Bool)])]
    , canPlaceCard :: [Game -> Card -> Bool]
}



dealCards :: Game -> Int -> Game
dealCards game n = do
    let (plrs', deck') = deal n (deck game) (players game)
    game { players = plrs', deck = deck'}


createEmptyGame :: Game
createEmptyGame = Game {
                    gameName = "Default Game"
                    , turnOrder = []
                    , discard = []
                    , playerMoves = []
                    , cardGen = []
                    , cardSuits = []
                    , cardRanks = [] 
                    , cardEffects = []
                    , deck = []
                    , pile = []
                    , players = empty
                    , endCon = []
                    , winCon = []
                    , state = Start
                    , rules = []
                    , actions = []
                    , canPlaceCard = []
                    }

-- Applies the game actions to the game
gameActions :: [[Game -> IO Game]] -> Game -> IO Game
gameActions xs = go (concat xs)
    where
        go :: [Game -> IO Game] -> Game -> IO Game
        go [] gm = return gm
        go (f:fs) gm = f gm >>= go fs
