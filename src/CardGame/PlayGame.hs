module CardGame.PlayGame (
    gameStart
    ) where

import Data.CircularList
    ( focus, fromList, update, removeR )
import System.Time.Extra ( sleep )

import CardGame.Player ( createPlayers, Player (name, moves, hand, pScore, movesHistory), prettyPrintMoves )
import Text.Read (readMaybe)
import Data.List (find, intercalate)
import Feature ( Feature(..) )
import Functions (lookupAll, removeFirst, count, dropFilteredCount, unique, removeLookupAll, remLst, elemLst)
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass, DiscardCard))
import System.IO ( hFlush, stdout )
import CardGame.PlayCommands (validatePLCommand, PLCommand (plc), UserActions (..), printUACommands)
import CardGame.Game (GameState (Start, TurnEnd, TurnStart), dealCards, gameActions, createEmptyGame, Game (..))
import CardGame.Card (Card)
import Data.Maybe (mapMaybe)

gameLoop :: Game -> IO Game
gameLoop g = do
    --clearScreen

    -- Incase every player leaves the game
    if null (players g)
        then
            do
                putStrLn "No players left."
                return g
        else
            if any ($ g) (endCon g)
                then
                    do
                        putStrLn "game over"
                        return g
                else
                    do
                        g' <- doPlayerTurn g
                        -- new player turn, and changes to the new players turn
                        game <- execCDSLGame (turnOrder g') (g' { state = TurnEnd})

                        -- Actions that should happen on this state
                        let acts = map (map fst) (lookupAll (state game) (actions game))
                        -- Applies the actions, on the same game, and returns the updated game
                        game' <- gameActions acts game
                        -- Filters the actions on the snd parameter in the tuple, which would be a boolean
                        -- , on if this actions should be removed or not
                        -- Only used for CEEffects, that do not occur on the player who placed the card
                        let acts' = map (filter snd) (lookupAll (state game) (actions game))
                        gameLoop (game' { actions = removeLookupAll TurnEnd acts' (actions game) })


playerPlayCard :: Int -> Player -> Game -> IO Game
playerPlayCard cardIndex plr game = case lookup PlayCard (moves plr) of
                        Just b -> if cardIndex `elem` [1..(length (hand plr))]
                            then
                                do
                                    let card = hand plr !! (cardIndex - 1)

                                    -- checks if card can be placed
                                    if foldr ((&&) . (\f -> f game card)) True (canPlaceCard game)
                                        then
                                            do
                                                putStrLn ("Plays " ++ show card)
                                                let plr' = plr { hand = removeFirst (hand plr) card
                                                    , moves = removeFirst (moves plr) (PlayCard, b)
                                                    , movesHistory = (PlayCard, b) : movesHistory plr }
                                                -- Check card effect
                                                game' <- checkCardEffect card (game { pile = (card, Nothing):pile game })
                                                -- If player can play card again, 
                                                if b
                                                    then
                                                        doPlayerTurn game' { players = update plr' (players game') }
                                                    else
                                                        do
                                                            putStrLn "Hit Enter to go to next turn."
                                                            _ <- getLine
                                                            return game' { state = TurnEnd, players = update plr' (players game')}
                                        else
                                            do
                                                putStrLn ("Cannot place " ++ show card)
                                                sleep 1
                                                doPlayerTurn game
                            else
                                do
                                    putStrLn ("Invalid input, expected number in the range " ++ show [1..(length (hand plr))])
                                    doPlayerTurn game
                        _ -> do
                            putStrLn "Cannot Play a card this turn, type 'moves', to show available moves."
                            doPlayerTurn game


playerDrawCard :: Int -> Player -> Game -> IO Game
playerDrawCard c plr game = do
    let bls = lookupAll DrawCard (moves plr)
    let tC = count True bls
    let fC = length bls - tC
    case (tC >= c, fC == 1 && (tC + 1) >= c) of
        -- Can do action again
        (True, _) -> do
            let crds = take c (deck game)
            putStrLn ("Drew: " ++ intercalate ", " (map show crds))
            let p' = plr {hand = crds ++ hand plr, moves = dropFilteredCount (== (DrawCard, True)) c (moves plr), movesHistory = replicate c (PlayCard, True) ++ movesHistory plr }
            doPlayerTurn (game { deck = drop c (deck game), players = update p' (players game) })
        -- Cannot do action again
        (False, True) -> do
            let crds = take c (deck game)
            putStrLn ("Drew: " ++ intercalate ", " (map show crds))
            let p' = plr {hand = crds ++ hand plr, moves = filter (\(m, _) -> m == DrawCard) (moves plr), movesHistory = replicate c (PlayCard, True) ++ movesHistory plr }
            putStrLn "Hit Enter to go to next turn."
            _ <- getLine
            return (game { deck = drop c (deck game), players = update p' (players game) })
        -- Invalid
        _ -> do
            putStrLn "Cannot Draw a card this turn, type 'moves', to show available moves."
            doPlayerTurn game



playerPassTurn :: Player -> Game -> IO Game
playerPassTurn plr game = case lookup Pass (moves plr) of
    Just b ->
        do
            let p' = plr { moves = removeFirst (moves plr) (Pass, b), movesHistory = (Pass, b) : movesHistory plr }
            if b
                then
                    doPlayerTurn game { players = update p' (players game) }
                else
                    do
                        -- Find the name of the next player
                        tG <- execCDSLGame (turnOrder game) game
                        nm <- case focus (players tG) of
                            Just p -> return ("Hit Enter to go to " ++ name p ++ "'s turn.")
                            Nothing -> return "Hit Enter to go to next turn."
                        putStrLn nm
                        _ <- getLine
                        return game { players = update p' (players game) }
    _ -> do
        putStrLn "Cannot Pass this turn, type 'moves', to show available moves."
        doPlayerTurn game


playerDiscardCard :: [Int] -> Player -> Game -> IO Game
playerDiscardCard is plr game = case lookup DiscardCard (moves plr) of
    Just b -> if is `elemLst` [1..length (hand plr)]
        then
            do
                putStrLn ("Discarding the cards: " ++ intercalate ", " (map show is))
                let p = plr { hand = remLst (hand plr) (map (\c -> c - 1) is) }
                tG <- execCDSLGame (turnOrder game) game
                nm <- case focus (players tG) of
                    Just p' -> return ("Hit Enter to go to " ++ name p' ++ "'s turn.")
                    Nothing -> return "Hit Enter to go to next turn."
                putStrLn nm
                _ <- getLine
                if b
                    then
                        doPlayerTurn (game { players = update p (players game) })
                    else
                        return (game { players = update p (players game) })
        else
            do
                putStrLn ("Invalid numbers, expected something in the range of 1 to " ++ show (length (hand plr)))
                doPlayerTurn game

    Nothing -> do
        putStrLn "Cannot Discard this turn, type 'moves', to show available moves."
        doPlayerTurn game

-- Returns once a player turn is over
doPlayerTurn :: Game -> IO Game
doPlayerTurn g = do
    -- Applies actions on round start
    let g' = g { state = TurnStart }
    let acts = map (map fst) (lookupAll (state g') (actions g'))
    game <- gameActions acts g'
    case focus (players game) of
        Just plr -> do
            putStr ("Playing -> " ++ gameName game ++ " -> " ++ name plr ++ " > ")
            hFlush stdout
            putStr "\nPile: "
            case pile game of
                [] -> putStrLn "no cards on deck"
                p -> case snd $ head $ pile game of
                    Just c -> prettyPrintCards [c]
                    Nothing -> prettyPrintCards [fst (head p)]
            action <- getLine
            case validatePLCommand action of
                Left cm -> case plc cm of
                    (Play cardIndex) -> playerPlayCard cardIndex plr game

                    (Draw c) -> playerDrawCard c plr game

                    PassTurn -> playerPassTurn plr game

                    (DiscardUA is) -> playerDiscardCard is plr game

                    Moves -> do
                        let mv = moves plr
                        putStr "\nYour moves: "
                        prettyPrintMoves (unique mv)
                        doPlayerTurn game

                    HandUA -> do
                        let cd = hand plr
                        putStr "\nYour hand: "
                        prettyPrintCards cd
                        doPlayerTurn game

                    ScoreUA -> do
                        putStrLn ("Your score: " ++ show (pScore plr))
                        doPlayerTurn game


                    QuitUA -> do
                        putStrLn "You wanna quit the game? (yes/n, default = n)"
                        ans <- getLine
                        case words ans of
                            ["yes"] -> return (game { players = removeR (players game)})
                            _ -> doPlayerTurn game


                    HelpUA -> do
                        printUACommands
                        doPlayerTurn game

                _ -> do
                    putStrLn "Unknown command, type 'help' to list all commands."
                    doPlayerTurn game
        _ -> return game



gameStart :: Int -> IO ()
gameStart gi = do
    plrs <- createPlayer
    let gm = createEmptyGame
    game <- loadGame (gm { players = fromList plrs, state = Start }) gi
    game' <- case lookup PlayerHand (rules game) of
        Just [Numeric n] -> return (dealCards game n)
        _ -> return (dealCards game 3)

    let acts = map (map fst) (lookupAll (state game') (actions game'))
    g <- gameActions acts game'

    game'' <- gameLoop g
    gameEnd game''

gameEnd :: Game -> IO ()
gameEnd g = case find (not . null) (map ($ g) (winCon g)) of
    Just (x:_) ->  putStrLn ("The winner is: " ++ name x ++ "!")
    _ -> putStrLn "No winners!"

createPlayer :: IO [Player]
createPlayer = do
    putStrLn "How many players?"
    c <- getLine
    case readMaybe c :: Maybe Int of
        Just i -> createPlayers i
        Nothing -> do
            putStrLn "Invalid input, please enter a number greater than 2"
            sleep 1
            createPlayer



checkCardEffect :: Card -> Game -> IO Game
checkCardEffect c g = case check (cardEffects g) of
    ef -> case focus (players g) of
        Just p -> exec ef g p
        Nothing -> return g
    where
        exec :: [(CardEffect, Maybe [CDSLExpr])] -> Game -> Player -> IO Game
        exec [] gm _ = return gm
        exec (x:xs) gm p = do
            g' <- execCardEffect x gm p
            exec xs g' p

        -- The expressions is only used, earlier, to check if the card was places, not in the actuall execution of the effect
        check :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> [(CardEffect, Maybe [CDSLExpr])]
        check [] = []
        check (((CEChangeCard, ar), ex):xs)
            | c `cardElem` concatMap exprToCard (mapMaybe getAllCards ex) = (ChangeCard, ar) : check xs
            | otherwise = check xs
        check (((CESwapHand, ar), ex):xs)
            | c `cardElem` concatMap exprToCard (mapMaybe getAllCards ex) = (SwapHand, ar) : check xs
            | otherwise = check xs
        check (((CETakeFromHand, ar), ex):xs)
            | c `cardElem` concatMap exprToCard (mapMaybe getAllCards ex) = (TakeFromHand, ar) : check xs
            | otherwise = check xs
        check (((CEGiveCard, ar), ex):xs)
            | c `cardElem` concatMap exprToCard (mapMaybe getAllCards ex) = (GiveCard, ar) : check xs
            | otherwise = check xs
        check (((CEPassNext, ar), ex):xs)
            | c `cardElem` concatMap exprToCard (mapMaybe getAllCards ex) = (PassNext, ar) : check xs
            | otherwise = check xs
        check (((CEDrawCard, ar), ex):xs)
            | c `cardElem` concatMap exprToCard (mapMaybe getAllCards ex) = (DrawCards, ar) : check xs
            | otherwise = check xs
        check (_:xs) = check xs