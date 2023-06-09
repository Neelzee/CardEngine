module Main where

import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.Console.ANSI (clearScreen)
import Control.Monad (unless, when)
import System.IO ( hFlush, stdout )
import CardGame.PlayGame (gameStart)
import GameData.LoadGD (loadGameData)
import CDSL.CDSLExpr ( CDSLExpr(Numeric, Text) )
import Terminal.ValidateGameCommands (validateGameCommand)
import GameEditor (editor, gameDataStatus)
import Feature (Attribute(GameAttributes), Feature(GameName))

import Functions (allGames, lookupM)
import Terminal.ExecGameCommands
    ( execGameCommands, confirmCommand, printGCEffect, fromCDSLParseErrorOnLoad,  )
import Constants (gameFolder)
import qualified Data.Map as Map
import Terminal.GameCommands
    ( GCError(InvalidCommandArgumentError, MissingOrCorruptDataError,
              CDSLError, GCError, errType, input),
      GameCommand(Quit, Play, Create, Edit, Debug),
      GCEffect(GCEffect, gcErr, se, ve),
      showAll )
import GameData.DebugGD (loadGameDataDebug)

-- Play the selected game
playGame :: Int -> IO ()
playGame n = do
    games <- listDirectory "games"
    if n >= 0 && n < length games
        then
            gameStart n
        else
            putStrLn "Invalid game number."


mainExecCmd :: String -> IO ()
mainExecCmd s = case validateGameCommand s of
    Right err -> do
        print err
        mainLoop
    Left gc -> case gc of
        (Play (Numeric i)) -> do
            playGame i
            mainLoop
        -- Create Command
        (Create (Text gm) flg) -> do
                let gce = [GCEffect { se = "Created GameData: " ++ gm, ve = "Created GameData: " ++ gm, gcErr = [] }]
                res <- confirmCommand gc gce flg
                if res
                    then
                        do
                            let im = Map.fromList [((GameName, Nothing), [Text gm])]
                            editor (Map.fromList [(GameAttributes, im)])
                            mainLoop
                    else
                        mainLoop
        -- Edit Command
        (Edit (Numeric i) flg) -> do
            g <- allGames
            if i `elem` [0..(length g)]
                then
                    do
                        res <- loadGameData Map.empty i
                        case res of
                            Left gd' -> do
                                gce <- case Map.lookup GameAttributes gd' of
                                    Just att -> case lookupM GameName att of
                                        Just (_, [Text gm]) -> return [GCEffect { se = "Loaded GameData: " ++ gm
                                            , ve = "Loaded GameData: " ++ gm
                                            , gcErr = [] }]
                                        _ -> return [GCEffect { se = "Loaded GameData"
                                            , ve = "Loaded GameData"
                                            , gcErr = [GCError { errType = MissingOrCorruptDataError "No GameName Found", input = showAll gc}] }]
                                    Nothing -> return [GCEffect { se = "Loaded GameData"
                                            , ve = "Loaded GameData"
                                            , gcErr = [GCError { errType = MissingOrCorruptDataError "No GameName Found", input = showAll gc}] }]
                                r <- confirmCommand gc gce flg
                                if r
                                    then
                                        do
                                            editor gd'
                                            mainLoop
                                    else
                                        mainLoop
                            Right err -> do
                                printGCEffect [fromCDSLParseErrorOnLoad err] flg
                                mainLoop
                else
                    do
                        printGCEffect [GCEffect { se = "Invalid number: '" ++ show i ++ "'"
                            , ve = "Invalid number: '" ++ show i ++ "', expected a number in the range of 0 to " ++ show (length g)
                            , gcErr = [GCError { errType = InvalidCommandArgumentError (show i), input = showAll gc }] }] flg
                        mainLoop

        -- Debug Command

        (Debug (Numeric i) flg) -> do
            let ge = GCEffect { se = "All information about the game, and errors, if any", ve = "All information about the game, and errors, if any", gcErr = [] }
            g <- allGames
            if i `elem` [0..(length g)]
                then
                    do
                        r <- confirmCommand gc [ge] flg
                        when r $ do
                                    (gd, res) <- loadGameDataDebug i
                                    putStrLn "\nGameData:\n"
                                    printGCEffect (gameDataStatus gd) flg
                                    putStrLn "\nErrors:\n"
                                    printGCEffect (map (\(e, n) -> GCEffect { se = "Error at line: " ++ show n, ve = "Error at line: " ++ show n, gcErr = [GCError { errType = CDSLError (Right [e]), input = "" }] }) res) flg
                                    mainLoop
                else
                    do
                        printGCEffect [GCEffect { se = "Invalid number: '" ++ show i ++ "'"
                            , ve = "Invalid number: '" ++ show i ++ "', expected a number in the range of 0 to " ++ show (length g)
                            , gcErr = [GCError { errType = InvalidCommandArgumentError (show i), input = showAll gc }] }] flg
                        mainLoop

        -- Quit
        (Quit flgs) -> do
            res <- confirmCommand gc [GCEffect { se = "Quitting program.", ve = "Quitting program.", gcErr = [] }] flgs
            unless res mainLoop
        -- Generic commands
        _ -> do
            execGameCommands gc
            mainLoop

-- Main program loop
mainLoop :: IO ()
mainLoop = do
    putStr "> "
    hFlush stdout
    c <- getLine
    mainExecCmd c

-- Main program entry point
main :: IO ()
main = do
    setup
    clearScreen
    putStrLn "Welcome to CardEngine"
    mainLoop


-- Ensures that the game folder exists
setup :: IO ()
setup = do
    let dir = gameFolder
    exists <- doesDirectoryExist dir
    createDirectoryIfMissing exists dir