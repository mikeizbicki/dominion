module Engine
    where

import Data.List
import Control.Monad
import Control.Monad.Random

import Dominion.Rules
import Dominion.Strategy

----------------------------------------

doTurn :: Config -> GameState -> IO GameState
doTurn cfg gs = do
    putStrLn $ replicate 10 '='
    putStrLn $ "active player: "++show(currentPlayer gs)
    putStrLn $ "supply: " ++ show (supply gs)
    putStrLn $ "hand: " ++ show (hand $ getCurrentPlayerState gs)
    putStrLn $ "moves: "
    gs <- go gs
    gs <- updateCurrentPlayerState (return . resetTurn) gs
    return $ gs { currentPlayer = (currentPlayer gs + 1) `mod` numPlayers gs }
    where
        go gs = case doAction gs action of
            Nothing -> return gs
            Just gs' -> do
                putStrLn $ "  "++show action
                go gs'
            where
                action = getCurrentPlayerStrategy cfg gs gs

getCurrentPlayerStrategy :: Config -> GameState -> GameState -> Action
getCurrentPlayerStrategy cfg gs = playerStrategies cfg !! currentPlayer gs

isGameOver :: GameState -> Bool
isGameOver gs = noprovince || empty3
    where
        noprovince = any go $ supply gs
            where
                go (c,i) = if c==province && i==0
                    then True
                    else False

        empty3 = (sum $ map go $ supply gs) >= 3
            where
                go (_,0) = 1
                go _     = 0

data Config = Config
    { playerStrategies :: [GameState -> Action]
    }

defConfig :: Config
defConfig = Config
    { playerStrategies = 
        [ buyVillage
        , buyWoodcutter
        , buyVPs 
        ]
    }

mkGameState :: StdGen -> Config -> GameState
mkGameState sg cfg = GameState
    { playerStates = mkPlayerStates sg n
    , supply = 
        [ (copper,100)
        , (estate,10)
        , (village,10)
        , (woodcutter,10)
        ]
    , currentPlayer = 0
    }
    where
        n = length $ playerStrategies cfg

        mkPlayerStates sg 0 = []
        mkPlayerStates sg i = initPlayerState sg1:mkPlayerStates sg2 (i-1)
            where
                (sg1,sg2) = split sg

runGame :: Config -> IO ()
runGame cfg = do
    sg <- newStdGen
    let gs = mkGameState sg cfg
    gs <- go gs
    putStrLn "==========================="
    putStrLn "final results"
    forM (playerStates gs) $ \ps -> do
--         putStrLn $ "  cards: "++show (getAllCards ps)
        putStrLn $ "  victory points: " ++ show (getVictoryPoints ps)
    return ()
    where
        go gs = if isGameOver gs
            then return gs
            else do
                gs' <- doTurn cfg gs
                go gs' 
