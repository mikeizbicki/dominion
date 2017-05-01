module Engine.Simulator
    where

import Data.List
import Control.Monad
import Control.Monad.Random (StdGen,split)
import Control.Monad.Reader
import System.Random.Shuffle

import Dominion.Rules
import Dominion.Strategy
import Engine.Monad

----------------------------------------

doTurn :: Config -> GameState -> Sim GameState
doTurn cfg gs = do
    writeMsg Turn $ replicate 10 '='
    writeMsg Turn $ "active player: "++show(currentPlayer gs)
    writeMsg Turn $ "supply: " ++ show (supply gs)
    writeMsg Turn $ "hand: " ++ show (hand $ getCurrentPlayerState gs)
    writeMsg Turn $ "moves: "
    gs <- go gs
    gs <- updateCurrentPlayerState (return . resetTurn) gs
    return $ gs { currentPlayer = (currentPlayer gs + 1) `mod` numPlayers gs }
    where
        go gs = case doAction gs action of
            Nothing -> return gs
            Just gs' -> do
                writeMsg Turn $ "  "++show action
                go gs'
            where
                action :: Action
                action = strategyAction (playerStrategies cfg !! currentPlayer gs) gs

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
    { playerStrategies :: [Strategy]
    }

mkGameState :: StdGen -> Config -> GameState
mkGameState sg cfg = GameState
    { playerStates = mkPlayerStates sg n
    , supply = defSupply
    , currentPlayer = 0
    }
    where
        n = length $ playerStrategies cfg

        mkPlayerStates sg 0 = []
        mkPlayerStates sg i = initPlayerState sg1:mkPlayerStates sg2 (i-1)
            where
                (sg1,sg2) = split sg

runGame :: Config -> Sim GameState
runGame cfg = do
    sg <- mkStdGen
--     let sg = mkStdGen 0
    let gs = mkGameState sg cfg
    gs <- go gs
    writeMsg Game "==========================="
    writeMsg Game "final results"
    forM (getPlayerIDs gs) $ \i -> do
        writeMsg Game $ "  score: " ++ show (getScore gs i)
    writeMsg Game $ "player "++show (getWinner gs)++" wins!"
    return gs
    where
        go gs = if isGameOver gs
            then return gs
            else do
                gs' <- doTurn cfg gs
                go gs' 

defConfig :: Config
defConfig = Config
    { playerStrategies = 
        [ bigMoney
        , bigWoodcutter
        , miniEngine
        ]
    }

tournament :: Int -> IO ()
tournament n = do
    let strats = [bigMoney, bigWoodcutter, miniEngine,bigSmithy]
    winners <- forM [1..n] $ \i -> do
        putStrLn $ "starting game "++show i
        strats' <- shuffleM strats
        gs <- runSim None $ runGame $ Config strats'
        return $ strats' !! getWinner gs

    putStrLn $ "score:"
    forM strats $ \s -> do
        putStrLn $ "  "++show s++": "++ show (length $ filter (==s) winners)

    return ()

getWinner :: GameState -> Int
getWinner gs = head $ elemIndices maxScore scores
    where
        maxScore :: Score
        maxScore = maximum scores

        scores :: [Score]
        scores = map (getScore gs) $ getPlayerIDs gs
