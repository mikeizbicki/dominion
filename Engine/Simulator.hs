module Engine.Simulator
    where

import Data.List
import Control.Monad
import Control.Monad.Random (StdGen,split)
import Control.Monad.Reader
import Numeric
import System.Random.Shuffle

import Data.IORef

import Dominion.Cards
import Dominion.Rules
import Dominion.Setup
import Dominion.Strategy
import Engine.Monad

----------------------------------------

doTurn :: Config -> GameState -> Sim GameState
doTurn cfg gs = do
    writeMsg Turn $ replicate 10 '='
    writeMsg Turn $ "active player: "++show (currentPlayer gs) ++ " ("++show (playerStrategies cfg!!currentPlayer gs)++")"
    writeMsg Turn $ "turn number: "++show (turnsCompleted $ getCurrentPlayerState gs) 
    writeMsg Turn $ "supply: " ++ show (supply gs)
    writeMsg Turn $ "deck: " ++ show (deck $ getCurrentPlayerState gs)
    writeMsg Turn $ "hand: " ++ show (hand $ getCurrentPlayerState gs)
    writeMsg Turn $ "discard: " ++ show (discard $ getCurrentPlayerState gs)
    writeMsg Turn $ "moves: "
--     when (playerStrategies cfg !! currentPlayer gs == bigMoney)
--         $ writeMsg None $ " vpDensity: " ++ showFFloat (Just 2) (vpDensity gs) "" 
--                  ++ "; treasureDensity: "++ showFFloat (Just 2) (treasureDensity gs) ""
    gs <- go gs
    gs <- return $ updatePlayerState CurrentPlayer cleanUpPhase gs
    return $ gs 
        { currentPlayer = (currentPlayer gs + 1) `mod` numPlayers gs 
        }
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
    , supply = mkSupply (length $ playerStrategies cfg) firstGame 
    , currentPlayer = 0
    , currentRound = 0
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
        writeMsg Game $ "  "++show (playerStrategies cfg !! i)++": " ++ show (getScore gs i)
    writeMsg Game $ "player "++show (getWinner gs)++" wins!"
    return gs
    where
        go gs = if isGameOver gs
            then return gs
            else do
                gs' <- doTurn cfg gs
                go gs' 

defPlayers :: [Strategy]
defPlayers = 
    [ bigMoney
    , bigSmithy
    , miniEngine
    ]

watchGame :: IO ()
watchGame = do
    runSim All $ runGame $ Config defPlayers
    return ()

tournament :: Int -> IO ()
tournament n = do
    winners <- forM [1..n] $ \i -> do
        putStrLn $ "starting game "++show i
        players <- shuffleM defPlayers
        gs <- runSim None $ runGame $ Config players
        return $ players !! getWinner gs

    putStrLn $ "score:"
    forM defPlayers $ \s -> do
        putStrLn $ "  "++show s++": "++ show (length $ filter (==s) winners)

    return ()


elo :: Int -> [Strategy] -> IO ()
elo n players = do

    eloref <- newIORef $ Elo []

    forM [1..n] $ \i -> do
        putStrLn $ "starting game "++show i
        players' <- fmap (take 2) $ shuffleM players
        gs <- runSim None $ runGame $ Config players'
        let winnerid = getWinner gs
            loserid  = (winnerid+1)`mod`2
            winner = players' !! winnerid
            loser  = players' !! loserid

        modifyIORef eloref (updateElo (winner,1) (loser,0))

        putStrLn "Elo ratings:"
        (Elo xs) <- readIORef eloref
        forM xs $ \(p,r) -> 
            putStrLn $ "  "++show p++": "++showFFloat (Just 2) r ""
--     putStrLn $ "elo: " ++ show elo
--     forM defPlayers $ \s -> do
--         putStrLn $ "  "++show s++": "++ show (length $ filter (==s) winners)

    return ()

data Elo = Elo [(Strategy,Double)]
    deriving (Show)

updateElo :: (Strategy,Double) -> (Strategy,Double) -> Elo -> Elo
updateElo (p1,s1) (p2,s2) (Elo xs) = Elo $ map go xs2
    where
        xs1 = case lookup p1 xs of
            Nothing -> (p1,r1):xs
            Just _  -> xs

        xs2 = case lookup p2 xs1 of
            Nothing -> (p2,r1):xs1
            Just _  -> xs1

        go (p,r) = if p==p1
            then (p,r1')
            else if p==p2
                then (p, r2')
                else (p, r)

        r1 = case lookup p1 xs of
            Nothing -> 0
            Just r  -> r
        
        r2 = case lookup p2 xs of 
            Nothing -> 0
            Just r  -> r

        k = 32
        a = 400

        e1 = 1/(1+exp((r2-r1)/a))
        e2 = 1/(1+exp((r1-r2)/a))

        r1' = r1 + k*(s1-e1)
        r2' = r2 + k*(s2-e2)
