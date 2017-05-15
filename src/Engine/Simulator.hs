module Engine.Simulator
    where

import Data.IORef
import Data.List
import Control.Monad
import Control.Monad.Random (StdGen,split,MonadRandom)
import Control.Monad.Reader
import Numeric

import System.Random.Shuffle

import Dominion.Cards
import Dominion.Rules
import Dominion.Setup
import Dominion.Policy.Simple
import Engine.Monad
import Engine.Ratings

----------------------------------------

runGame :: GameConfig -> Sim GameState
runGame cfg = do
    gs <- mkGameState cfg
    gs <- go gs
    writeMsg Game "==========================="
    writeMsg Game "final results"
    forM (getPlayerIDs gs) $ \i -> do
        writeMsg Game $ "  "++show (players cfg !! i)++": " ++ show (getScore gs i)
    writeMsg Game $ "player "++show (getWinner gs)++" wins!"
    return gs
    where
        go gs = if isGameOver gs
            then return gs
            else do
                gs' <- doTurn cfg gs
                go gs' 

doTurn :: GameConfig -> GameState -> Sim GameState
doTurn cfg gs = do
    when (currentPlayer gs == 0) $ writeMsg Turn $ "\n\n\n"
    writeMsg Turn $ replicate 10 '='
    writeMsg Turn $ "active player: "++show (currentPlayer gs) ++ " ("++show (players cfg!!currentPlayer gs)++")"
    writeMsg Turn $ "turn number: "++show (turnsCompleted $ getCurrentPlayerState gs) 
    writeMsg Turn $ "supply: " ++ show (supply gs)
    writeMsg Turn $ "deck: " ++ show (deck $ getCurrentPlayerState gs)
    writeMsg Turn $ "hand: " ++ show (hand $ getCurrentPlayerState gs)
    writeMsg Turn $ "discard: " ++ show (discard $ getCurrentPlayerState gs)
    writeMsg Turn $ "moves: "
--     when (players cfg !! currentPlayer gs == bigMoney)
--         $ writeMsg None $ " vpDensity: " ++ showFFloat (Just 2) (vpDensity gs) "" 
--                  ++ "; treasureDensity: "++ showFFloat (Just 2) (treasureDensity gs) ""
    gs <- doAction (policyAction (players cfg !! currentPlayer gs)) gs
    gs <- doBuy    (policyBuy    (players cfg !! currentPlayer gs)) gs
    writeMsg Turn $ "deck: " ++ show (deck $ getCurrentPlayerState gs)
    writeMsg Turn $ "hand: " ++ show (hand $ getCurrentPlayerState gs)
    writeMsg Turn $ "played: " ++ show (played $ getCurrentPlayerState gs)
    writeMsg Turn $ "discard: " ++ show (discard $ getCurrentPlayerState gs)
    gs <- updatePlayerStateM CurrentPlayer cleanUpPhase gs
    return $ gs 
        { currentPlayer = (currentPlayer gs + 1) `mod` numPlayers gs 
        }

--------------------
defPolicies :: [Policy]
defPolicies = 
    [ bigMoney
    , bigCard smithy 1
    ]

watchGame :: [Policy] -> IO ()
watchGame ps = do
    runSim All $ runGame $ GameConfig ps
    return ()

tournament :: [Policy] -> Int -> IO ()
tournament ps n = do
    winners <- forM [1..n] $ \i -> do
        putStrLn $ "starting game "++show i
        players <- shuffleM ps
        gs <- runSim None $ runGame $ GameConfig players
        return $ players !! getWinner gs

    putStrLn $ "score:"
    forM ps $ \s -> do
        putStrLn $ "  "++show s++": "++ show (length $ filter (==s) winners)

    return ()


