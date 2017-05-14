module Engine.Simulator
    where

import Data.IORef
import Data.List
import Control.Monad
import Control.Monad.Random (StdGen,split,MonadRandom)
import Control.Monad.Reader
import Numeric

import Debug.Trace
import System.Console.ANSI
import System.Random.Shuffle

import Dominion.Cards
import Dominion.Rules
import Dominion.Setup
import Dominion.Policy.Simple
-- import Dominion.Policy.MDP
import Engine.Monad
import Engine.Ratings

----------------------------------------

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
    where


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

mkGameState :: MonadRandom m => GameConfig -> m GameState
mkGameState cfg = do
    playerStates0 <- replicateM n initPlayerState
    return $ GameState
        { playerStates = playerStates0
        , supply = mkSupply n firstGame 
        , currentPlayer = 0
        , currentRound = 0
        }
    where
        n = length $ players cfg

runGame :: GameConfig -> Sim GameState
runGame cfg = do
    sg <- mkStdGen
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

defPlayers :: [Policy]
defPlayers = 
    [ bigMoney
    , bigCard smithy 1
--     , engMini
--     , engSV4
--     , bigCard woodcutter 1
--     , engSV2
--     , mdp
    ]

watchGame :: IO ()
watchGame = do
    runSim All $ runGame $ GameConfig defPlayers
    return ()

tournament :: Int -> IO ()
tournament n = do
    winners <- forM [1..n] $ \i -> do
        putStrLn $ "starting game "++show i
        players <- shuffleM defPlayers
        gs <- runSim None $ runGame $ GameConfig players
        return $ players !! getWinner gs

    putStrLn $ "score:"
    forM defPlayers $ \s -> do
        putStrLn $ "  "++show s++": "++ show (length $ filter (==s) winners)

    return ()


computeRatings :: IO ()
computeRatings = do

    let players =
            [ bigMoney
            , bigVP
            , engMini
            , engMoat
            , engMSM
            , engMMSM
            , engMSVM
            , engMMSVM
            , engMSVSVM
            , engMMSVSVM
            , engSV1
            , engSV2
            , engSV3
            , engSV4
            ] 
--             ++
--             [ bigCellar 3
--             , bigCard smithy 1
--             , comboCellar mine 1
--             , comboMineMoat 1
--             , comboCellar militia 1
--             , bigCard militia 1
--             ]
            ++
            concatMap (\f -> [f 1, f 2, f 3])
                [ bigCellar
                , bigChapel
                , bigMine
                , bigCard moat 
                , bigCard militia 
                , bigCard smithy 
                , bigCard market 
                , bigCard woodcutter 
                , comboCellar moat
                , comboCellar militia
                , comboCellar smithy
                , comboCellar market
                , comboCellar woodcutter
                , comboCellar mine
                , comboMineMoat
                , engRemodel 
                ]

    let numPlayers = 2
        ratingsPath = "ratings-"++show numPlayers++".txt"
    rs <- loadRatings ratingsPath
    rref <- newIORef rs

    clearScreen
    forM [0..] $ \i -> do
--         clearScreen
        setCursorPosition 0 0
        putStrLn $ "starting game "++show i
        ratings <- readIORef rref
        printRatings ratings

        players' <- fmap (take numPlayers) $ shuffleM players
        let cfg = GameConfig players'

        forM [0] $ \j -> do
            gs <- runSim None $ runGame cfg
            modifyIORef rref $ recordGame cfg gs

        when (i`mod`100==0) $ do
            saveRatings ratingsPath ratings

    return ()
