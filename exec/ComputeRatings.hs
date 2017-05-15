module Main 
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
import Engine.Monad
import Engine.Ratings
import Engine.Simulator

main :: IO ()
main = do
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
