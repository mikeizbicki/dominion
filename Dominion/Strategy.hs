module Dominion.Strategy
    where

import Data.Monoid
import Debug.Trace

import Dominion.Rules

----------------------------------------

data Strategy = Strategy
    { strategyName :: String
    , strategyAction :: GameState -> Action
    }

instance Show Strategy where
    show = strategyName

instance Eq Strategy where
    s1==s2 = strategyName s1==strategyName s2

bigMoney :: Strategy
bigMoney = Strategy
    { strategyName = "big money"
    , strategyAction 
        = playMoney
       <> buyList [province,gold,silver]
    }

bigWoodcutter :: Strategy
bigWoodcutter = Strategy
    { strategyName = "BM woodcutter"
    , strategyAction
        = playMoney 
       <> play woodcutter 
       <> buyUpTo 1 woodcutter 
       <> buyList [province,gold,silver]
    }

miniEngine :: Strategy
miniEngine = Strategy
    { strategyName = "mini engine"
    , strategyAction 
        = playMoney 
       <> play village
       <> play festival
       <> play smithy
       <> play woodcutter
       <> buyList [province,gold,smithy]
       <> buyUpTo 1 village
       <> buyUpTo 1 woodcutter
       <> buyList [village,silver]
    }

bigSmithy :: Strategy
bigSmithy = Strategy
    { strategyName = "big smithy"
    , strategyAction 
        = playMoney 
       <> play village
       <> play festival
       <> play smithy
       <> buyList [province,gold,smithy,silver]
    }

----------------------------------------

try :: Action -> GameState -> Action
try a gs = case doAction gs a of
    Nothing -> Pass
    Just _  -> a

buy :: Card -> GameState -> Action
buy c = try (Buy c)

play :: Card -> GameState -> Action
play c = try (Play c)

tryList :: [Action] -> GameState -> Action
tryList = fmap mconcat . mapM try

buyList :: [Card] -> GameState -> Action
buyList = tryList . map Buy

playList :: [Card] -> GameState -> Action
playList = tryList . map Play

playMoney :: GameState -> Action
playMoney = playList [gold,silver,copper]

buyUpTo :: Int -> Card -> GameState -> Action
buyUpTo n c gs = if n < numCards c (getCurrentPlayerState gs)
    then Pass
    else buy c gs

numCards :: Card -> PlayerState -> Int
numCards c ps = sum $ map (\c' -> if c==c' then 1 else 0) $ getAllCards ps

hasCard :: Card -> PlayerState -> Bool
hasCard c ps = or $ map (==c) $ getAllCards ps
