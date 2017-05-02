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

cpStrategy :: Strategy -> Strategy
cpStrategy s = s { strategyName = strategyName s ++ "X" }

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
       <> buyList [province,gold]
       <> buyUpTo 1 woodcutter 
       <> buy silver
    }

bigMilitia :: Strategy
bigMilitia = Strategy
    { strategyName = "BM militia"
    , strategyAction
        = playMoney 
       <> play militia 
       <> buyList [province,gold]
       <> buyUpTo 3 militia 
       <> buy silver
    }

bigWitch :: Strategy
bigWitch = Strategy
    { strategyName = "BM witch"
    , strategyAction
        = playMoney 
       <> play witch 
       <> buyList [province,gold]
       <> buyUpTo 3 witch 
       <> buy silver
    }

banditti :: Strategy
banditti = Strategy
    { strategyName = "BM bandits"
    , strategyAction
        = playMoney
       <> play bandit
       <> buyList [province,gold]
       <> buyUpTo 3 bandit
       <> buy silver
    }

miniEngine :: Strategy
miniEngine = Strategy
    { strategyName = "mini engine"
    , strategyAction 
        = playMoney 
       <> playList [village,festival,witch,smithy,moat,woodcutter]
       <> buyList [province,gold]
       <> buyUpTo 2 witch
       <> buyList [smithy]
       <> buyUpTo 1 village
       <> buyUpTo 1 woodcutter
       <> buyList [village,silver]
    }

moatEngine :: Strategy
moatEngine = Strategy
    { strategyName = "moat engine"
    , strategyAction 
        = playMoney 
       <> playList [village,festival,witch,smithy,moat,woodcutter]
       <> buyList [province,gold]
       <> buyUpTo 1 witch
       <> buyList [smithy]
--        <> buyUpTo 1 village
       <> buyUpTo 3 moat
       <> buyList [village,silver]
    }

bigSmithy :: Strategy
bigSmithy = Strategy
    { strategyName = "big smithy"
    , strategyAction 
        = playMoney 
       <> play smithy
       <> buyList [province,gold,smithy,silver]
    }

bigMoat :: Strategy
bigMoat = Strategy
    { strategyName = "big moat"
    , strategyAction 
        = playMoney 
       <> play moat
       <> buyList [province,gold]
       <> buyUpTo 2 moat
       <> buyList [silver,moat]
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
