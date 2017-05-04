module Dominion.Strategy
    where

import Data.Monoid
import Debug.Trace

import Dominion.Cards
import Dominion.Rules

----------------------------------------

data Strategy = Strategy
    { strategyName :: String
    , strategyAction :: GameState -> Action
--     , strategyBuy :: GameState -> [Card]
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
--     , strategyBuy
--         = tryToBuy [province]
--     }
--  
-- data B 
--     = B [Card]
--     | NoB
-- 
-- tryToBuy :: GameState -> [Card] -> B
-- tryToBuy gs xs = if canBuyAllCards gs xs
--     then B xs
--     else NoB
--     where
--         canBuyAllCards gs []     = True
--         canBuyAllCards gs (x:xs) = case doAction gs (Buy x) of
--             Nothing  -> False
--             Just gs' -> canBuyAllCards gs' xs


bigMine :: Strategy
bigMine = Strategy
    { strategyName = "bm mine"
    , strategyAction 
        = try (Play mine [silver])
       <> try (Play mine [copper])
       <> playMoney
       <> buyList [province,gold]
       <> buyUpTo 2 mine
       <> buy silver
    }

bigCellar :: Strategy
bigCellar = Strategy
    { strategyName = "bm cellar"
    , strategyAction
        = playCellar
       <> playMoney
       <> buyList [province,gold,silver]
       <> buyUpTo 2 cellar
    }
    where
        playCellar gs = try (Play cellar $ filter isBadCard $ hand ps) gs
            where
                isBadCard c = (c==copper) || (not $ treasure $ cardType c)
                ps = getCurrentPlayerState gs

bigChapel :: Strategy
bigChapel = Strategy
    { strategyName = "bm chapel"
    , strategyAction
        = playChapel
       <> playMoney
       <> buyList [province,gold,silver]
       <> buyUpTo 3 chapel
    }
    where
        playChapel gs = flip try gs $ Play chapel $ go (totalMoney $ hand ps) [] $ hand ps
            where
                ps = getCurrentPlayerState gs

                go n ret []     = ret
                go n ret (x:xs) = if n==8 || n==6 || n==3
                    then ret
                    else go n' ret' xs
                    where
                        (n',ret') = if x==copper
                            then (n-1,x:ret)
                            else if x==estate || x==chapel
                                then (n,x:ret)
                                else (n,ret)

        totalMoney :: [Card] -> Int
        totalMoney [] = 0
        totalMoney (x:xs) = if x==copper
            then totalMoney xs+1
            else if x==silver
                then totalMoney xs+2
                else if x==gold
                    then totalMoney xs+3
                    else totalMoney xs

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
       <> buyUpTo 2 smithy
       <> buyUpTo 1 village
       <> buyUpTo 2 silver
       <> buyUpTo 1 village
       <> buyList [smithy,silver,cellar]
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
       <> buyList [province,gold]
       <> buyUpTo 1 smithy
       <> buyList [silver]
    }

bigSmithy2 :: Strategy
bigSmithy2 = Strategy
    { strategyName = "big smithy2"
    , strategyAction 
        = playMoney 
       <> play smithy
       <> buyList [province,gold]
       <> buyUpTo 2 smithy
       <> buyList [silver]
    }

bigSmithy3 :: Strategy
bigSmithy3 = Strategy
    { strategyName = "big smithy3"
    , strategyAction 
        = playMoney 
       <> play smithy
       <> buyList [province,gold]
       <> buyUpTo 3 smithy
       <> buyList [silver]
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
play c = try (Play c [])

tryList :: [Action] -> GameState -> Action
tryList = fmap mconcat . mapM try

buyList :: [Card] -> GameState -> Action
buyList = tryList . map Buy

playList :: [Card] -> GameState -> Action
playList = tryList . map (\c -> Play c [])

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
