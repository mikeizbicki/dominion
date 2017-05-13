module Dominion.Strategy
    where

import Data.Monoid
import Debug.Trace

import Dominion.Cards
import Dominion.Rules

----------------------------------------

bigMoney :: Strategy
bigMoney = Strategy
    { strategyName = "big money"
    , strategyAction 
        = playAllCards
       <> buyList [province,gold,silver]
    }

bigVP :: Strategy
bigVP = Strategy
    { strategyName = "big VP"
    , strategyAction 
        = playAllCards
       <> buyList [province,gold,duchy,silver,estate]
    }

bigCard :: Card -> Int -> Strategy
bigCard c n = Strategy
    { strategyName = "big "++show c++" x"++show n
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo n c
       <> buy silver
    }

bigMine :: Int -> Strategy
bigMine n = Strategy
    { strategyName = "big mine x"++show n
    , strategyAction 
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo n mine
       <> buy silver
    }

bigCellar :: Int -> Strategy
bigCellar n = Strategy
    { strategyName = "big cellar x"++show n
    , strategyAction
        = playAllCards
       <> buyList [province,gold,silver]
       <> buyUpTo n cellar
    }

comboCellar :: Card -> Int -> Strategy
comboCellar card n = Strategy
    { strategyName = "combo cellar "++show card++" x"++show n
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo n card
       <> buyList [silver,cellar]
    }

comboCellarMine :: Int -> Strategy
comboCellarMine n = Strategy
    { strategyName = "combo cellar mine x"++show n
    , strategyAction 
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo n mine
       <> buyList [silver,cellar]
    }

comboMineMoat :: Int -> Strategy
comboMineMoat n = Strategy
    { strategyName = "combo mine x"++show n++" moat"
    , strategyAction 
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo n mine
       <> buyList [silver,moat]
    }

bigChapel :: Int -> Strategy
bigChapel n = Strategy
    { strategyName = "big chapel x"++show n
    , strategyAction
        = playAllCards
       <> buyList [province,gold,silver]
       <> buyUpTo n chapel
    }

engMSM :: Strategy
engMSM = Strategy
    { strategyName = "engine MSM"
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 1 mine
       <> buyUpTo 1 smithy
       <> buyList [silver,moat]
    }

engMMSM :: Strategy
engMMSM = Strategy
    { strategyName = "engine MMSM"
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 2 mine
       <> buyUpTo 1 smithy
       <> buyList [silver,moat]
    }

engMMSVM :: Strategy
engMMSVM = Strategy
    { strategyName = "engine MMSVM"
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 2 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyList [silver,moat]
    }

engMMSVSVM :: Strategy
engMMSVSVM = Strategy
    { strategyName = "engine MMSVSVM"
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 2 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyUpTo 2 smithy
       <> buyUpTo 2 village
       <> buyList [silver,moat]
    }

engMSVSVM :: Strategy
engMSVSVM = Strategy
    { strategyName = "engine MSVSVM"
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 1 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyUpTo 2 smithy
       <> buyUpTo 2 village
       <> buyList [silver,moat]
    }

engMSVM :: Strategy
engMSVM = Strategy
    { strategyName = "engine MSVM"
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 1 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyList [silver,moat]
    }

engRemodel :: Int -> Strategy
engRemodel n = Strategy
    { strategyName = "engine remodel x"++show n
    , strategyAction
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo n remodel
       <> buy silver
    }

engMini :: Strategy
engMini = Strategy
    { strategyName = "engine mini"
    , strategyAction 
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 2 witch
       <> buyUpTo 2 smithy
       <> buyUpTo 1 village
       <> buyUpTo 2 silver
       <> buyUpTo 1 village
       <> buyList [smithy,silver,cellar]
    }

engMoat :: Strategy
engMoat = Strategy
    { strategyName = "engine moat"
    , strategyAction 
        = playAllCards
       <> buyList [province,gold]
       <> buyUpTo 1 witch
       <> buyList [smithy]
       <> buyUpTo 1 village
       <> buyUpTo 3 moat
       <> buyList [village,silver]
    }

engSV1 :: Strategy
engSV1 = Strategy
    { strategyName = "engine smithy+village v1"
    , strategyAction = \gs -> let ps = getCurrentPlayerState gs in
        ( playAllCards
       <> buy province
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy smithy else pass)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy smithy else pass)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [smithy,silver] else pass)
       <> (if countNumCards ps village > countNumCards ps smithy
            then buy village
            else buy smithy
          )
       <> buy village
        ) gs
    }

engSV2 :: Strategy
engSV2 = Strategy
    { strategyName = "engine smithy+village v2"
    , strategyAction = \gs -> let ps = getCurrentPlayerState gs in
        ( playAllCards
       <> (if countNumCards ps smithy >= 3
            then buy province
            else pass
          )
       <> (if countNumCards ps province >= 3
            then buyList [duchy,estate]
            else pass
          )
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy smithy else pass)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy smithy else pass)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [smithy,silver] else pass)
       <> (if money ps >= 7 && buys ps > 1
            then buy smithy
            else pass
          )
       <> buyUpTo 1 gold
       <> buyUpTo 5 market
       <> (if countNumCards ps village < countNumCards ps smithy
            then buy village
            else buy smithy
          )
       <> buy village
       <> buy moat
        ) gs
    }

engSV3 :: Strategy
engSV3 = Strategy
    { strategyName = "engine smithy+village v3"
    , strategyAction = \gs -> let ps = getCurrentPlayerState gs in
        ( playAllCards
       <> (if countNumCards ps smithy >= 3
            then buy province
            else pass
          )
       <> (if countNumCards ps province >= 3
            then buyList [duchy,estate]
            else pass
          )
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy militia else pass)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy militia else pass)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [militia,silver] else pass)
       <> (if money ps >= 7 && buys ps > 1
            then buy smithy
            else pass
          )
       <> buyUpTo 1 gold 
       <> buyUpTo 5 market 
       <> (if countNumCards ps village < countNumCards ps smithy
            then buy village
            else buy smithy
          )
       <> buy village 
       <> buy smithy
       <> buy moat
       ) gs
    }

engSV4 :: Strategy
engSV4 = Strategy
    { strategyName = "engine smithy+village v4"
    , strategyAction = \gs -> let ps = getCurrentPlayerState gs in
        ( playList [village,market]
       <> (if countNumCards ps village < countNumCards ps smithy
            then try (Play remodel [estate,village])
            else try (Play remodel [estate,smithy])
          )
       <> (if countNumCards ps moat < 3 && countNumCards ps silver >= 2
            then try (Play remodel [copper,moat])
            else pass
          )
       <> (if countNumCards ps smithy >= 3
            then try (Play remodel [gold,province])
            else pass
          )
       <> playMoney
       <> playList [militia,smithy,moat]
       <> (if countNumCards ps smithy >= 3
            then buy province
            else pass
          )
       <> (if countNumCards ps province >= 4
            then buyList [duchy,estate]
            else pass
          )
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else pass)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy militia else pass)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy militia else pass)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [militia,silver] else pass)
       <> (if money ps >= 7 && buys ps > 1
            then buy smithy
            else pass
          )
       <> buyUpTo 1 remodel
       <> buyUpTo 1 militia 
       <> buyUpTo 1 gold 
       <> buyUpTo 5 market 
       <> (if countNumCards ps village < countNumCards ps smithy
            then buy village
            else buy smithy
          )
       <> buy village 
       <> buy smithy
       <> buyUpTo 3 moat
       ) gs
    }
    
    
----------------------------------------

playAllCards :: GameState -> Action
playAllCards
    = playList [village]
   <> playThroneRoom
   <> playList [festival,market,laboratory]
   <> playCellar
   <> playHarbinger
   <> playPoacher
   <> play witch
   <> playLibrary
   <> playArtisan
   <> try (Play mine [silver])
   <> try (Play mine [copper])
   <> playList [silver,gold]
   <> playList [merchant]
   <> playList [copper]
   <> playRemodel
   <> playChapel
   <> play moneylender
   <> playList [bandit,councilRoom,smithy,vassal,bureaucrat,militia,moat,woodcutter]
   where
        -- FIXME: All of these functions are broken
        playHarbinger :: GameState -> Action
        playHarbinger gs = try (Play harbinger xs) gs
            where
                xs = []

        playLibrary :: GameState -> Action
        playLibrary gs = Pass

        playArtisan :: GameState -> Action
        playArtisan gs = Pass

        playThroneRoom :: GameState -> Action
        playThroneRoom gs = Pass

        playPoacher :: GameState -> Action
        playPoacher gs = Pass

        playSentry :: GameState -> Action
        playSentry gs = Pass

playCellar :: GameState -> Action
playCellar gs = try (Play cellar $ filter isBadCard $ hand ps) gs
    where
        isBadCard c = (c==copper) || (not $ treasure $ cardType c)
        ps = getCurrentPlayerState gs

playRemodel :: GameState -> Action
playRemodel = do
    try (Play remodel [estate,smithy])
    try (Play remodel [cellar,village])
    try (Play remodel [copper,cellar])
    try (Play remodel [remodel,gold])
    try (Play remodel [gold,province])

playChapel :: GameState -> Action
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


----------------------------------------

pass :: GameState -> Action
pass _ = Pass

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
buyUpTo n c gs = if n <= numCards c (getCurrentPlayerState gs)
    then Pass
    else buy c gs

numCards :: Card -> PlayerState -> Int
numCards c ps = sum $ map (\c' -> if c==c' then 1 else 0) $ getAllCards ps

hasCard :: Card -> PlayerState -> Bool
hasCard c ps = or $ map (==c) $ getAllCards ps
