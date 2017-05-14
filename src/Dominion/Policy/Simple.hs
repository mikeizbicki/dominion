module Dominion.Policy.Simple
    where

import Control.Monad.Random
import Control.Monad.Trans.Maybe
import Data.Monoid
import Debug.Trace

import Dominion.Cards
import Dominion.Rules

----------------------------------------

defPolicy :: Policy
defPolicy = Policy
    { policyName = error "policyName undefined"
    , policyAction = playAllCards
    , policyBuy = error "policyBuy undefined"
    }

bigMoney :: Policy
bigMoney = defPolicy
    { policyName = "big money"
    , policyBuy = buyList [province,gold,silver]
    }

bigVP :: Policy
bigVP = defPolicy
    { policyName = "big VP"
    , policyBuy = buyList [province,gold,duchy,silver,estate]
    }

bigCard :: Card -> Int -> Policy
bigCard c n = defPolicy
    { policyName = "big "++show c++" x"++show n
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo n c
       <> buy silver
    }

bigMine :: Int -> Policy
bigMine n = defPolicy
    { policyName = "big mine x"++show n
    , policyBuy 
        = buyList [province,gold]
       <> buyUpTo n mine
       <> buy silver
    }

bigCellar :: Int -> Policy
bigCellar n = defPolicy
    { policyName = "big cellar x"++show n
    , policyBuy
        = buyList [province,gold,silver]
       <> buyUpTo n cellar
    }

comboCellar :: Card -> Int -> Policy
comboCellar card n = defPolicy
    { policyName = "combo cellar "++show card++" x"++show n
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo n card
       <> buyList [silver,cellar]
    }

comboCellarMine :: Int -> Policy
comboCellarMine n = defPolicy
    { policyName = "combo cellar mine x"++show n
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo n mine
       <> buyList [silver,cellar]
    }

comboMineMoat :: Int -> Policy
comboMineMoat n = defPolicy
    { policyName = "combo mine x"++show n++" moat"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo n mine
       <> buyList [silver,moat]
    }

bigChapel :: Int -> Policy
bigChapel n = defPolicy
    { policyName = "big chapel x"++show n
    , policyBuy
        = buyList [province,gold,silver]
       <> buyUpTo n chapel
    }

engMSM :: Policy
engMSM = defPolicy
    { policyName = "engine MSM"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 1 mine
       <> buyUpTo 1 smithy
       <> buyList [silver,moat]
    }

engMMSM :: Policy
engMMSM = defPolicy
    { policyName = "engine MMSM"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 2 mine
       <> buyUpTo 1 smithy
       <> buyList [silver,moat]
    }

engMMSVM :: Policy
engMMSVM = defPolicy
    { policyName = "engine MMSVM"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 2 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyList [silver,moat]
    }

engMMSVSVM :: Policy
engMMSVSVM = defPolicy
    { policyName = "engine MMSVSVM"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 2 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyUpTo 2 smithy
       <> buyUpTo 2 village
       <> buyList [silver,moat]
    }

engMSVSVM :: Policy
engMSVSVM = defPolicy
    { policyName = "engine MSVSVM"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 1 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyUpTo 2 smithy
       <> buyUpTo 2 village
       <> buyList [silver,moat]
    }

engMSVM :: Policy
engMSVM = defPolicy
    { policyName = "engine MSVM"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 1 mine
       <> buyUpTo 1 smithy
       <> buyUpTo 3 silver
       <> buyUpTo 1 village
       <> buyList [silver,moat]
    }

engRemodel :: Int -> Policy
engRemodel n = defPolicy
    { policyName = "engine remodel x"++show n
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo n remodel
       <> buy silver
    }

engMini :: Policy
engMini = defPolicy
    { policyName = "engine mini"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 2 witch
       <> buyUpTo 2 smithy
       <> buyUpTo 1 village
       <> buyUpTo 2 silver
       <> buyUpTo 1 village
       <> buyList [smithy,silver,cellar]
    }

engMoat :: Policy
engMoat = defPolicy
    { policyName = "engine moat"
    , policyBuy
        = buyList [province,gold]
       <> buyUpTo 1 witch
       <> buyList [smithy]
       <> buyUpTo 1 village
       <> buyUpTo 3 moat
       <> buyList [village,silver]
    }

engSV1 :: Policy
engSV1 = defPolicy
    { policyName = "engine smithy+village v1"
    , policyBuy = \gs -> let ps = getCurrentPlayerState gs in
        ( buy province
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy smithy else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy smithy else passBuy)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [smithy,silver] else passBuy )
       <> (if countNumCards ps village > countNumCards ps smithy
            then buy village
            else buy smithy
          )
       <> buy village
        ) gs
    }

engSV2 :: Policy
engSV2 = defPolicy
    { policyName = "engine smithy+village v2"
    , policyBuy = \gs -> let ps = getCurrentPlayerState gs in
        ( (if countNumCards ps smithy >= 3
            then buy province
            else passBuy
          )
       <> (if countNumCards ps province >= 3
            then buyList [duchy,estate]
            else passBuy
          )
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy smithy else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy smithy else passBuy)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [smithy,silver] else passBuy )
       <> (if money ps >= 7 && buys ps > 1
            then buy smithy
            else passBuy
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

engSV3 :: Policy
engSV3 = defPolicy
    { policyName = "engine smithy+village v3"
    , policyBuy = \gs -> let ps = getCurrentPlayerState gs in
        ( (if countNumCards ps smithy >= 3
            then buy province
            else passBuy
          )
       <> (if countNumCards ps province >= 3
            then buyList [duchy,estate]
            else passBuy
          )
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy militia else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy militia else passBuy)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [militia,silver] else passBuy)
       <> (if money ps >= 7 && buys ps > 1
            then buy smithy
            else passBuy
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

engSV4 :: Policy
engSV4 = defPolicy
    { policyName = "engine smithy+village v4"
    , policyAction = \gs -> let ps = getCurrentPlayerState gs in
        ( (if countNumCards ps village < countNumCards ps smithy
            then try (Play remodel $ APList [estate,village])
            else try (Play remodel $ APList [estate,smithy])
          )
       <> (if countNumCards ps moat < 3 && countNumCards ps silver >= 2
            then try (Play remodel $ APList [copper,moat])
            else pass
          )
       <> (if countNumCards ps smithy >= 3
            then try (Play remodel $ APList [gold,province])
            else pass
          )
       <> playMoney
       <> playList [militia,smithy,moat]
       ) gs
    , policyBuy = \gs -> let ps = getCurrentPlayerState gs in
        ( (if countNumCards ps smithy >= 3
            then buy province
            else passBuy
          )
       <> (if countNumCards ps province >= 4
            then buyList [duchy,estate]
            else passBuy
          )
       <> (if turnsCompleted ps == 0 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 2 then buy copper else passBuy)
       <> (if turnsCompleted ps == 0 && money ps == 5 then buy militia else passBuy)
       <> (if turnsCompleted ps == 1 && money ps == 5 then buy militia else passBuy)
       <> (if turnsCompleted ps == 0 || turnsCompleted ps == 1 then buyList [militia,silver] else passBuy)
       <> (if money ps >= 7 && buys ps > 1
            then buy smithy
            else passBuy
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
   <> try (Play mine $ APList [silver])
   <> try (Play mine $ APList [copper])
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
        playHarbinger gs = try (Play harbinger $ APList xs) gs
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
playCellar gs = try (Play cellar $ APList $ filter isBadCard $ hand ps) gs
    where
        isBadCard c = (c==copper) || (not $ treasure $ cardType c)
        ps = getCurrentPlayerState gs

playRemodel :: GameState -> Action
playRemodel = do
    try (Play remodel $ APList [estate,smithy])
    try (Play remodel $ APList [cellar,village])
    try (Play remodel $ APList [copper,cellar])
    try (Play remodel $ APList [remodel,gold])
    try (Play remodel $ APList [gold,province])

playChapel :: GameState -> Action
playChapel gs = flip try gs $ Play chapel $ APList $ go (totalMoney $ hand ps) [] $ hand ps
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

tryBuy :: Buy -> GameState -> Buy
tryBuy cmdbuy gs = case resolveBuy cmdbuy gs of
    Nothing -> Buy []
    Just _  -> cmdbuy

passBuy :: GameState -> Buy
passBuy _ = Buy []

buy :: Card -> GameState -> Buy
buy c gs = tryBuy (Buy [c]) gs

buyList :: [Card] -> GameState -> Buy
buyList = fmap mconcat . mapM buy

buyUpTo :: Int -> Card -> GameState -> Buy
buyUpTo n c gs = if n <= numCards c (getCurrentPlayerState gs)
    then Buy []
    else buy c gs

--------------------

pass :: GameState -> Action
pass _ = Pass

try :: Action -> GameState -> Action
try a gs = case evalRand (runMaybeT (resolveAction a gs)) (mkStdGen 0) of
    Nothing -> Pass
    Just _  -> a

play :: Card -> GameState -> Action
play c = try (Play c $ APList [])

tryList :: [Action] -> GameState -> Action
tryList = fmap mconcat . mapM try

playList :: [Card] -> GameState -> Action
playList = tryList . map (\c -> Play c $ APList [])

playMoney :: GameState -> Action
playMoney = playList [gold,silver,copper]

numCards :: Card -> PlayerState -> Int
numCards c ps = sum $ map (\c' -> if c==c' then 1 else 0) $ getAllCards ps

hasCard :: Card -> PlayerState -> Bool
hasCard c ps = or $ map (==c) $ getAllCards ps
