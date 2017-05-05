module Dominion.Setup
    where

import Control.Monad.Random

import Dominion.Cards
import Dominion.Rules

----------------------------------------

firstGame :: [Card]
firstGame = [cellar,market,militia,mine,moat,remodel,smithy,village,woodcutter,workshop]

initPlayerState :: StdGen -> PlayerState
initPlayerState stdgen = cleanUpPhase $ PlayerState
    { deck = [] 
    , hand = []
    , played = []
    , discard = replicate 7 copper ++ replicate 3 estate
    , actions = 0
    , buys = 0
    , money = 0
    , turnsCompleted = -1
    , stdgen = stdgen
    }

mkSupply 
    :: Int          -- ^ number of players
    -> [Card]       -- ^ the kingdom cards
    -> [(Card,Int)] -- ^ the starting supply
mkSupply n cs =
    [ (copper       ,60)
    , (silver       ,40)
    , (gold         ,30)
    , (curse        ,10*n)
    , (estate       ,numcards)
    , (duchy        ,numcards)
    , (province     ,numcards)
    ]
    ++
    map (\c -> (c,numcards)) cs
    where
        numcards = if n > 2
            then 12
            else 8
