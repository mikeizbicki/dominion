module Dominion.Setup
    where

import Control.Monad.Random

import Dominion.Cards
import Dominion.Rules

----------------------------------------

firstGame :: [Card]
firstGame = [cellar,market,militia,mine,moat,remodel,smithy,village,woodcutter,workshop]

sizeDistortion :: [Card]
sizeDistortion = [artisan,bandit,bureaucrat,chapel,festival,gardens,sentry,throneRoom,witch,workshop]

deckTop :: [Card]
deckTop = [artisan,bureaucrat,councilRoom,festival,harbinger,laboratory,moneylender,sentry,vassal,village]

sleightOfHand :: [Card]
sleightOfHand = [cellar,councilRoom,festival,gardens,library,harbinger,militia,poacher,smithy,throneRoom]

improvements :: [Card]
improvements = [artisan,cellar,market,merchant,mine,moat,moneylender,poacher,remodel,witch]

silverAndGold :: [Card]
silverAndGold = [bandit,bureaucrat,chapel,harbinger,laboratory,merchant,mine,moneylender,throneRoom,vassal]

----------------------------------------

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
