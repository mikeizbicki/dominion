module Dominion.Policy.MDP
    where

import Debug.Trace

import Dominion.Cards
import Dominion.Policy.Simple
import Dominion.Rules
import Engine.Monad

----------------------------------------

mdp :: Policy
mdp = defPolicy
    { policyName = "mdp"
--     , policyBuy = \gs -> trace ("valid buys: "++show (validBuys gs)) $ Buy []
    , policyBuy = policyBuy
    }
    where
        policyBuy :: GameState -> Buy
        policyBuy gs = case lookup maxUtil $ zip utilities options of
            Just b -> b
            where
                options = validBuys gs
                utilities = map go options
                maxUtil = maximum utilities

                go b = case resolveBuy b gs of
                    Just gs' -> utility gs'
                    Nothing -> utility gs

utility :: GameState -> Double
utility gs = fromIntegral $ totalVPs gs
    where
        ps = getCurrentPlayerState gs

simulateHand :: GameState -> GameState
simulateHand gs 
    = evalSim
    $ doAction playAllCards
    $ updatePlayerState CurrentPlayer cleanUpPhase gs
