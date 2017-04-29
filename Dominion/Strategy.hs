module Dominion.Strategy
    where

import Data.Monoid
import Debug.Trace

import Dominion.Rules

----------------------------------------

buyVPs :: GameState -> Action
buyVPs gs 
    = playFirst gs [copper,silver,gold,village,woodcutter] 
   <> buyFirst gs [province,duchy,estate,gold,silver,copper]

buyVillage :: GameState -> Action
buyVillage gs 
    = playMoney gs
   <> tryAction gs (Buy village)
   <> tryAction gs (Buy woodcutter)
   <> buyBestVP gs

buyWoodcutter :: GameState -> Action
buyWoodcutter gs 
    = playMoney gs
   <> tryAction gs (Buy woodcutter)
   <> tryAction gs (Buy village)
   <> buyBestVP gs

----------------------------------------

tryAction :: GameState -> Action -> Action
tryAction gs a = case doAction gs a of
    Nothing -> Pass
    Just _  -> a

doFirst :: GameState -> [Action] -> Action
doFirst gs = mconcat . map (tryAction gs)

buyFirst :: GameState -> [Card] -> Action
buyFirst gs = doFirst gs . map Buy

buyBestMoney :: GameState -> Action
buyBestMoney gs = buyFirst gs [gold,silver,copper]

buyBestVP :: GameState -> Action
buyBestVP gs = buyFirst gs [province,duchy,estate]

playFirst :: GameState -> [Card] -> Action
playFirst gs = doFirst gs . map Play

playMoney :: GameState -> Action
playMoney gs = playFirst gs [gold,silver,copper]
