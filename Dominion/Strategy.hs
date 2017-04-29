module Dominion.Strategy
    where

import Dominion.Rules

----------------------------------------

buyVillage :: GameState -> Action
buyVillage gs = case selectCard gs of
    Just a -> Play a
    Nothing -> go $ map (\a->(a,doAction gs a)) [Buy village,Buy woodcutter,Buy estate]
        where
            go :: [(Action,Maybe GameState)] -> Action
            go ((a,Just _):xs) = a
            go (_         :xs) = go xs
            go []              = Stop

buyWoodcutter :: GameState -> Action
buyWoodcutter gs = case selectCard gs of
    Just a -> Play a
    Nothing -> go $ map (\a->(a,doAction gs a)) [Buy woodcutter,Buy village,Buy estate]
        where
            go :: [(Action,Maybe GameState)] -> Action
            go ((a,Just _):xs) = a
            go (_         :xs) = go xs
            go []              = Stop

buyVPs :: GameState -> Action
buyVPs gs = case selectCard gs of
    Just a -> Play a
    Nothing -> go $ map (\a->(a,doAction gs a)) [Buy estate]
        where
            go :: [(Action,Maybe GameState)] -> Action
            go ((a,Just _):xs) = a
            go (_         :xs) = go xs
            go []              = Stop

selectCard :: GameState -> Maybe CardID
selectCard gs = go $ hand $ getCurrentPlayerState gs
    where
        go (x:xs) = case idAction x gs of
            Just ps' -> Just x
            Nothing -> go xs
        go [] = Nothing

