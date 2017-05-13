module Dominion.Cards
    where

import Control.Monad
import Data.List

import Dominion.Rules

----------------------------------------

defCard :: Card
defCard = Card
    { cardVPs = \_ -> 0
    , cardAction = \_ gs -> Just gs
    , cardAttack = error "card attack undefined"
    , cardCost = error "card cost undefined"
    , cardName = error "card name undefined"
    , cardType = error "card type undefined"
    }

defCardType :: CardType
defCardType = CardType False False False False False

----------------------------------------

copper :: Card
copper = defCard
    { cardAction = actionAddMoney 1
    , cardCost = 0
    , cardName = "copper"
    , cardType = defCardType { treasure = True }
    }

silver :: Card
silver = defCard
    { cardAction = actionAddMoney 2
    , cardCost = 3
    , cardName = "silver"
    , cardType = defCardType { treasure = True }
    }

gold :: Card
gold = defCard
    { cardAction = actionAddMoney 3
    , cardCost = 6
    , cardName = "gold"
    , cardType = defCardType { treasure = True }
    }

curse :: Card
curse = defCard
    { cardVPs = \_ -> -1
    , cardCost = 0
    , cardName = "curse"
    , cardType = defCardType 
    }

estate :: Card
estate = defCard
    { cardVPs = \_ -> 1
    , cardCost = 2
    , cardName = "estate" 
    , cardType = defCardType { victory = True }
    }

duchy :: Card
duchy = defCard
    { cardVPs = \_ -> 3
    , cardCost = 5
    , cardName = "duchy"
    , cardType = defCardType { victory = True }
    } 

province :: Card
province = defCard
    { cardVPs = \_ -> 6
    , cardCost = 8
    , cardName = "province"
    , cardType = defCardType { victory = True }
    } 

gardens :: Card
gardens = defCard
    { cardVPs = \ps -> (length $ getAllCards ps) `div` 10
    , cardCost = 4
    , cardName = "gardens"
    , cardType = defCardType { victory = True }
    }

----------------------------------------

cellar :: Card
cellar = defCard
    { cardAction = \cs gs -> do
        gs <- actionAddActions 1 cs gs
        gs <- actionDiscardCards cs gs
        return gs
    , cardCost = 2
    , cardName = "cellar"
    , cardType = defCardType { action = True }
    }
    where
        actionDiscardCards :: [Card] -> GameState -> Maybe GameState
        actionDiscardCards []     gs = Just gs
        actionDiscardCards (c:cs) gs = do
            gs <- updatePlayerStateM CurrentPlayer discardCard gs
            gs <- actionDrawCards 1 [] gs
            return gs
            where
                discardCard :: PlayerState -> Maybe PlayerState
                discardCard ps = case find (==c) $ hand ps of
                    Nothing -> Nothing
                    Just _  -> Just $ ps 
                        { hand = delete c $ hand ps
                        , discard = c : discard ps
                        }

chapel :: Card
chapel = defCard
    { cardAction = actionTrashCards
    , cardCost = 2
    , cardName = "chapel"
    , cardType = defCardType { action = True }
    }
    where
        actionTrashCards :: [Card] -> GameState -> Maybe GameState
        actionTrashCards []     gs = Just gs
        actionTrashCards (c:cs) gs = updatePlayerStateM CurrentPlayer trashCard gs
            where
                trashCard :: PlayerState -> Maybe PlayerState
                trashCard ps = case find (==c) $ hand ps of
                    Nothing -> Nothing
                    Just _  -> Just $ ps { hand = delete c $ hand ps }

harbinger :: Card
harbinger = defCard
    { cardAction = \cs gs -> do
        gs <- actionDrawCards 1 cs gs
        gs <- actionAddActions 1 cs gs
        gs <- actionDiscardToDeck cs gs
        return gs
    , cardCost = 3
    , cardName = "harbinger"
    , cardType = defCardType { action = True }
    }
    where
        actionDiscardToDeck :: [Card] -> GameState -> Maybe GameState
        actionDiscardToDeck [ ] gs = Just gs
        actionDiscardToDeck [c] gs = updatePlayerStateM CurrentPlayer discard2deck gs
            where
                discard2deck ps = case find (==c) $ discard ps of
                    Nothing -> Nothing
                    Just _  -> Just $ ps 
                        { discard = delete c $ discard ps
                        , deck = c : deck ps
                        }
        actionDiscardToDeck _   _  = Nothing

vassal :: Card
vassal = defCard
    { cardAction = \cs gs -> do
        gs <- actionAddMoney 2 cs gs
        gs <- actionPlayTopCard cs gs
        return gs
    , cardCost = 3
    , cardName = "vassal"
    , cardType = defCardType { action = True }
    }
    where
        actionPlayTopCard :: [Card] -> GameState -> Maybe GameState
        actionPlayTopCard _ gs = updatePlayerStateM CurrentPlayer action2hand gs
            where
                action2hand ps = case getNextCards 1 ps of
                    ([c],ps') -> if action $ cardType c
                        then Just $ ps' { hand = c : hand ps' }
                        else Just $ ps' { discard = c : discard ps' }
                    _         -> Nothing

workshop :: Card
workshop = defCard
    { cardAction = actionGainCardUpTo 4
    , cardCost = 3
    , cardName = "workshop"
    , cardType = defCardType { action = True }
    }
    where
        actionGainCardUpTo :: Int -> [Card] -> GameState -> Maybe GameState
        actionGainCardUpTo n [c] gs = if cardCost c > n
            then Nothing
            else do
                gs' <- drawCardFromSupply c gs
                return $ updatePlayerState CurrentPlayer (\ps -> ps {hand = c:hand ps}) gs'
        actionGainCardUpTo _ _   gs = Nothing


village :: Card
village = defCard
    { cardAction = \cs gs -> do
        gs <- actionDrawCards 1 cs gs
        gs <- actionAddActions 2 cs gs
        return gs
    , cardCost = 3
    , cardName = "village"
    , cardType = defCardType { action = True }
    } 

smithy :: Card
smithy = defCard
    { cardAction = actionDrawCards 3
    , cardCost = 4
    , cardName = "smithy"
    , cardType = defCardType { action = True }
    }

councilRoom :: Card
councilRoom = defCard
    { cardAction = \cs gs -> do
        gs <- actionDrawCards 3 cs gs
        gs <- actionAllPlayersDrawCards 1 cs gs
        gs <- actionAddBuys 1 cs gs
        return gs
    , cardCost = 5
    , cardName = "council room"
    , cardType = defCardType { action = True }
    }

festival :: Card
festival = defCard
    { cardAction = \cs gs -> do
        gs <- actionAddActions 2 cs gs 
        gs <- actionAddBuys 1 cs gs 
        gs <- actionAddMoney 2 cs gs 
        return gs
    , cardCost = 5
    , cardName = "festival"
    , cardType = defCardType { action = True }
    }
                
market :: Card
market = defCard
    { cardAction = \cs gs -> do
        gs <- actionAddActions 1 cs gs
        gs <- actionAddBuys 1 cs gs
        gs <- actionAddMoney 1 cs gs
        gs <- actionDrawCards 1 cs gs
        return gs
    , cardCost = 5
    , cardName = "market"
    , cardType = defCardType { action = True }
    }

mine :: Card
mine = defCard
    { cardAction = exchangeTreasure
    , cardCost = 4
    , cardName = "mine"
    , cardType = defCardType { action = True }
    }
    where
        exchangeTreasure :: [Card] -> GameState -> Maybe GameState
        exchangeTreasure [c] gs = do
            c' <- if c==copper
                then return silver
                else if c==silver
                    then return gold
                    else Nothing
            gs' <- drawCardFromSupply c' gs
            updatePlayerStateM CurrentPlayer (\ps -> do
                find (==c) $ hand ps
                return $ ps { hand = c': delete c (hand ps) }
                ) gs
        exchangeTreasure _   gs = Nothing

remodel :: Card
remodel = defCard 
    { cardAction = exchangeCards
    , cardCost = 4
    , cardName = "remodel"
    , cardType = defCardType { action = True }
    }
    where
        exchangeCards :: [Card] -> GameState -> Maybe GameState
        exchangeCards [c1,c2] gs = do
            when (cardCost c2 > cardCost c1+2) Nothing
            gs' <- drawCardFromSupply c2 gs
            updatePlayerStateM CurrentPlayer (\ps -> do
                find (==c1) $ hand ps
                return $ ps 
                    { hand = delete c1 (hand ps) 
                    , discard = c2: discard ps
                    }
                ) gs

woodcutter :: Card
woodcutter = defCard
    { cardAction = \cs gs -> do
        gs <- actionAddBuys 1 cs gs 
        gs <- actionAddMoney 2 cs gs
        return gs
    , cardCost = 3
    , cardName = "woodcutter"
    , cardType = defCardType { action = True }
    }

laboratory :: Card
laboratory = defCard
    { cardAction = \cs gs -> do
        gs <- actionDrawCards 2 cs gs
        gs <- actionAddActions 1 cs gs
        return gs
    , cardCost = 5
    , cardName = "laboratory"
    , cardType = defCardType { action = True }
    }

moneylender :: Card
moneylender = defCard
    { cardAction = \cs gs ->
        if elem copper $ hand $ getCurrentPlayerState gs
            then do
                gs <- return $ updatePlayerState CurrentPlayer (\ps -> ps { hand = delete copper $ hand ps }) gs
                gs <- actionAddMoney 3 cs gs
                return gs
            else return gs
    , cardCost = 4
    , cardName = "moneylender"
    , cardType = defCardType { action = True }
    }
 
merchant :: Card
merchant = defCard
    { cardAction = \cs gs -> do
        gs <- actionDrawCards 1 cs gs
        gs <- actionAddActions 1 cs gs
        gs <- if elem silver $ played $ getCurrentPlayerState gs
            then actionAddMoney 1 cs gs
            else return gs
        return gs
    , cardCost = 3
    , cardName = "merchant"
    , cardType = defCardType { action = True }
    }

artisan :: Card
artisan = defCard
    { cardAction = artisanAction
    , cardCost = 6
    , cardName = "artisan"
    , cardType = defCardType { action = True }
    }
    where
        artisanAction (c1:c2:[]) gs = do
            gs <- drawCardFromSupply c1 gs
            _ <- find (==c2) $ hand $ getCurrentPlayerState gs
            let f ps = ps
                    { hand = c1:delete c2 (hand ps)
                    , deck = c2:deck ps
                    }
            gs <- return $ updatePlayerState CurrentPlayer f gs
            return gs
        artisanAction _ _ = Nothing

poacher :: Card
poacher = defCard { cardAction = undefined }

throneRoom :: Card
throneRoom = defCard { cardAction = undefined }

sentry :: Card
sentry = defCard { cardAction = undefined }

library :: Card
library = defCard { cardAction = undefined }

--------------------

moat :: Card
moat = defCard
    { cardAction = actionDrawCards 2
    , cardCost = 2
    , cardName = "moat"
    , cardType = defCardType { action = True, reaction = True }
    }

-- | 
--
-- TODO:
-- Interestingly, the cardAttack makes this card worse against a big money deck.
-- I'm not sure if that's because it improves the opponents' decks cycling or if there's a bug somewhere.
--
-- FIXME: 
-- The player should be able to choose which card to delete.
bandit :: Card
bandit = defCard
    { cardAction = \cs gs -> do
        gs <- drawCardFromSupply gold gs
        return $ updatePlayerState CurrentPlayer (\ps -> ps { discard = gold:discard ps }) gs
    , cardAttack = \i gs -> updatePlayerState (OnlyPlayer i) trashGoodMoney gs
    , cardCost = 5
    , cardName = "bandit"
    , cardType = defCardType { action = True, attack = True }
    }
    where
        trashGoodMoney :: PlayerState -> PlayerState
        trashGoodMoney ps = ps' { discard = cs'++discard ps }
            where
                (cs,ps') = getNextCards 2 ps
                cs' = if elem gold cs
                    then delete gold cs
                    else delete silver cs

bureaucrat :: Card
bureaucrat = defCard
    { cardAction = \cs gs -> do 
        gs <- drawCardFromSupply silver gs
        return $ updatePlayerState CurrentPlayer (\ps -> ps { deck = silver:deck ps }) gs
    , cardAttack = \i gs -> updatePlayerState (OnlyPlayer i) mvVPHandToDeck gs 
    , cardCost = 4
    , cardName = "bureaucrat"
    , cardType = defCardType { action = True, attack = True }
    }
    where
        mvVPHandToDeck :: PlayerState -> PlayerState
        mvVPHandToDeck ps = case find (\c -> victory (cardType c) == True) $ hand ps of
            Nothing -> ps
            Just x -> ps
                { deck = x:deck ps
                , hand = delete x $ hand ps 
                }

-- | 
--
-- FIXME: 
-- The attack should trigger a discard down to 3 round that players can respond to in a custom way.
militia :: Card
militia = defCard
    { cardAction = actionAddMoney 2
    , cardAttack = \i gs -> updatePlayerState (OnlyPlayer i) discardDownTo3 gs
    , cardCost = 4
    , cardName = "militia"
    , cardType = defCardType { action = True, attack = True }
    }
    where
        discardDownTo3 :: PlayerState -> PlayerState
        discardDownTo3 ps = if length (hand ps) <= 3
            then ps
            else discardDownTo3 $ ps 
                { discard = worstCard : discard ps
                , hand = delete worstCard $ hand ps
                }
            where
                worstCard = getWorstCard $ hand ps

                getWorstCard :: [Card] -> Card
                getWorstCard cs = case find (victory . cardType) cs of
                    Just c -> c
                    Nothing -> head $ sortBy (\c1 c2 -> compare (cardCost c1) (cardCost c2)) cs

witch :: Card
witch = defCard
    { cardAction = actionDrawCards 2
    , cardAttack = \i gs -> case drawCardFromSupply curse gs of
        Nothing -> gs
        Just gs' -> updatePlayerState (OnlyPlayer i) (\ps -> ps { discard = curse:discard ps }) gs'
    , cardCost = 5
    , cardName = "witch"
    , cardType = defCardType { action = True, attack = True }
    }

--------------------

actionDrawCards :: Int -> [Card] -> GameState -> Maybe GameState
actionDrawCards n _ gs = return $ updatePlayerState CurrentPlayer (drawCards n) gs

actionAllPlayersDrawCards :: Int -> [Card] -> GameState -> Maybe GameState
actionAllPlayersDrawCards n _ gs = return $ gs { playerStates = map (drawCards n) $ playerStates gs }

actionAddActions :: Int -> [Card] -> GameState -> Maybe GameState
actionAddActions n _ gs = return $ updatePlayerState CurrentPlayer addActions gs
    where
        addActions ps = ps { actions = actions ps + n }

actionAddBuys :: Int -> [Card] -> GameState -> Maybe GameState
actionAddBuys n _ = updatePlayerStateM CurrentPlayer $ \ps -> Just $ ps { buys = buys ps + n }

actionAddMoney :: Int -> [Card] -> GameState -> Maybe GameState
actionAddMoney n _ = updatePlayerStateM CurrentPlayer $ \ps -> Just $ ps {money = money ps + n }

actionNone :: [Card] -> GameState -> Maybe GameState
actionNone _ _ = Nothing

