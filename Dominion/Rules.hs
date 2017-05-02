module Dominion.Rules
    where

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Monoid
import System.Random.Shuffle
import Debug.Trace

----------------------------------------

-- |
--
-- Invariant: 
-- If any two instantiations of `Card` have the same `cardName`,
-- then all other attributes must also be the same.
-- There is no way to enforce this invariant within the type system.
--
-- FIXME:
-- The moat's reaction ability is hardcoded in the `doAction` function rather than appearing as a property here.
data Card = Card
    { cardVPs       :: PlayerState -> Int
    , cardAction    :: GameState -> Maybe GameState
    , cardAttack    :: Maybe (PlayerID -> GameState -> GameState)
    , cardCost      :: Int
    , cardName      :: String
    , cardType      :: CardType
    }

instance Show Card where
    show = cardName

instance Eq Card where
    c1==c2 = (cardName c1)==(cardName c2)

data CardType = CardType
    { action    :: Bool
    , reaction  :: Bool
    , treasure  :: Bool
    , attack    :: Bool
    , victory   :: Bool
    }

basicCardType :: CardType
basicCardType = CardType False False False False False

--------------------

basicCard :: Card
basicCard = Card
    { cardVPs = \_ -> 0
    , cardAction = \ps -> Just ps
    , cardAttack = Nothing
    , cardCost = error "card cost undefined"
    , cardName = error "card name undefined"
    , cardType = error "card type undefined"
    }

--------------------

basicCards :: [(Card,Int)]
basicCards = 
    [ (copper,100)
    , (silver,100)
    , (gold,100)
    , (estate,10)
    , (duchy,10)
    , (province,10)
    , (curse,40)
    ]

copper :: Card
copper = basicCard
    { cardAction = actionAddMoney 1
    , cardCost = 0
    , cardName = "copper"
    , cardType = basicCardType { treasure = True }
    }

silver :: Card
silver = basicCard
    { cardAction = actionAddMoney 2
    , cardCost = 3
    , cardName = "silver"
    , cardType = basicCardType { treasure = True }
    }

gold :: Card
gold = basicCard
    { cardAction = actionAddMoney 3
    , cardCost = 6
    , cardName = "gold"
    , cardType = basicCardType { treasure = True }
    }

curse :: Card
curse = basicCard
    { cardVPs = \_ -> -1
    , cardCost = 0
    , cardName = "curse"
    , cardType = basicCardType 
    }

estate :: Card
estate = basicCard
    { cardVPs = \_ -> 1
    , cardCost = 2
    , cardName = "estate" 
    , cardType = basicCardType { victory = True }
    }

duchy :: Card
duchy = basicCard
    { cardVPs = \_ -> 3
    , cardCost = 5
    , cardName = "duchy"
    , cardType = basicCardType { victory = True }
    } 

province :: Card
province = basicCard
    { cardVPs = \_ -> 6
    , cardCost = 8
    , cardName = "province"
    , cardType = basicCardType { victory = True }
    } 

--------------------

defSupply :: [(Card,Int)]
defSupply =
    basicCards
    ++
    [ (village,10)
    , (woodcutter,10)
    , (smithy,10)
    , (councilRoom,10)
    , (festival,10)
    , (witch,10)
    , (moat,10)
    , (bandit,10)
    , (witch,10)
    , (militia,10)
    ]

village :: Card
village = basicCard
    { cardAction = do
        actionCost 1
        actionDrawCards 1
        actionAddActions 2
    , cardCost = 3
    , cardName = "village"
    , cardType = basicCardType { action = True }
    } 

smithy :: Card
smithy = basicCard
    { cardAction = do
        actionCost 1
        actionDrawCards 3
    , cardCost = 3
    , cardName = "smithy"
    , cardType = basicCardType { action = True }
    }

councilRoom :: Card
councilRoom = basicCard
    { cardAction = do
        actionCost 1
        actionDrawCards 3
        actionAllPlayersDrawCards 1
        actionAddBuys 1
    , cardCost = 5
    , cardName = "council room"
    , cardType = basicCardType { action = True }
    }

festival :: Card
festival = basicCard
    { cardAction = do
        actionCost 1
        actionAddActions 2
        actionAddBuys 1
        actionAddMoney 2
    , cardCost = 5
    , cardName = "festival"
    , cardType = basicCardType { action = True }
    }
                
woodcutter :: Card
woodcutter = basicCard
    { cardAction = do
        actionCost 1
        actionAddBuys 1
        actionAddMoney 2
    , cardCost = 3
    , cardName = "woodcutter"
    , cardType = basicCardType { action = True }
    }

--------------------

moat :: Card
moat = basicCard
    { cardAction = do
        actionCost 1
        actionDrawCards 2
    , cardCost = 2
    , cardName = "moat"
    , cardType = basicCardType { action = True, reaction = True }
    }

-- | 
--
-- TODO:
-- Interestingly, the cardAttack makes this card worse against a big money deck.
-- I'm not sure if that's because it improves the opponents' decks cycling or if there's a bug somewhere.
bandit :: Card
bandit = basicCard
    { cardAction = \gs -> do
        gs <- actionCost 1 gs
        gs <- drawCardFromSupply gold gs
        updateCurrentPlayerState (\ps -> Just $ ps { discard = gold:discard ps }) gs
    , cardAttack = Just $ \i gs -> gs
        { playerStates = flip map (zip [0..] $ playerStates gs) $ \(i',ps) -> if i/=i'
            then ps
            else let
                (cs,ps') = getNextCards 2 ps
                cs' = if elem gold cs
                    then delete gold cs
                    else delete silver cs
                
                in ps' { discard = cs'++discard ps }
        }
    , cardCost = 5
    , cardName = "bandit"
    , cardType = basicCardType { action = True, attack = True }
    }

bureaucrat :: Card
bureaucrat = basicCard
    { cardAction = \gs -> do 
        gs <- actionCost 1 gs
        gs <- drawCardFromSupply silver gs
        updateCurrentPlayerState (\ps -> Just $ ps { deck = silver:deck ps }) gs
    , cardAttack = Just $ \i gs -> gs 
        { playerStates = flip map (zip [0..] $ playerStates gs) $ \(i',ps) -> if i/=i'
            then ps
            else case find (\c -> victory (cardType c) == True) $ hand ps of
                Nothing -> ps
                Just x -> ps
                    { deck = x:deck ps
                    , hand = delete x $ hand ps 
                    }
        }
    , cardCost = 4
    , cardName = "bureaucrat"
    , cardType = basicCardType { action = True, attack = True }
    }

-- | 
--
-- FIXME: 
-- The attack should trigger a discard down to 3 round that players can respond to in a custom way.
militia :: Card
militia = basicCard
    { cardAction = do
        actionCost 1
        actionAddMoney 2
    , cardAttack = Just $ \i gs -> gs
        { playerStates = flip map (zip [0..] $ playerStates gs) $ \(i',ps) -> if i/=i'
            then ps
            else discardDownTo3 ps
        }
    , cardCost = 4
    , cardName = "militia"
    , cardType = basicCardType { action = True, attack = True }
    }
    where
        discardDownTo3 :: PlayerState -> PlayerState
        discardDownTo3 ps = if length (hand ps) <= 3
            then ps
            else ps 
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
witch = basicCard
    { cardAction = do
        actionCost 1
        actionDrawCards 2
    , cardAttack = Just $ \i gs -> case drawCardFromSupply curse gs of
        Nothing -> gs
        Just gs' -> gs'
            { playerStates = flip map (zip [0..] $ playerStates gs') $ \(i',ps) -> if i/=i'
                then ps
                else ps { discard = curse : discard ps }
            }
    , cardCost = 5
    , cardName = "witch"
    , cardType = basicCardType { action = True, attack = True }
    }

--------------------

actionDrawCards :: Int -> GameState -> Maybe GameState
actionDrawCards n gs = go n $ Just gs
    where
        go 0 mgs = mgs
        go n mgs = case mgs of 
            Nothing -> Nothing
            Just gs -> go (n-1) $ updateCurrentPlayerState (\ps -> Just $ drawCard ps) gs

actionAllPlayersDrawCards :: Int -> GameState -> Maybe GameState
actionAllPlayersDrawCards n gs = Just $ go n gs
    where
        go 0 gs = gs
        go n gs = go (n-1) $ gs { playerStates = map drawCard $ playerStates gs }

actionAddActions :: Int -> GameState -> Maybe GameState
actionAddActions n = updateCurrentPlayerState $ \ps -> Just $ ps { actions = actions ps + n }

actionCost :: Int -> GameState -> Maybe GameState
actionCost n = updateCurrentPlayerState $ \ps -> if actions ps < n
    then Nothing
    else Just $ ps { actions = actions ps - n }

actionAddBuys :: Int -> GameState -> Maybe GameState
actionAddBuys n = updateCurrentPlayerState $ \ps -> if actions ps < n
    then Nothing
    else Just $ ps { buys = buys ps + n }

actionAddMoney :: Int -> GameState -> Maybe GameState
actionAddMoney n = updateCurrentPlayerState $ \ps -> Just $ ps {money = money ps + n }

actionNone :: GameState -> Maybe GameState
actionNone _ = Nothing

----------------------------------------

data PlayerState = PlayerState
    { deck :: [Card]
    , hand :: [Card]
    , played :: [Card]
    , discard :: [Card]
    , actions :: Int
    , buys :: Int
    , money :: Int
    , stdgen :: StdGen 
    }
    deriving (Show)

getNextCards :: Int -> PlayerState -> ([Card],PlayerState)
getNextCards 0 ps = ([],ps)
getNextCards n ps = case deck ps of
    [] -> case discard ps of
        [] -> ([],ps)
        _  -> getNextCards n $ ps
            { deck = deck'
            , discard = []
            , stdgen = sg2
            }
            where
                (sg1,sg2) = split $ stdgen ps
                deck' = shuffle' (discard ps) (length $ discard ps) sg1
    (c:cs) -> (c:cs',ps')
        where
            (cs',ps') = getNextCards (n-1) $ ps { deck = cs }

-- drawCard :: PlayerState -> PlayerState
-- drawCard ps = case deck ps of
--     [] -> case discard ps of 
--         [] -> ps
--         _  -> drawCard $ ps
--             { deck = deck'
--             , discard = []
--             , stdgen = sg2
--             }
--             where 
--                 (sg1,sg2) = split $ stdgen ps
--                 deck' = shuffle' (discard ps) (length $ discard ps) sg1
--     (x:xs) -> ps
--         { deck = tail $ deck ps
--         , hand = head (deck ps) : hand ps
--         }

drawCard :: PlayerState -> PlayerState
drawCard ps = ps' { hand = cs ++ hand ps' }
    where
        (cs,ps') = getNextCards 1 ps

getAllCards :: PlayerState -> [Card]
getAllCards ps = deck ps ++ hand ps ++ played ps ++ discard ps

initPlayerState :: StdGen -> PlayerState
initPlayerState stdgen = PlayerState
    { deck = replicate 7 copper ++ replicate 3 estate
    , hand = []
    , played = []
    , discard = []
    , actions = 0
    , buys = 0
    , money = 0
    , stdgen = stdgen
    }

resetTurn :: PlayerState -> PlayerState
resetTurn ps = drawCard $ drawCard $ drawCard $ drawCard $ drawCard $ PlayerState
    { deck = deck ps
    , hand = []
    , played = []
    , discard = hand ps ++ played ps ++ discard ps
    , actions = 1
    , buys = 1
    , money = 0
    , stdgen = stdgen ps
    }

----------------------------------------

data GameState = GameState
    { playerStates :: [PlayerState]
    , supply :: [(Card,Int)]
    , currentPlayer :: PlayerID
    }

type PlayerID = Int

getPlayerIDs :: GameState -> [PlayerID]
getPlayerIDs gs = [0..numPlayers gs-1]

getNoncurrentPlayerIDs :: GameState -> [PlayerID]
getNoncurrentPlayerIDs gs = delete (currentPlayer gs) $ getPlayerIDs gs

numPlayers :: GameState -> Int
numPlayers gs = length $ playerStates gs

getCurrentPlayerState :: GameState -> PlayerState
getCurrentPlayerState gs = playerStates gs !! currentPlayer gs

setCurrentPlayerState :: GameState -> PlayerState -> GameState
setCurrentPlayerState gs ps = gs { playerStates = go (currentPlayer gs) (playerStates gs) }
    where
        go 0 (x:xs) = ps:xs
        go i (x:xs) = x:go (i-1) xs

updateNoncurrentPlayerStates :: Monad m => (PlayerState -> m PlayerState) -> GameState -> m GameState
updateNoncurrentPlayerStates f gs = do
    playerStates' <- forM (zip [0..] $ playerStates gs) $ \(i,ps) -> do
        if i==currentPlayer gs
            then return ps
            else f ps
    return $ gs {playerStates = playerStates'}

updateCurrentPlayerState :: Monad m => (PlayerState -> m PlayerState) -> GameState -> m GameState
updateCurrentPlayerState f gs = do 
    ps <- f $ getCurrentPlayerState gs
    return $ setCurrentPlayerState gs ps

cardsInSupply :: GameState -> Card -> Int
cardsInSupply gs c = case lookup c $ supply gs of
    Nothing -> 0
    Just i -> i

----------------------------------------

data Score = Score
    { scoreVPs :: Int
    , scoreCards :: Int
    }
    deriving (Show,Eq)

instance Ord Score where
    compare s1 s2 = case compare (scoreVPs s1) (scoreVPs s2) of
        EQ -> compare (scoreCards s1) (scoreCards s2)
        x  -> x

getScore :: GameState -> PlayerID -> Score
getScore gs n = Score
    { scoreVPs = sum $ map (\c -> cardVPs c ps) $ getAllCards ps
    , scoreCards = length $ getAllCards ps
    }
    where
        ps = playerStates gs !! n

----------------------------------------

data Action
    = Play Card
    | Buy Card
    | Pass
    deriving (Show)

instance Monoid Action where
    Pass `mappend` x = x
    x    `mappend` _ = x
    mempty = Pass

doAction :: GameState -> Action -> Maybe GameState
doAction gs Pass = Nothing
doAction gs (Play c) = do
    gs <- updateCurrentPlayerState (putCardOnTable c) gs
    gs <- cardAction c gs
    gs <- Just $ foldr doReaction gs $ getNoncurrentPlayerIDs gs
    return gs
    where
        doReaction :: PlayerID -> GameState -> GameState
        doReaction i gs = if elem moat $ hand $ playerStates gs !! i
            then gs
            else case cardAttack c of
                Nothing -> gs
                Just f -> f i gs 
doAction gs (Buy c) = if (cardCost c <= money ps) 
                      && (buys ps > 0) 
    then drawCardFromSupply c
              $ setCurrentPlayerState gs 
              $ ps 
                { discard = c:discard ps 
                , money = money ps - cardCost c
                , buys = buys ps - 1
                }
    else Nothing
    where 
        ps = getCurrentPlayerState gs

drawCardFromSupply :: Card -> GameState -> Maybe GameState
drawCardFromSupply c gs = case lookup c $ supply gs of
    Nothing -> Nothing
    Just i -> if i>0
        then Just $ gs { supply = map go $ supply gs }
        else Nothing
    where
        go (c',i) = if c==c' && i>0
            then (c',i-1)
            else (c',i)

putCardOnTable :: Card -> PlayerState -> Maybe PlayerState
putCardOnTable c ps = if c `elem` hand ps
    then Just ps
        { hand = delete c $ hand ps
        , played = c:played ps
        }
    else Nothing
