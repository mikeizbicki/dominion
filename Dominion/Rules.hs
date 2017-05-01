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
data Card = Card
    { cardVPs       :: PlayerState -> Int
    , cardAction    :: GameState -> Maybe GameState
    , cardCost      :: Int
    , cardName      :: String
    }

instance Show Card where
    show = cardName

instance Eq Card where
    c1==c2 = (cardName c1)==(cardName c2)

defSupply :: [(Card,Int)]
defSupply =
    [ (copper,100)
    , (silver,100)
    , (gold,100)
    , (estate,10)
    , (duchy,10)
    , (province,10)
    , (village,10)
    , (woodcutter,10)
    , (smithy,10)
    , (councilRoom,10)
    , (festival,10)
    ]

copper :: Card
copper = Card
    { cardVPs = \_ -> 0
    , cardAction = actionAddMoney 1
    , cardCost = 0
    , cardName = "copper"
    }

silver :: Card
silver = Card
    { cardVPs = \_ -> 0
    , cardAction = actionAddMoney 2
    , cardCost = 3
    , cardName = "silver"
    }

gold :: Card
gold = Card
    { cardVPs = \_ -> 0
    , cardAction = actionAddMoney 3
    , cardCost = 6
    , cardName = "gold"
    }

estate :: Card
estate = Card
    { cardVPs = \_ -> 1
    , cardAction = actionNone
    , cardCost = 2
    , cardName = "estate" 
    }

duchy :: Card
duchy = Card
    { cardVPs = \_ -> 3
    , cardAction = actionNone
    , cardCost = 5
    , cardName = "duchy"
    } 

province :: Card
province = Card
    { cardVPs = \_ -> 6
    , cardAction = actionNone
    , cardCost = 8
    , cardName = "province"
    } 

village :: Card
village = Card
    { cardVPs = \_ -> 0
    , cardAction = do
        actionCost 1
        actionDrawCards 1
        actionAddActions 2
    , cardCost = 3
    , cardName = "village"
    } 

smithy :: Card
smithy = Card
    { cardVPs = \_ -> 0
    , cardAction = do
        actionCost 1
        actionDrawCards 3
    , cardCost = 3
    , cardName = "smithy"
    }

councilRoom :: Card
councilRoom = Card
    { cardVPs = \_ -> 0
    , cardAction = do
        actionCost 1
        actionDrawCards 3
        actionAllPlayersDrawCards 1
        actionAddBuys 1
    , cardCost = 5
    , cardName = "council room"
    }

festival :: Card
festival = Card
    { cardVPs = \_ -> 0
    , cardAction = do
        actionCost 1
        actionAddActions 2
        actionAddBuys 1
        actionAddMoney 2
    , cardCost = 5
    , cardName = "festival"
    }

woodcutter :: Card
woodcutter = Card
    { cardVPs = \_ -> 0
    , cardAction = do
        actionCost 1
        actionAddBuys 1
        actionAddMoney 2
    , cardCost = 3
    , cardName="woodcutter"
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

drawCard :: PlayerState -> PlayerState
drawCard ps = case deck ps of
    [] -> case discard ps of 
        [] -> ps
        _  -> drawCard $ ps
            { deck = deck'
            , discard = []
            , stdgen = sg2
            }
            where 
                (sg1,sg2) = split $ stdgen ps
                deck' = shuffle' (discard ps) (length $ discard ps) sg1
    (x:xs) -> ps
        { deck = tail $ deck ps
        , hand = head (deck ps) : hand ps
        }

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

numPlayers :: GameState -> Int
numPlayers gs = length $ playerStates gs

getCurrentPlayerState :: GameState -> PlayerState
getCurrentPlayerState gs = playerStates gs !! currentPlayer gs

setCurrentPlayerState :: GameState -> PlayerState -> GameState
setCurrentPlayerState gs ps = gs { playerStates = go (currentPlayer gs) (playerStates gs) }
    where
        go 0 (x:xs) = ps:xs
        go i (x:xs) = x:go (i-1) xs

updateCurrentPlayerState :: Monad m => (PlayerState -> m PlayerState) -> GameState -> m GameState
updateCurrentPlayerState f gs = do 
    ps <- f $ getCurrentPlayerState gs
    return $ setCurrentPlayerState gs ps

cardsInSupply :: GameState -> Card -> Int
cardsInSupply gs c = case lookup c $ supply gs of
    Nothing -> 0
    Just i -> i

drawCardFromSupply :: Card -> GameState -> GameState
drawCardFromSupply c gs = gs { supply = map go $ supply gs }
    where
        go (c',i) = if c==c' && i>0
            then (c',i-1)
            else (c',i)

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
    gs' <- updateCurrentPlayerState (putCardOnTable c) gs
    case cardAction c gs' of
        Just x -> Just x
doAction gs (Buy c) = if (cardCost c <= money ps) 
                      && (buys ps > 0) 
                      && (cardsInSupply gs c > 0)
    then Just $ drawCardFromSupply c
              $ setCurrentPlayerState gs 
              $ ps 
                { discard = c:discard ps 
                , money = money ps - cardCost c
                , buys = buys ps - 1
                }
    else Nothing
    where 
        ps = getCurrentPlayerState gs

putCardOnTable :: Card -> PlayerState -> Maybe PlayerState
putCardOnTable c ps = if c `elem` hand ps
    then Just ps
        { hand = delete c $ hand ps
        , played = c:played ps
        }
    else Nothing
