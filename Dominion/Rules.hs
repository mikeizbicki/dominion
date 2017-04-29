module Dominion.Rules
    where

import Data.List
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle
import Debug.Trace

----------------------------------------

data CardInfo = CardInfo
    { cardVPs :: PlayerState -> Int
    , cardAction :: GameState -> Maybe GameState
    , cardCost :: Int
    }

newtype CardID = CardID String
    deriving (Read,Eq,Ord)

instance Show CardID where show (CardID c) = c

cards :: [(CardID,CardInfo)]
cards =
    [ ( CardID "copper", CardInfo
        { cardVPs = \_ -> 0
        , cardAction = updateCurrentPlayerState $ \ps -> Just $ ps { money = money ps + 1 }
        , cardCost = 0
        } )
    , ( CardID "silver", CardInfo
        { cardVPs = \_ -> 0
        , cardAction = updateCurrentPlayerState $ \ps -> Just $ ps { money = money ps + 2 }
        , cardCost = 3
        } )
    , ( CardID "gold", CardInfo
        { cardVPs = \_ -> 0
        , cardAction = updateCurrentPlayerState $ \ps -> Just $ ps { money = money ps + 3 }
        , cardCost = 6
        } )
    , ( CardID "estate", CardInfo
        { cardVPs = \_ -> 1
        , cardAction = \_ -> Nothing
        , cardCost = 2
        } )
    , ( CardID "duchy", CardInfo
        { cardVPs = \_ -> 3
        , cardAction = \_ -> Nothing
        , cardCost = 5
        } )
    , ( CardID "province", CardInfo
        { cardVPs = \_ -> 6
        , cardAction = \_ -> Nothing
        , cardCost = 8
        } )
    , ( CardID "village", CardInfo
        { cardVPs = \_ -> 0
        , cardAction = updateCurrentPlayerState $ \ps -> if actions ps == 0
            then Nothing
            else Just $ drawCard $ ps
                { actions = actions ps + 1 }
        , cardCost = 3
        } )
    , ( CardID "woodcutter", CardInfo
        { cardVPs = \_ -> 0
        , cardAction = updateCurrentPlayerState $ \ps -> if actions ps == 0
            then Nothing
            else Just $ ps
                { buys = buys ps + 1
                , money = money ps + 2
                , actions = actions ps - 1
                }
        , cardCost = 3
        } )
    ]

copper :: CardID
copper = CardID "copper"

estate :: CardID
estate = CardID "estate"

province :: CardID
province = CardID "province"

village :: CardID
village = CardID "village"

woodcutter :: CardID
woodcutter = CardID "woodcutter"

lookupCardInfo :: CardID -> CardInfo
lookupCardInfo id = case lookup id cards of
    Just x -> x 

idVPs :: CardID -> PlayerState -> Int
idVPs = cardVPs . lookupCardInfo 

idAction :: CardID -> GameState -> Maybe GameState
idAction = cardAction . lookupCardInfo 

idCost :: CardID -> Int
idCost = cardCost . lookupCardInfo 

----------------------------------------

data PlayerState = PlayerState
    { deck :: [CardID]
    , hand :: [CardID]
    , played :: [CardID]
    , discard :: [CardID]
    , actions :: Int
    , buys :: Int
    , money :: Int
    , stdgen :: StdGen 
    }
    deriving (Show)

drawCard :: PlayerState -> PlayerState
drawCard ps = case deck ps of
    [] -> drawCard $ ps
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

getAllCards :: PlayerState -> [CardID]
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
    , supply :: [(CardID,Int)]
    , currentPlayer :: Int 
    }
    deriving (Show)

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

cardsInSupply :: GameState -> CardID -> Int
cardsInSupply gs c = case lookup c $ supply gs of
    Nothing -> 0
    Just i -> i

drawCardFromSupply :: CardID -> GameState -> GameState
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
    deriving (Read,Show,Eq)

instance Ord Score where
    compare s1 s2 = case compare (scoreVPs s1) (scoreVPs s2) of
        EQ -> compare (scoreCards s1) (scoreCards s2)
        x  -> x

getScore :: PlayerState -> Score
getScore ps = Score
    { scoreVPs = sum $ map (flip idVPs ps) $ getAllCards ps
    , scoreCards = length $ getAllCards ps
    }

----------------------------------------

data Action
    = Play CardID
    | Buy CardID
    | Stop
    deriving (Read,Show)

doAction :: GameState -> Action -> Maybe GameState
doAction gs Stop = Nothing
doAction gs (Play c) = do
    gs' <- updateCurrentPlayerState (putCardOnTable c) gs
    idAction c gs'
doAction gs (Buy c) = if (idCost c <= money ps) && (buys ps > 0) && (cardsInSupply gs c > 0)
    then Just $ drawCardFromSupply c
              $ setCurrentPlayerState gs 
              $ ps 
                { discard = c:discard ps 
                , money = money ps - idCost c
                , buys = buys ps - 1
                }
    else Nothing
    where 
        ps = getCurrentPlayerState gs

putCardOnTable :: CardID -> PlayerState -> Maybe PlayerState
putCardOnTable c ps = if c `elem` hand ps
    then Just ps
        { hand = delete c $ hand ps
        , played = c:played ps
        }
    else Nothing
