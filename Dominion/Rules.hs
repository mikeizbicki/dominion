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
    , cardAction    :: [Card] -> GameState -> Maybe GameState
    , cardAttack    :: PlayerID -> GameState -> GameState
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

--------------------

actionDrawCards :: Int -> [Card] -> GameState -> Maybe GameState
actionDrawCards n _ gs = go n $ Just gs
    where
        go 0 mgs = mgs
        go n mgs = case mgs of 
            Nothing -> Nothing
            Just gs -> go (n-1) $ return $ updatePlayerState CurrentPlayer drawCard gs

actionAllPlayersDrawCards :: Int -> [Card] -> GameState -> Maybe GameState
actionAllPlayersDrawCards n _ gs = Just $ go n gs
    where
        go 0 gs = gs
        go n gs = go (n-1) $ gs { playerStates = map drawCard $ playerStates gs }

actionAddActions :: Int -> [Card] -> GameState -> Maybe GameState
actionAddActions n _ = updatePlayerStateM CurrentPlayer $ \ps -> Just $ ps { actions = actions ps + n }

actionAddBuys :: Int -> [Card] -> GameState -> Maybe GameState
actionAddBuys n _ = updatePlayerStateM CurrentPlayer $ \ps -> if actions ps < n
    then Nothing
    else Just $ ps { buys = buys ps + n }

actionAddMoney :: Int -> [Card] -> GameState -> Maybe GameState
actionAddMoney n _ = updatePlayerStateM CurrentPlayer $ \ps -> Just $ ps {money = money ps + n }

actionNone :: [Card] -> GameState -> Maybe GameState
actionNone _ _ = Nothing

----------------------------------------

data PlayerState = PlayerState
    { deck              :: [Card]
    , hand              :: [Card]
    , played            :: [Card]
    , discard           :: [Card]
    , actions           :: Int
    , buys              :: Int
    , money             :: Int
    , turnsCompleted    :: Int
    , stdgen            :: StdGen 
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

drawCard :: PlayerState -> PlayerState
drawCard ps = ps' { hand = cs ++ hand ps' }
    where
        (cs,ps') = getNextCards 1 ps

getAllCards :: PlayerState -> [Card]
getAllCards ps = deck ps ++ hand ps ++ played ps ++ discard ps

resetTurn :: PlayerState -> PlayerState
resetTurn ps = drawCard $ drawCard $ drawCard $ drawCard $ drawCard $ PlayerState
    { deck = deck ps
    , hand = []
    , played = []
    , discard = hand ps ++ played ps ++ discard ps
    , actions = 1
    , buys = 1
    , money = 0
    , turnsCompleted = turnsCompleted ps + 1
    , stdgen = stdgen ps
    }

----------------------------------------

totalVPs :: GameState -> Int
totalVPs gs = sum (map (flip cardVPs ps) $ getAllCards ps)
    where
        ps = getCurrentPlayerState gs

vpDensity :: GameState -> Double
vpDensity gs = (fromIntegral $ totalVPs gs) / (genericLength $ getAllCards ps)
    where
        ps = getCurrentPlayerState gs

totalTreasure :: GameState -> Int
totalTreasure gs = sum (map go $ getAllCards ps)
    where
        ps = getCurrentPlayerState gs

        go c = if treasure $ cardType c
            then case cardAction c [] gs of
                Nothing -> 0
                Just gs' -> money (getCurrentPlayerState gs') - money (getCurrentPlayerState gs) 
            else 0

treasureDensity :: GameState -> Double
treasureDensity gs = (fromIntegral $ totalTreasure gs) / (genericLength $ getAllCards $ getCurrentPlayerState gs)

----------------------------------------

data GameState = GameState
    { playerStates  :: [PlayerState]
    , supply        :: [(Card,Int)]
    , currentPlayer :: PlayerID
    , currentRound  :: Int
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

updatePlayerState
    :: WhichPlayers
    -> (PlayerState -> PlayerState)
    -> (GameState   -> GameState  )
updatePlayerState w f gs = case updatePlayerStateM w (Just . f) gs of
    Just gs -> gs

updatePlayerStateM 
    :: WhichPlayers
    -> (PlayerState -> Maybe PlayerState)
    -> (GameState   -> Maybe GameState  )
updatePlayerStateM CurrentPlayer f gs = updatePlayerStateM (OnlyPlayer $ currentPlayer gs) f gs
updatePlayerStateM (OnlyPlayer i) f gs = do
    playerStates' <- forM (zip [0..] $ playerStates gs) $ \(i',ps') -> if i/=i'
        then return ps'
        else f ps'
    return $ gs { playerStates = playerStates' }

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
    = Play Card [Card]
    | Buy Card
    | Pass
    deriving (Show)

instance Monoid Action where
    Pass `mappend` x = x
    x    `mappend` _ = x
    mempty = Pass

doAction :: GameState -> Action -> Maybe GameState
doAction gs Pass = Nothing
doAction gs (Play c cs) = do
    gs <- updatePlayerStateM CurrentPlayer (\ps -> 
        if (action (cardType c) && actions ps <1)
            then Nothing
            else if action (cardType c)
                then Just $ ps { actions = actions ps -1 }
                else Just $ ps 
            ) gs
    gs <- updatePlayerStateM CurrentPlayer (putCardOnTable c) gs
    gs <- cardAction c cs gs
    gs <- Just $ foldr doReaction gs $ getNoncurrentPlayerIDs gs
    return gs
    where
        doReaction :: PlayerID -> GameState -> GameState
        doReaction i gs = case find (\c -> reaction $ cardType c) $ hand $ playerStates gs !! i of
            Just _  -> gs
            Nothing -> if attack $ cardType c 
                then cardAttack c i gs
                else gs
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

data Pile
    = Supply
    | Deck
    | Hand
    | Discard
    | Played
    | Trash

data WhichPlayers
    = CurrentPlayer
    | NoncurrentPlayers
    | AllPlayers 
    | OnlyPlayer PlayerID

-- | 
mvCard 
    :: WhichPlayers 
    -> Card                 -- ^ card to move 
    -> Pile                 -- ^ source pile 
    -> Pile                 -- ^ destination pile
    -> GameState 
    -> Maybe GameState
mvCard (OnlyPlayer pid) c p1 p2 gs = do
    let ps = playerStates gs !! pid

    supply' <- case p1 of
        Supply -> do
            n <- lookup c $ supply gs
            if n > 0
                then Just $ flip map (supply gs) $ \(c',n') -> if c==c'
                    then (c',n'-1)
                    else (c',n')
                else Nothing
        _ -> Just $ supply gs
       
    supply' <- case p2 of
        Supply -> error "cannot add cards to supply"
        _ -> return supply'

    deck' <- case p1 of
        Deck -> do
            find (==c) $ deck ps
            return $ delete c $ deck ps
        _ -> return $ deck ps

    deck' <- case p2 of
        Deck -> return $ c:deck ps
        _ -> return deck'

    hand' <- case p1 of
        Hand -> do
            find (==c) $ hand ps
            return $ delete c $ hand ps
        _ -> return $ hand ps

    hand' <- case p2 of
        Hand -> return $ c:hand'
        _ -> return hand'

    discard' <- case p1 of
        Discard -> do
            find (==c) $ discard ps
            return $ delete c $ discard ps
        _ -> return $ discard ps

    discard' <- case p2 of
        Discard -> return $ c:discard'
        _ -> return discard'

    played' <- case p1 of
        Played -> do
            find (==c) $ played ps
            return $ delete c $ played ps
        _ -> return $ played ps

    played' <- case p2 of
        Played -> return $ c:played'
        _ -> return played'

    return $ gs 
        { supply = supply'
        , playerStates = flip map (zip [0..] $ playerStates gs) $ \(i',ps') -> if pid==i'
            then ps'
                { deck = deck'
                , hand = hand'
                , discard = discard'
                , played = played'
                }
            else ps'
        }

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


