module Dominion.Rules
    where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List
import Data.Monoid
import System.Random.Shuffle
import Debug.Trace

import Engine.Monad

----------------------------------------

-- |
--
-- Invariant: 
-- If any two instantiations of `Card` have the same `cardName`,
-- then all other attributes must also be the same.
-- There is no way to enforce this invariant within the type system.
--
-- FIXME:
-- The moat's reaction ability is hardcoded in the `resolveAction` function rather than appearing as a property here.
data Card = Card
    { cardVPs       :: PlayerState -> Int
    , cardAction    :: forall m. MonadRandom m => ActionParams -> GameState -> MaybeT m GameState
    , cardAttack    :: forall m. MonadRandom m => PlayerID -> GameState -> m GameState
    , cardCost      :: Int
    , cardName      :: String
    , cardType      :: CardType
    }

instance Show Card where
    show = cardName

instance Eq Card where
    c1==c2 = (cardName c1)==(cardName c2)

data ActionParams 
    = APNone
    | APList [Card]
    deriving (Show,Eq)

data CardType = CardType
    { action    :: Bool
    , reaction  :: Bool
    , treasure  :: Bool
    , attack    :: Bool
    , victory   :: Bool
    }

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
    }
    deriving (Show)

getAllCards :: PlayerState -> [Card]
getAllCards ps = deck ps ++ hand ps ++ played ps ++ discard ps

countNumCards :: PlayerState -> Card -> Int
countNumCards ps c = length $ filter (==c) $ getAllCards ps

getNextCards :: MonadRandom m => Int -> PlayerState -> m ([Card],PlayerState)
getNextCards 0 ps = return ([],ps)
getNextCards n ps = case deck ps of
    [] -> case discard ps of
        [] -> return ([],ps)
        _  -> do
            deck' <- shuffleM $ discard ps
            getNextCards n $ ps
                { deck = deck'
                , discard = []
                }
    (c:cs) -> do
        (cs',ps') <- getNextCards (n-1) $ ps { deck = cs }
        return (c:cs',ps')

drawCards :: MonadRandom m => Int -> PlayerState -> m PlayerState
drawCards n ps = do
    (cs,ps') <- getNextCards n ps
    return $ ps' { hand = cs ++ hand ps' }

-- getNextCards :: Int -> PlayerState -> ([Card],PlayerState)
-- getNextCards 0 ps = ([],ps)
-- getNextCards n ps = case deck ps of
--     [] -> case discard ps of
--         [] -> ([],ps)
--         _  -> getNextCards n $ ps
--             { deck = deck'
--             , discard = []
--             , stdgen = sg2
--             }
--             where
--                 (sg1,sg2) = split $ stdgen ps
--                 deck' = shuffle' (discard ps) (length $ discard ps) sg1
--     (c:cs) -> (c:cs',ps')
--         where
--             (cs',ps') = getNextCards (n-1) $ ps { deck = cs }
-- 
-- drawCards :: Int -> PlayerState -> PlayerState
-- drawCards n ps = ps' { hand = cs ++ hand ps' }
--     where
--         (cs,ps') = getNextCards n ps

cleanUpPhase :: MonadRandom m => PlayerState -> m PlayerState
cleanUpPhase ps = drawCards 5 $ PlayerState
    { deck = deck ps
    , hand = []
    , played = []
    , discard = hand ps ++ played ps ++ discard ps
    , actions = 1
    , buys = 1
    , money = 0
    , turnsCompleted = turnsCompleted ps + 1
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

-- totalTreasure :: GameState -> Int
-- totalTreasure gs = sum (map go $ getAllCards ps)
--     where
--         ps = getCurrentPlayerState gs
-- 
--         go c = if treasure $ cardType c
--             then case cardAction c APNone gs of
--                 Nothing -> 0
--                 Just gs' -> money (getCurrentPlayerState gs') - money (getCurrentPlayerState gs) 
--             else 0
-- 
-- treasureDensity :: GameState -> Double
-- treasureDensity gs = (fromIntegral $ totalTreasure gs) / (genericLength $ getAllCards $ getCurrentPlayerState gs)

----------------------------------------

newtype GameStateM a = GameStateM 
    ( MaybeT
        ( RandT StdGen
            ( State GameState
            )
        )
      a
    )
    deriving (Functor,Applicative,Monad,MonadRandom,MonadState GameState)

-- runGameStateM :: GameState -> GameStateM () -> Sim (Maybe GameState)
-- runGameStateM gs (GameStateM m) = do
--     sg <- getSplit
--     m' <- runMaybeT _m
--     case m' of 
--         Nothing -> return Nothing
--         Just x -> return Nothing
--     return Nothing

----------------------------------------

data GameConfig = GameConfig
    { players :: [Policy]
    }

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
    :: Monad m
    => WhichPlayers
    -> (PlayerState -> m PlayerState)
    -> (GameState   -> m GameState  )
updatePlayerStateM CurrentPlayer f gs = updatePlayerStateM (OnlyPlayer $ currentPlayer gs) f gs
updatePlayerStateM (OnlyPlayer i) f gs = do
    playerStates' <- forM (zip [0..] $ playerStates gs) $ \(i',ps') -> if i/=i'
        then return ps'
        else f ps'
    return $ gs { playerStates = playerStates' }

updatePSM   
    :: WhichPlayers
    -> (PlayerState -> GameStateM PlayerState )
    -> GameStateM ()
updatePSM CurrentPlayer f = do
    gs <- get
    updatePSM (OnlyPlayer $ currentPlayer gs) f 
updatePSM (OnlyPlayer i) f = do
    gs <- get
    playerStates' <- forM (zip [0..] $ playerStates gs) $ \(i',ps') -> if i/=i'
        then return ps'
        else f ps'
    put $ gs { playerStates = playerStates' }

cardsInSupply :: GameState -> Card -> Int
cardsInSupply gs c = case lookup c $ supply gs of
    Nothing -> 0
    Just i -> i

getWinner :: GameState -> PlayerID
getWinner gs = head $ elemIndices maxScore scores
    where
        maxScore :: Score
        maxScore = maximum scores

        scores :: [Score]
        scores = map (getScore gs) $ getPlayerIDs gs

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

data Policy = Policy
    { policyName    :: String
    , policyAction  :: GameState -> Action
    , policyBuy     :: GameState -> Buy
    }

instance Show Policy where
    show = policyName

instance Eq Policy where
    s1==s2 = policyName s1==policyName s2

cpPolicy :: Policy -> Policy
cpPolicy s = s { policyName = policyName s ++ "X" }

----------------------------------------

newtype Buy = Buy [Card]
    deriving (Show,Eq)

instance Monoid Buy where
    (Buy []) `mappend` x = x
    x        `mappend` _ = x
    mempty               = Buy []

resolveBuy :: Buy -> GameState -> Maybe GameState
resolveBuy (Buy []) gs = Nothing
resolveBuy (Buy cs) gs = go cs gs
    where
        go [] gs = Just gs
        go (c:cs) gs = if cardCost c <= money ps && buys ps > 0
            then do
                gs <- drawCardFromSupply c gs
                gs <- return $ setCurrentPlayerState gs $ ps
                    { discard = c:discard ps
                    , money = money ps - cardCost c
                    , buys = buys ps - 1
                    }
                go cs gs
            else Nothing
            where
                ps = getCurrentPlayerState gs

doBuy :: (GameState -> Buy) -> GameState -> Sim GameState
doBuy getBuy gs = case resolveBuy cmdbuy gs of
    Nothing -> do
        writeMsg Turn $ "  "++ "attempted to play: "++show cmdbuy
        return gs
    Just gs' -> do
        let ps' = getCurrentPlayerState gs'
        writeMsg Turn $ "  "++ padRight 30 (show cmdbuy) 
                            ++ "buys:"++show (buys ps')
                            ++ "; money: "++show (money ps')
                            ++ "; actions: "++show (actions ps')
        doBuy getBuy gs'
    where
        cmdbuy :: Buy
        cmdbuy = getBuy gs

validBuys :: GameState -> [Buy]
validBuys gs = Buy [] : if buys ps == 0
    then []
    else map (\x -> Buy [fst x]) $ filter go $ supply gs
    where
        go (c,0) = False
        go (c,_) = cardCost c <= money ps

        ps = getCurrentPlayerState gs

----------------------------------------

data Action
    = Play Card ActionParams
    | Pass
    deriving (Show)

instance Monoid Action where
    Pass `mappend` x = x
    x    `mappend` _ = x
    mempty = Pass

resolveAction :: MonadRandom m => Action -> GameState -> MaybeT m GameState
resolveAction Pass        gs = fail "pass"
resolveAction (Play c cs) gs = do
    gs <- updatePlayerStateM CurrentPlayer (\ps -> 
        if (action (cardType c) && actions ps <1)
            then fail "insufficient actions"
            else if action (cardType c)
                then return $ ps { actions = actions ps - 1 }
                else return $ ps 
            ) gs
    gs <- updatePlayerStateM CurrentPlayer (putCardOnTable c) gs
    gs <- cardAction c cs gs
    gs <- foldM doReaction gs $ getNoncurrentPlayerIDs gs
    return gs
    where
        doReaction :: MonadRandom m => GameState -> PlayerID -> m GameState
        doReaction gs i = case find (\c -> reaction $ cardType c) $ hand $ playerStates gs !! i of
            Just _  -> return gs
            Nothing -> if attack $ cardType c 
                then cardAttack c i gs
                else return gs


        putCardOnTable :: Monad m => Card -> PlayerState -> MaybeT m PlayerState
        putCardOnTable c ps = if c `elem` hand ps
            then return $ ps
                { hand = delete c $ hand ps
                , played = c:played ps
                }
            else fail "card not in hand"

doAction :: (GameState -> Action) -> GameState -> Sim GameState
doAction getAction gs = do
    gs' <- runMaybeT $ resolveAction action gs  
    case gs' of
        Nothing -> return gs
        Just gs' -> do
            let ps' = getCurrentPlayerState gs'
            writeMsg Turn $ "  "++ padRight 30 (show action) 
                                ++ "buys:"++show (buys ps')
                                ++ "; money: "++show (money ps')
                                ++ "; actions: "++show (actions ps')
            doAction getAction gs'
    where
        action :: Action
        action = getAction gs

padRight n xs = xs ++ replicate (n-length xs) ' ' 

----------------------------------------

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

