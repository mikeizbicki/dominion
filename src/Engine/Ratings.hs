module Engine.Ratings
    where

import Control.Exception
import Control.Monad
import Data.List
import Numeric

import Dominion.Rules

----------------------------------------

type PlayerName = String

newtype Ratings = Ratings [(PlayerName,Rating)]
    deriving (Read,Show)

printRatings :: Ratings -> IO ()
printRatings (Ratings rs) = do
    putStrLn "ratings: "
    forM_ (take 50 $ sortBy (\(_,r1) (_,r2) -> compare (eloWin r2) (eloWin r1)) rs) $ \(p,r) -> do
        let dispname = take 40 $ p++repeat ' '
        putStrLn $ "  "++dispname 
                       ++" eloW: "  ++ (frontPad 5 $ showFFloat (Just 2) (score $ eloWin r) "")
                       ++" eloS: "  ++ (frontPad 5 $ showFFloat (Just 2) (score $ eloScore r) "")
                       ++" wins: "  ++ (frontPad 4 $ show (wins r))
                       ++" losses: "++ (frontPad 4 $ show (losses r))
                       ++" win%: "  ++ (frontPad 4 $ showFFloat (Just 2) (winPercent r) "")
    where
        frontPad n xs = replicate (n-length xs) ' ' ++ xs

loadRatings :: FilePath -> IO Ratings
loadRatings fn = catch (fmap read $ readFile fn) $ \(SomeException _) -> return (Ratings [])
    
saveRatings :: FilePath -> Ratings -> IO ()
saveRatings fn rs = writeFile fn $ show rs
    
----------------------------------------

data Rating = Rating
    { eloWin    :: Elo
    , eloScore  :: Elo
    , wins      :: Int
    , losses    :: Int
    }
    deriving (Read,Show,Eq,Ord)

numGames :: Rating -> Int
numGames r = wins r + losses r

winPercent :: Rating -> Double
winPercent r = fromIntegral (wins r) / fromIntegral (wins r + losses r)

unrated :: Rating
unrated = Rating
    { eloWin = defElo
    , eloScore = defElo
    , wins = 0
    , losses = 0
    }

getRating :: PlayerName -> Ratings -> Rating
getRating p (Ratings rs) = case lookup p rs of
    Nothing -> unrated
    Just r  -> r

setRating :: (PlayerName,Rating) -> Ratings -> Ratings
setRating (p,r') (Ratings rs) = Ratings $ sort $ (p,r'):delete (p,r) rs
    where
        r = getRating p (Ratings rs)

setRatings :: [(PlayerName,Rating)] -> Ratings -> Ratings
setRatings []     rs = rs
setRatings (x:xs) rs = setRatings xs $ setRating x rs

recordGame :: GameConfig -> GameState -> Ratings -> Ratings
recordGame cfg gs rs = setRatings ((wp,wr'):zip lps lrs') rs
    where
        wid  = getWinner gs
        lids = delete wid $ getPlayerIDs gs

        wp  = show $ players cfg !! wid
        lps = [ show $ players cfg !! lid | lid <- lids ]

        wr  = getRating wp rs
        lrs = [ getRating lp rs | lp <- lps ]

        weloWin':leloWins' = updateElos ( (eloWin wr,1): zip (map eloWin lrs) (repeat 0))

        scores = map (scoreVPs . getScore gs) $ wid:lids
        totalScore = sum scores
        scoreFractions = map (\x -> fromIntegral x / fromIntegral totalScore) scores
        weloScore':leloScores' = updateElos $ zip (map eloScore $ wr:lrs) scoreFractions


        wr' = Rating
            { eloWin    = weloWin'
            , eloScore  = weloScore'
            , wins      = wins wr+1
            , losses    = losses wr
            }

        lrs' = map f $ zip3 lrs leloWins' leloScores'
            where
                f (r,eloWin',eloScore') = Rating
                    { eloWin    = eloWin'
                    , eloScore  = eloScore'
                    , wins      = wins r
                    , losses    = losses r + 1
                    }

----------------------------------------

data Elo = Elo 
    { score     :: !Double
    , numgames  :: !Double
    }
    deriving (Show,Read,Eq,Ord)

defElo :: Elo
defElo = Elo 0 0

updateElos :: [(Elo,Double)] -> [Elo]
updateElos xs = map go $ zip [0..] xs
    where
        n = length xs

        go :: (Int,(Elo,Double)) -> Elo
        go (x,(Elo r m,s)) = Elo r' (m+1)
            where
                r' = r + k*(s-e)

                rs = map (score.fst) xs
                e = sum [1/(1+exp((rs!!i) - (rs!!x))) | i <- [0..n-1], i/=x] 
                  / (fromIntegral (n*(n-1))/2)

                alpha = 10
                k = alpha/(alpha+m**(2/3))


