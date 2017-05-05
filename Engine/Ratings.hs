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
    forM_ (take 50 $ sortBy (\(_,r1) (_,r2) -> compare (elo r2) (elo r1)) rs) $ \(p,r) -> do
        let dispname = take 40 $ p++repeat ' '
        putStrLn $ "  "++dispname 
                       ++" elo: "   ++ (frontPad 5 $ showFFloat (Just 2) (elo r) "")
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
    { elo :: Double
    , wins :: Int
    , losses :: Int
    }
    deriving (Read,Show,Eq,Ord)

numGames :: Rating -> Int
numGames r = wins r + losses r

winPercent :: Rating -> Double
winPercent r = fromIntegral (wins r) / fromIntegral (wins r + losses r)

unrated :: Rating
unrated = Rating
    { elo = 0
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
recordGame cfg gs rs = setRatings [(wp,wr'),(lp,lr')] rs
    where
        winnerid = getWinner gs
        loserid  = (winnerid+1)`mod`2
        wp = show $ players cfg !! winnerid
        lp = show $ players cfg !! loserid

        wr = getRating wp rs
        lr = getRating lp rs

        alpha=1
        (welo',_) = calcElo (alpha/(alpha+(sqrt $ fromIntegral $ numGames wr))) (elo wr, elo lr)
        (_,lelo') = calcElo (alpha/(alpha+(sqrt $ fromIntegral $ numGames lr))) (elo wr, elo lr)

        wr' = Rating
            { elo       = welo'
            , wins      = wins wr+1
            , losses    = losses wr
            }
        lr' = Rating
            { elo       = lelo'
            , wins      = wins lr
            , losses    = losses lr+1
            }

        calcElo :: Double -> (Double,Double) -> (Double,Double)
        calcElo k (r1,r2) = (r1',r2')
            where
                e1 = 1/(1+exp(r2-r1))
                e2 = 1/(1+exp(r1-r2))

                r1' = r1 + k*(1-e1)
                r2' = r2 + k*(0-e2)
