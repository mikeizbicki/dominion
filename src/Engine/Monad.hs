module Engine.Monad
    where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Writer
import qualified Data.DList as D

----------------------------------------

data MessageType
    = All
    | Game
    | Turn
    | None
    deriving (Read,Show,Eq,Ord)

newtype Sim a = Sim (RandT StdGen (Writer (D.DList (MessageType,String))) a)
    deriving (Functor,Applicative,Monad,MonadRandom)

writeMsg :: MessageType -> String -> Sim ()
writeMsg t str = Sim $ do
    tell $ D.fromList [(t,str)]

mkStdGen :: Sim StdGen
mkStdGen = Sim $ do
    r <- getRandom
    return $ Control.Monad.Random.mkStdGen r

runSim :: MessageType -> Sim a -> IO a
runSim t (Sim m) = do
    sg <- newStdGen
    let (a,w) = runWriter $ evalRandT m sg
    go (D.toList w)
    return a
    where
        go []            = return ()
        go ((t',str):xs) = do
            if t'>=t
                then putStrLn str
                else return ()
            go xs
