module Cont where

import Data.Function
import Text.Printf
import Control.Monad
import Control.Monad.State.Lazy

ascFact :: (Num a, Monad m) => a -> a -> (a -> a -> m r) -> m r
ascFact n f cont = cont (n + 1) (n * f)

from0 :: (Num a, Monad m) => (a -> a -> m r) -> m r
from0 cont = ascFact 1 1 cont

listFact :: Num a => a -> a -> [a]
listFact = 
    fix $ \rec n' f' -> f' : ascFact n' f' rec

showFact :: (Ord a, Num a, PrintfArg a) => a -> a -> a -> IO ()
showFact n =
    fix $ \rec n' f' -> printf "%d! = %d\n" (n' - 1) f' >> if n' <= n then ascFact n' f' rec else return ()

stateFact :: (Ord a, Num a, Monad m) => a -> a -> a -> StateT [(a,a)] m ()
stateFact n =
    fix $ \rec n' f' -> get >>= (put . ((n',f'):)) >> if n' < n then ascFact n' f' rec else return ()


fun :: Int -> IO ()
fun n = do
    putStrLn $ show $ take n $ from0 listFact
    from0 $ showFact n
    execStateT (from0 $ stateFact n) [(1,1)] >>= (putStrLn . show)





