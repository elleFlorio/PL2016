module EsHs52016 where

import Control.Monad.State

-- State Monad
-- Stack
type Stack = [Int]  

-- Define the pop function  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

-- Define the push function 
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)

-- Define a function that executes the following operations on the stack:
-- pop an element
-- pop another element
-- push 100
-- pop an element
-- pop another element
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    (a,newStack1) = pop stack  
    (b,newStack2) = pop newStack1
    ((),newStack3) = push 100 newStack2
    (c, newStack4) = pop newStack3  
    in pop newStack4

-- newtype State s a = State { runState :: s -> (a,s) }
-- instance Monad (State s) where  
-- return x = State $ \s -> (x,s)  
-- (State h) >>= f = State $ \s -> let (a, newState) = h s  
--                                     (State g) = f a  
--                                 in  g newState

-- get :: m s
-- Return the state from the internals of the monad.

-- put :: s -> m ()
-- Replace the state inside the monad.

-- Define the pop function using State monad
-- runState popM [1,2,3,4,5]
popM :: State Stack Int  
popM = do
    x:xs <- get
    put xs
    return x 
    
-- Define the push function using State monad
-- runState (pushM 100) [1,2,3,4,5]
pushM :: Int -> State Stack ()  
pushM a = do
    xs <- get
    put (a:xs)
    return ()

-- Define stakcManipM function (previouly defined) using State Monad
-- runState stackManipM [1,2,3,4,5]
stackManipM :: State Stack Int  
stackManipM = do  
    popM  
    popM
    pushM 100
    popM  
    popM

-- pl20150922
-- Define the Bilist data-type, which is a container of two homogeneous lists.
-- Define an accessor for Blist, called bilist_ref, that, given an index i, 
-- returns the pair of values at position i in both lists.
-- E.g. bilist_ref (Bilist [1,2,3] [4,5,6]) 1 should return (2,5).
data Bilist a = Bilist [a] [a] deriving (Show, Eq)

bilist_ref (Bilist l r) pos = (l !! pos, r !! pos)

-- Define a function, called oddeven, that is used to build a Bilist x y from a simple list. 
-- oddeven takes all the elements at odd positions and put them in y, 
-- while all the other elements are put in x, maintaining their order. 
-- You may assume that the given list has an even length (or 0). 
-- Write also all the types of the functions you define.
-- E.g. oddeven [1,2,3,4] must be Bilist [1,3] [2,4].

-- helper
oddevenh :: [a] -> [a] -> [a] -> Bilist a
oddevenh [] ev od = Bilist ev od
oddevenh (x:xs) ev od = oddevenh xs od (ev++[x])

-- function
oddeven :: [a] -> Bilist a
oddeven l = oddevenh l [] []

-- Define an inverse of oddeven, e.g. inv_oddeven $ oddeven [1,2,3,4] must be [1,2,3,4].
-- Write also all the types of the functions you define.
inv_oddeven :: Bilist a -> [a]
inv_oddeven (Bilist l r) = foldl (++) [] $ map (\(x,y) -> [x,y]) $ zip l r

-- Define a function, called bilist_max, that given an 
-- input Bilist [x1, x2, . . . , xn] [y1, y2, . . . , yn], 
-- where xk + yk, for 1 ≤ k ≤ n, is the maximum, returns k.
-- E.g.
-- > bilist_max (Bilist [3,2,-1] [2,1,7])
--   2

-- helper
bilist_maxh (Bilist (l:ls) (r:rs)) pos curmax maxpos | 
    l+r > curmax = bilist_maxh (Bilist ls rs) (pos+1) (l+r) pos
bilist_maxh (Bilist (l:ls) (r:rs)) pos curmax maxpos =
    bilist_maxh (Bilist ls rs) (pos+1) curmax maxpos
bilist_maxh _ _ _ maxpos = maxpos

-- function
bilist_max (Bilist (l:ls) (r:rs)) = bilist_maxh (Bilist ls rs) 1 (l+r) 0




