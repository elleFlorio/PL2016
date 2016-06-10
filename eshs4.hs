module EsHs4 where

import Control.Applicative
import Control.Monad

-- Logger
type Log = [String]
newtype Logger a = Logger { runLogger :: (a, Log) }

-- Define an instance of Show for Logger
instance Show a => Show (Logger a) where
    show (Logger a) = show a

-- Define an instance of Functor for Logger
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

loggerMap :: (a -> b) -> (Logger a) -> (Logger b)
loggerMap f lg =
    let (v, l) = runLogger lg
        n = f v
    in Logger (n, l)

instance Functor Logger where
    fmap = loggerMap

-- Define an instance of Applicative Functor for Logger
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

loggerApp :: Logger (a -> b) -> Logger a -> Logger b
loggerApp lf lg =
    let (f, s) = runLogger lf
        nl = loggerMap f lg
        (n, l) = runLogger nl
    in Logger (n, l ++ s)

instance Applicative Logger where
    pure a = Logger (a, [])
    (<*>) = loggerApp

-- Define the Logger Monad
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

instance Monad Logger where
    m >>= f = let (a, w) = runLogger m
                  n      = f a
                  (b, x) = runLogger n
              in Logger (b, w ++ x)

-- Define a function that takes a number, add one and log the operation
logPlusOne :: (Num a) => a -> Logger a
logPlusOne a = Logger (a+1, ["Add One"])

-- Define a function that takes a number, doubles it and log the operation
logMultiplyTwo :: (Num a) => a -> Logger a
logMultiplyTwo a = Logger (a*2, ["Multiply Two"])

-- Define a function that takes a logger, adds one, dobles the value
-- and logs all the operations
logOps :: (Num a) => Logger a -> Logger a
logOps lg = do
    v <- lg
    p1 <- logPlusOne v
    m2 <- logMultiplyTwo p1
    return m2

-- Define a record function to record things in the log
record :: String -> Logger ()
record s = Logger ((), [s])

-- Binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr f acc EmptyTree = acc
treeFoldr f acc (Node b left right) = treeFoldr f (f b (treeFoldr f acc right)) left
--------------------------------

-- We want to add logging to our tree, so define singletonM, treeInsertM and treeSumM
-- that logs the operations performed on the tree during execution
singletonM :: (Show a) => a -> Logger (Tree a)
singletonM x = do
    record ("Created singleton " ++ show x)
    return (Node x EmptyTree EmptyTree)

-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldM treeInsertM EmptyTree nums
-- (foldM because treeInsertM returns a monad instead of the standard data structure)
treeInsertM :: (Ord a, Show a) => Tree a -> a -> Logger (Tree a)
treeInsertM EmptyTree x = singletonM x
treeInsertM (Node a left right) x
    | x == a = do
        record ("Inserted " ++ show x)
        return (Node x left right)
    | x < a = do
        l <- treeInsertM left x
        return (Node a l right)
    | x > a = do
        r <- treeInsertM right x
        return (Node a left r)

treeSumM :: Num a => Logger (Tree a) -> Logger a
treeSumM t = fmap (treeFoldr (+) 0) t

-- Define a function that logs if the tree is balanced
-- Hint: define an andM function to do "logical and"(&)
-- on your monad
andM :: Logger Bool -> Logger Bool -> Logger Bool
andM l1 l2 = do
    c1 <- l1
    c2 <- l2
    return (c1 && c2)

treeBalancedM :: Tree a -> Logger Bool
treeBalancedM EmptyTree = do
    record "An empty tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree EmptyTree) = do
    record "A single node tree is always balanced"
    return True
treeBalancedM (Node _ EmptyTree _) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ _ EmptyTree) = do
    record "Unbalanced!"
    return False
treeBalancedM (Node _ left right) = andM (treeBalancedM left) (treeBalancedM right)