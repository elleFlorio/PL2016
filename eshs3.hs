module EsHs3 where

import qualified Data.Map as M
import Control.Monad

-- Binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Define a function that creates a singleton tree (one node with 2 empty leaf)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Define a function to insert an element into a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- Define a function to check if a tree contains an element
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- Define a function that sum the elements of a tree composed by numbers
treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + (treeSum left) + (treeSum right)

-- Define a function that returns a list containing all the values in a tree
treeValues :: Tree a -> [a]
treeValues EmptyTree = []
treeValues (Node a left right) = a : ((treeValues left) ++ (treeValues right))

-- Define the map function on tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f EmptyTree = EmptyTree
treeMap f (Node a left right) = Node (f a ) (treeMap f left) (treeMap f right)

-- Define the foldl function on tree
treeFoldl :: (a -> b -> a) -> a -> Tree b -> a
treeFoldl f acc EmptyTree = acc
treeFoldl f acc (Node b left right) = treeFoldl f (f (treeFoldl f acc left) b) right

-- Define the foldr function on tree
treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr f acc EmptyTree = acc
treeFoldr f acc (Node b left right) = treeFoldr f (f b (treeFoldr f acc right)) left

-- Define a filter function on tree
treeFilter :: (a -> Bool) -> Tree a -> [a]
treeFilter f = treeFoldr (\x acc -> if f x then x:acc else acc) []

-- Define the sum function on a tree using treeFoldr
treeSum2 :: Num a => Tree a -> a
treeSum2 = treeFoldr (+) 0

-- Define the values function on a tree using treeFoldr
treeValues2 :: Tree a -> [a]
treeValues2 = treeFoldr (:) []

-- Define the values function on a tree using treeFoldl
treeValues3 :: Tree a -> [a]
treeValues3 t = treeFoldl (++) [] (treeMap (:[]) t)

-- typeclass
-- implementation example (from the Prelude:)
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)
--
-- This means that a) we have to implement both "equals" and "not equals"
-- and b) since "x is equal to y if x is not equal to y" and viceversa,
-- we can just define "equals" or "not equals" and Haskell will infer the
-- other one. (minimal complete definition)

-- Define a TrafficLight that implements Eq and Show typeclasses
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- Monads
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg

-- Maybe monad
-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f  = f x
--     fail _ = Nothing

-- do notation example
-- using bind
foo :: Maybe String  
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- using do notation
foo2 :: Maybe String  
foo2 = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y) 

-- k-v map monad
-- lookup :: Ord k => k -> Map k a -> Maybe a
-- O(log n). Lookup the value at a key in the map.
-- The function will return the corresponding value as (Just value), 
-- or Nothing if the key isn't in the map.
type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = TIM
                   | Vodafone
                   | Wind
                     deriving (Eq, Ord)

-- Define a function which takes as input:
-- the name of a person
-- the map(personName, PhoneNumber)
-- the map(PhoneNumber, MobileCarrier)
-- the map(MobileCarrier, BillingAddress)
-- and returns the corresponding billing address
-- or Nothing if the search does not produce results
  
-- maps e.g.
-- let persPhone = M.fromList [("pippo","12345678"),("topolino","87654321")]
-- let phoneMobile = M.fromList [("12345678", TIM),("87654321", Vodafone)]
-- let mobileBilling = M.fromList [(TIM,"Via tal dei tali"),(Vodafone, "via tizio caio")]
findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

-- That's annoying, let's use the power of monads!!!
findCarrierBillingAddress2 :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress2 person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap