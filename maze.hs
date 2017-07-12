module Maze where

data Tree a = Children [Tree a] | Leaf a deriving (Show)

depth (Leaf _) = 1
depth (Children c) = 1 + maximum (map depth c)

-- to solve a maze all we have to do is serach the tree for the exit
-- when we find the exist we just need to return the set of nodes that lead to the exit
-- this will assume that there is only one path to the exit
-- want also to provide a solution that will be able to find a maze without a
-- unique solution

-- playing with function composition

f :: Int -> Int
f x = 2 * x

g :: Int -> Int
g y = 3 + y

h :: Int -> Int
h = g . f

-- m :: (Int -> Int) -> Int

m :: Int -> Int -> Int
m a b = a + b


