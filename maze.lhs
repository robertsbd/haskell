> module Maze where

To solve a maze all we have to do is search the tree for the exit when we find the exist we just
need to return the set of nodes that lead to the exit this will assume that there is only one path
to the exit want also to provide a solution that will be able to find a maze without a unique
solution

fm :: Int -> Int is being built as a simple identity function.

fm is a function :: takes an Int (a) -> and returns an Int

> fm :: Int -> Int
> fm a = a

fn :: Int -> Int -> Int is a function of two variables.

fn is a function :: takes an Int (a) -> returns a function of a that takes an Int (b) -> which
returns an Int

> fn :: Int -> Int -> Int 
> fn a b = a + b

fn can also be built as fg :: Int -> (Int -> Int)

add is a function :: takes an Int (a) -> returns a function of a which is of the type that takes an
Int (in this case b) and returns an (Int -- -> Int)

> add :: Int -> (Int -> Int)
> add a b = a + b

trans takes a function and returns an output. This function is applied to the value a*a. We can see
that the second type is mapped against the first type (which is the function) and provides the
output

> trans :: (Int -> Int) -> Int -> Int
> trans func a = func (a*a)

Function composition.
We are defining two simple functions which take a single input f and g

> f :: Int -> Int
> f x = 2 * x

> g :: Int -> Int
> g y = 10 + y

Defining an infix operator which allows us to compose functions by chaining together functions which
take in single parameters

> (>.>) :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
> m >.> n = n . m

Using this infix operator we can then provide a logical approach to chaining together functions in
an order of application from left to right

> h = f >.> g
> h1 = f >.> g >.> f

Moving onto monads now

