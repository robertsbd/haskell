module DrunkenPirate where

drunkenStep :: Num t => (t, t) -> (t, t)
drunkenStep (x, y) = (x + 1, y + 1) 

drunkenWalk :: (Num t) => (t, t) -> [(t, t)]
drunkenWalk prev_step = prev_step:(drunkenWalk (drunkenStep prev_step))

data Position = Integer deriving (Show)

stagger d = d + 2
crawl d = d + 1

rtn x = x
x >>== f = f x

treasureMap pos =
  pos >>==
  stagger >>==
  stagger >>==
  crawl >>==
  rtn

