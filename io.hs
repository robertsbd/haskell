module IOsevenDays where

tryIo =
  do
    putStr "Enter your name: " ;
    line <- getLine ;
    let { backwards = reverse line } ;
    return ("Hello. Your name backwards is " ++ backwards)

instance Monad [] where
  m >>= f  = concatMap f m
  return x = [x]
