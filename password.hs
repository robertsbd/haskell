module Pass where

crack = do
  x <- ['a' .. 'z'] ; y <- ['a' .. 'z'] ; z <- ['a' .. 'z'] ;
  let { password = [x, y, z] } ;
  if attempt password
    then return (password,  "cracked")
    else return (show x, "")

attempt pw = if pw == "ben" then True else False
