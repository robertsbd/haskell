module Day2 where

-- question 1 - sort a list, selectSort and mergeSort provided

-- selectSort
-- uses selection to remove the min from the list and append to another list

selectSort :: [Integer] -> [Integer]
selectSort []       = []
selectSort [x]      = [x]
selectSort xs       = m:selectSort (select m xs)
    where
        m = minimum xs

select :: Integer -> [Integer] -> [Integer]    -- select first occurence of x from (y:ys) and remove, returning a list without that in it 
select q (z:zs)
    | q == z        = zs
    | otherwise     = z:select q zs

-- mergeSort
-- merges by splitting a list recursively and then merging the sorted lists back up the stack

mergeSort :: [Integer] -> [Integer]
mergeSort []        = []
mergeSort [x]       = [x]
mergeSort xs        = merge (mergeSort ys) (mergeSort zs)
        where (ys, zs) = split xs

split :: [Integer] -> ([Integer], [Integer])
split []            = ([], [])
split [x]           = ([x], []) 
split (x:y:rest)    = (x:xs, y:ys)
    where (xs, ys)  = split rest

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y        = x:merge xs (y:ys) 
    | y < x         = y:merge (x:xs) ys

-- question 2 - realised question 1 was not writing your own sort (need to >> import Data.List)

-- question 1 again would be
-- sort [3,4,2,3,4,1,1] gives [1,1,2,3,3,4,4]

-- question 2
-- sortBy (\x y -> compare y x ) [1,3,2,4,1,8,3] -- this would do a reverse sort, note the compare y x specifies that x should follow y, input to the function defined x as the first and y as the second parameter

-- question 3

stringToNum :: [Char] -> Float
stringToNum input_string = read ([x | x <- input_string, x /= '$', x /= ',']) :: Float

-- question 4

everyThirdNum x = everyNum x 3

everyFifthNum x = everyNum x 5

everyEigthNum x y = zipWith (+) (everyThirdNum x) (everyFifthNum y)

everyNum start step = start:(everyNum (start + step) step)

-- question 5

div2 x y = y / 2
half = div2 2 -- call this function as half x and it will return half the value

app x = x ++ "\n"
addCR = app

-- question 6

gcdenom :: Integer -> Integer -> Integer
gcdenom n m
  | n == 0         = 0
  | m == 0         = 0
  | n >= m         = gcdH n m m
  | otherwise      = gcdH m n m
  where
    gcdH :: Integer -> Integer -> Integer -> Integer
    gcdH n m divisor
      | m == 1                                           = 1
      | (rem m divisor == 0) && (rem n divisor == 0)     = divisor
      | otherwise                                        = gcdH n m (divisor - 1)

-- question 7

primeNumList :: Int -> [Integer]
primeNumList n = take n [x | x <- [1 .. ], isPrime x]

isPrime :: Integer -> Bool
isPrime n = isPrimeH n (n-1)
  where
    isPrimeH :: Integer -> Integer -> Bool
    isPrimeH n m
      | m <= 1 = True
      | rem n m == 0      = False
      | otherwise         = isPrimeH n (m-1)

-- q 8

breakString :: [Char] -> [Char]
breakString in_string = [if x == ' ' then '\n' else x | x <- in_string]


-- q 9

breakStringNum :: [Char] -> [Char] -> Integer -> [Char]
breakStringNum [] out_string counter = "1 " ++ out_string
breakStringNum (' ':xs) out_string counter = breakStringNum xs (out_string ++ "\n" ++ (show counter) ++ " ") (counter + 1)
breakStringNum (x:xs) out_string counter = breakStringNum xs (out_string ++ [x]) counter

-- q 10

-- breakStrList in_str :: [Char] width_of_line :: Integer justifcation :: Char

breakStrList :: [Char] -> Int -> Char -> [Char]
breakStrList in_str width_of_line justification = foldr (++) [] (zipWith (\w l -> right w l ++ l ++ full w l ++ w ++ "\n") (words in_str) (map (\x -> show x) [1..]))
  where
        full word line
          | justification == 'f'   = spacing word line
          | otherwise              = []
        right word line
          | justification == 'r'   = spacing word line
          | otherwise              = []
        spacing word line = take (width_of_line - length word - length line) (repeat ' ')

rightJust = putStr (breakStrList "hello I said to the man over by the door, he was having a great time. I wasn't quite as sure but it was fun all the same" 60 'r')
leftJust = putStr (breakStrList "hello I said to the man over by the door, he was having a great time. I wasn't quite as sure but it was fun all the same" 60 'l')
fullJust = putStr (breakStrList "hello I said to the man over by the door, he was having a great time. I wasn't quite as sure but it was fun all the same" 60 'f')
