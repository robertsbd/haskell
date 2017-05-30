{-- Haskell notepad, playing around with while learning haskell --}


module Playing where

    import Data.Char

    size :: Integer
    size = 12+13

    square :: Integer -> Integer
    square n = n*n

    double :: Integer -> Integer
    double n = 2*n

    double_and_square :: Integer -> Integer
    double_and_square x = square (double x) 

    square_and_double :: Integer -> Integer
    square_and_double x = double (square x)      

    example :: Integer
    example = double (size - square(2*2))

    exOr :: Bool -> Bool -> Bool
    exOr x y = x /= y

    exOr1 :: Bool -> Bool -> Bool
    exOr1 True x = not x
    exOr1 False x = x

    myNot :: Bool -> Bool
    myNot True = False
    myNot False = True

    threeDifferent :: Integer -> Integer -> Integer -> Bool
    threeDifferent m n p = (m /= n) && (n /= p) && (m /= p)

    threeEqual :: Integer -> Integer -> Integer -> Bool
    threeEqual m n p = (m == n) && (n == p)

    fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
    fourEqual m n p q = (m == n) && (n == q) && (p == q)

    fourEqual1 :: Integer -> Integer -> Integer -> Integer -> Bool
    fourEqual1 m n p q = (threeEqual m n p) && (m == q)

    myMax :: Integer -> Integer -> Integer
    myMax x y
        | x >= y        = x
        | otherwise     = y

    -- recursive max of a list using a helper variable, seed the first max as the head of the list
    maxList :: [Integer] -> Integer
    maxList (x:xs) = maxListHelper xs x

    maxListHelper :: [Integer] -> Integer -> Integer
    maxListHelper [] acc = acc
    maxListHelper (x:xs) acc
        | x > acc       = maxListHelper(xs) x 
        | otherwise     = maxListHelper(xs) acc   

    -- character functions (page 54) Haskell The Craft of Functional Programming

    offset :: Int
    offset = fromEnum 'A' - fromEnum 'a'

    convertToUpper :: Char -> Char
    convertToUpper x   
        | fromEnum x <= 90  = x
        | otherwise         = toEnum (fromEnum x + offset)

    convertToNum :: Char -> Int
    convertToNum x
        | (fromEnum x >= 48) && (fromEnum x <= 57)  = fromEnum x - 48
        | otherwise                                 = 0 

    -- Strings

    romanDigit :: Char -> String
    romanDigit x
        | x == '1'      = "I"
        | x == '2'      = "II"
        | x == '3'      = "III"
        | x == '4'      = "IV"
        | x == '5'      = "V"
        | x == '6'      = "VI"
        | x == '7'      = "VII"
        | x == '8'      = "VIII"
        | x == '9'      = "IX"
        | otherwise     = ""

    -- Floats

    numberNDroots :: Float -> Float -> Float -> Integer
    numberNDroots a b c 
        | b^2 > 4.0*a*c     = 2
        | b^2 == 4*a*c      = 1
        | otherwise         = 0 


    -- Tests

    prop_myNot :: Bool -> Bool
    prop_myNot x =
        not x == myNot x

    prop_exOrs :: Bool -> Bool -> Bool
    prop_exOrs x y =
        exOr x y == exOr1 x y

    prop_square :: Integer -> Bool
    prop_square n =
        square n == n * n

    prop_fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
    prop_fourEqual m n p q =
        fourEqual m n p q == fourEqual1 m n p q

    -- Now trying some recursive functions

    -- rangeProduct, returns the product of a range of numbers, m must be less than n. needs to deal with m of 0

    rangeProduct :: Integer -> Integer -> Integer
    rangeProduct m n
        | n + m == 0        = 1
        | n < m             = 0
        | n - m == 0        = m
        | n - m > 0         = rangeProduct m (n-1) * n

    -- factorial using rangeProduct

    facRange :: Integer -> Integer
    facRange n = rangeProduct 0 n 

    -- Int multiplication through addition

    mult :: Integer -> Integer -> Integer
    mult n m
        | n == 0    = 0
        | n > 0     = mult (n - 1) m + m

    -- Int Division through subtraction, create a helper func with an accumulator to count how many times we subtract m from n until we can't subtract

    intDiv :: Integer -> Integer -> Integer
    intDiv n m = intDivH n m 0
        where
        intDivH :: Integer -> Integer -> Integer -> Integer
        intDivH n m acc
            | m == 0    = error "Divide by zero"
            | n < m     = acc
            | n >= m    = intDivH (n-m) m (acc+1) 


    -- Remainder

    remainder :: Integer -> Integer -> Integer
    remainder n m 
        | m > n         = error "Denominator larger than numerator"
        | otherwise     = n - (intDiv n m * m) 

    -- Int Sqr Root, very inefficient as x becomes large as performs larger number of iterations

    intSqrRoot :: Integer -> Integer
    intSqrRoot x 
        | x < 0                     = error "intSqrRoot only works for non negative values"
        | x == 0                    = x
        | otherwise                 = intSqrRootH x (intDiv x 2)        -- initial value of result is half of x, need to get a better initial estimate to speed up
        where
        intSqrRootH :: Integer -> Integer -> Integer
        intSqrRootH m n
            | n*n > m               = intSqrRootH m (n-1)               -- here we should step down by a number that is not -1, leave for another time as only an exercise
            | otherwise             = n 

    -- functions that take functions as parameters

    -- maxFun is the max of the values of f n

    maxFun :: (Integer -> Integer) -> Integer -> Integer
    maxFun f n = maxFunH f n 0
        where
        maxFunH :: (Integer -> Integer) -> Integer -> Integer -> Integer
        maxFunH g m maxval
            | m < 0             = maxval
            | maxval < f m      = maxFunH g (m-1) (f m)
            | otherwise         = maxFunH g (m-1) maxval

    -- are any of the values of f n == 0

    isZeroFun :: (Integer -> Integer) -> Integer -> Bool
    isZeroFun f n = isZeroFunH f n False
        where
        isZeroFunH :: (Integer -> Integer) -> Integer -> Bool -> Bool
        isZeroFunH g m is_zero
            | m < 0             = is_zero
            | f m == 0          = isZeroFunH g (m-1) True
            | otherwise         = isZeroFunH g (m-1) is_zero

    f 0 = 1
    f 1 = 44
    f 2 = 17
    f 3 = 70
    f _ = 0

    -- SumFun page 86, Haskell Craft of functional programming

    sumFun :: (Integer -> Integer) -> Integer -> Integer
    sumFun f n
        | n==0        = f 0
        | n>0         = sumFun f (n-1) + f n

    -- number of regions in a plane divided by lines such that each line crosses every other line, each line creats n new regions

    regions :: Integer -> Integer
    regions n
        | n == 0    = 1
        | n>0       = regions (n-1) + n

    regions2 :: Integer -> Integer 
    regions2 m = sumFun (\x -> if x == 0 then 1 else x) m 

    -- how many regions created by cutting a 3D area by planes

    regions3D :: Integer -> Integer 
    regions3D n
        | n == 0    = 1
        | n > 0     = regions3D (n-1) + regions(n-1)

    -- Highest Common Denominator

    gcf :: Integer -> Integer -> Integer
    gcf m n 
        | n > m                   = gcf n m     -- swap parameters around to ensure first parameter is the greater
        | remainder m n == 0      = n
        | otherwise               = gcf n (remainder m n)

    -- Testing

    maxThree :: Integer -> Integer -> Integer -> Integer
    maxThree x y z
        | x >= y && x >= z  = x
        | y >= z            = y
        | otherwise         = z


    -- naive Fib

    fib :: Integer -> Integer
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    -- Faster Fib

    fibStep :: (Integer,Integer) -> (Integer,Integer)
    fibStep (u,v) = (v,u+v)

    fibPair :: Integer -> (Integer, Integer)
    fibPair n
        | n == 0        = (0,1)
        | otherwise     = fibStep(fibPair (n-1))

    fastFib :: Integer -> Integer
    fastFib = fst . fibPair

    -- Factorial

    fact :: Integer -> Integer
    fact n
        | n == 0        = 1
        | otherwise     = n*fact(n-1)

    -- From page 103 - Craft of Functional Programming

    maxOccurs :: Integer -> Integer -> (Integer,Integer)
    maxOccurs x y
        | x > y     = (x, 1)
        | y > x     = (y, 1)
        | x == y    = (x, 2)
    
    maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
    maxThreeOccurs x y z
        | snd (maxOccurs x y) == 1              = (u, v)
        | snd (maxOccurs x y) == 2 && v == 2    = (u, 3)
        | fst (maxOccurs x y) == u              = (u, 2)
        | snd (maxOccurs x y) == 2 && v == 1    = (u, 1)
        where (u,v) = maxOccurs (fst (maxOccurs x y)) z

    -- work out the various parts of a straight line

    slope :: (Float,Float) -> (Float,Float) -> Float
    slope (x1,y1) (x2,y2) = (y2-y1)/(x2-x1) 

    yIntercept :: (Float,Float) -> (Float,Float) -> Float
    yIntercept (x1,y1) (x2,y2) = -x1*slope (x1,y1) (x2,y2) + y1 

    xIntercept :: (Float,Float) -> (Float,Float) -> Float
    xIntercept (x1,y1) (x2,y2) = x1-y1*1/slope (x1,y1) (x2,y2)

    isPairSum :: Integer -> [Integer] -> Bool
    isPairSum target xs
        | length xs <= 1                    = False                                
        | head xs + last xs == target       = True
        | head xs + last xs > target        = isPairSum target (init xs)
        | head xs + last xs < target        = isPairSum target (tail xs)

    isPairSum2 :: Integer -> [Integer] -> [Integer] -> Bool
    isPairSum2 target xs testlist
        | length xs < 1                                                = False
        | length (filter (== head xs) testlist) /= 0                   = True
        | otherwise                                                    = isPairSum2 target (tail xs) ((target - (head xs)):testlist)

    -- types and data

    data Shape =    Circle Float |
                    Rectangle Float Float 
                    deriving (Eq,Ord,Show)

    perimeter :: Shape -> Float
    perimeter (Circle r)        = 2*pi*r
    perimeter (Rectangle h w)   = 2*h+2*2

    -- lists

    doubleAll :: [Integer] -> [Integer]
    doubleAll xs = [2*x | x <- xs]

    -- using offset defined earlier

    capitalise :: String -> String
    capitalise xs = [toEnum (fromEnum x + offset) | x <- xs]

    capitaliseLetter :: String -> String
    capitaliseLetter xs = [toEnum (fromEnum x + offset) | x<-xs, not (isDigit x)]

    capitaliseOnlyLetter :: String -> String
    capitaliseOnlyLetter xs = [ if isDigit x then x else toEnum (fromEnum x + offset) | x <- xs]

    divisors :: Integer -> [Integer]
    divisors x = [ y | y <- [2, 4 .. x], mod x y == 0]

    isPrime :: Integer -> Bool
    isPrime x = ([] == [ y | y <- [2 .. (x-1)], mod x y == 0])

    matches :: Integer -> [Integer] -> [Integer]
    matches y xs = [z | z <- xs, z == y]

    elem1 :: Integer -> [Integer] -> Bool
    elem1 y xs = [] /= matches y xs

    onSeparateLines :: [String] -> String -- need to to call this with putStr to print this out withthe line breaks
    onSeparateLines xs = concat [ x ++ "\n" | x <- xs]

    duplicate :: String -> Integer -> String
    duplicate str n
        | n <= 0        = ""
        | otherwise     = concat [ str | x <- [1..n]]

    pushRight :: Int -> String -> String
    pushRight l str = concat [ " " | x <- [1 .. (l - length str)]] ++ str

    fibS :: Integer -> String
    fibS n = "n\tfib n\n" ++ concat [show(x) ++ "\t" ++ show(fib x) ++ "\n" | x <- [1 .. n]]

    build_list :: Integer -> Integer -> [Integer]
    build_list i n 
        | i <= n        = i:(build_list (i+1) n)
        | otherwise     = []

    build_list2 :: Integer -> Integer -> [Integer]
    build_list2 i n = [i .. n]