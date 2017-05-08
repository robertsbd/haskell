{-- Haskell notepad, playing around with while learning haskell --}


module Playing where

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