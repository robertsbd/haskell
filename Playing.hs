module FirstScript where

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

    myNot :: Bool -> Bool
    myNot True = False
    myNot False = True