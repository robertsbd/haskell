module Testing where

    {-- Day 1 --}

    -- Question 2 page 271 (Day 1)
    -- call the rev function by providing only the input list
    reverse_list :: [Integer] -> [Integer]
    reverse_list (input_list) = rev(input_list, [])

    -- reverse a list of integers with an accumulator
    rev :: ([Integer], [Integer]) -> [Integer]
    rev ([], acc) = acc
    rev (h:t, acc) = rev(t, h:acc)

    -- reverse a list without an accumulator


    -- Question 3 page 271 (Day 1)
    two_tuples :: [[Char]] -> [([Char], [Char])]
    two_tuples [] = []
    two_tuples (h:t) = [(h, a) | a <- t] ++ two_tuples(t)

    -- Question 4 page 271 (Day 1)
    multiplication_table = [ (a, b, a*b) | a <- [1..12], b <- [1..12]]

    -- map colouring Question 5 page 271 (Day 1)
    cols :: [[Char]]
    cols = ["red", "green", "blue"]

    colours :: [([Char], [Char], [Char], [Char], [Char])]
    colours = [ ("Alabama = " ++ al, "Mississippi = " ++ mi, "Georgia = " ++ ge, "Tennessee = " ++ te, "Florida = " ++ fl) | al <- cols, mi <- cols, ge <- cols, te <- cols, fl <- cols, mi /= te, mi /= al, al /= te, al /= ge, al /= fl, ge /= fl, ge /= te]