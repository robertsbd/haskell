module SevenDaysDay1 where

    {-- Day 1 --}

    -- Question 1 page 271

    -- [x | x <- [1, 2, 3, 4], (rem x 2) == 0]

    -- Question 2 page 271 (Day 1)
    -- call the rev function by providing only the input list
    reverse_list :: [Integer] -> [Integer]
    reverse_list (input_list) = rev(input_list, [])

    -- reverse a list of integers with an accumulator
    -- rev ([list], [])
    rev :: ([Integer], [Integer]) -> [Integer]
    rev ([], acc) = acc
    rev (h:t, acc) = rev(t, h:acc)

    -- reverse a list without an accumulator
    -- rev [list]
    rev_no_acc :: [t] -> [t]
    rev_no_acc [] = []
    rev_no_acc (h:t) = rev_no_acc(t) ++ [h]

    -- Question 3 page 271 (Day 1)
    -- two_tuples ["black", "white", "blue", "yellow", "red"]
    two_tuples :: [[Char]] -> [([Char], [Char])]
    two_tuples [] = []
    two_tuples (h:t) = [(h, a) | a <- t] ++ two_tuples(t)

    -- Question 4 page 271 (Day 1)
    multiplication_table :: [(Integer, Integer, Integer)]
    multiplication_table = [ (a, b, a*b) | a <- [1..12], b <- [1..12]]

    -- map colouring Question 5 page 271 (Day 1)
    cols :: [[Char]]
    cols = ["red", "green", "blue"]

    colours :: [([Char], [Char], [Char], [Char], [Char])]
    colours = [ ("Alabama = " ++ al, "Mississippi = " ++ mi, "Georgia = " ++ ge, "Tennessee = " ++ te, "Florida = " ++ fl) | al <- cols, mi <- cols, ge <- cols, te <- cols, fl <- cols, mi /= te, mi /= al, al /= te, al /= ge, al /= fl, ge /= fl, ge /= te]


    -- cir :: [Integer]
    -- cir = [1 .. 9]

    -- solver :: [(Integer, Integer, Integer, Integer)]
    -- solver = [(a, b, c, d) | a <- cir, b <- cir, c <- cir, d <- cir, 18 == a + b + 3, 10 == 3 + b + c, 12 == 3 + c + d, 20 == 3 + d + a]



    -- col :: [String]
    -- col = ["red", "green", "yellow", "blue", "white"]

    -- nationality :: [String]
    -- nationality = ["brit", "swede", "dane", "norwegian", "german"]

    -- drink :: [String]
    -- drink = ["coffee", "tea", "milk", "beer", "water"]

    -- smoke :: [String]
    -- smoke = ["pall mall", "dunhill", "blends", "bluemaster", "prince"]
    
    -- pets :: [String]
    -- pets = ["dogs", "birds","cats", "horses", "fish"]

    -- house :: [String]
    -- house = ["One", "Two", "Three", "Four", "Five"]

    -- -- need to put something into place to deal with left right and next to

    -- houses :: [((String, String, String, String, String),
    --             (String, String, String, String, String))]
    -- houses = [((h1, c1, n1, d1, s1, p1), (h2, c2, n2, d2, s2, p2)) | 
    --             h1 <- house, c1 <- col, n1 <- nationality, d1 <- drink, p1 <- pets, 
    --             h2 <- house, c2 <- col, n2 <- nationality, d2 <- drink, p2 <- pets,
    --             h1 = _, c1 = "red", n1 = "brit", d1 = _, p1 = _,
