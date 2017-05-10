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
    -- sort [3,4,2,3,4,1,1]] gives [1,1,2,3,3,4,4]

    -- question 2
    -- sortBy (\x y -> compare y x ) [1,3,2,4,1,8,3] -- this would do a reverse sort, note the compare y x specifies that x should follow y, input to the function defined x as the first and y as the second parameter

    -- question 3