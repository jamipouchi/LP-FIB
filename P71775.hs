countIf :: (Int -> Bool) -> [Int] -> Int
-- given a checker and a list of integers, returns the number of elements that fulfill the check
countIf check xs = foldl (\count x -> if (check x) then (count + 1) else count) 0 xs

pam :: [Int] -> [Int -> Int] -> [[Int]]
-- given a list of integers and of functions, apply each function to the list
pam xs fs = [map f xs | f <- fs]

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
-- given a list of integers and of functions, apply to each integer the functions
pam2 xs fs = map (\x -> [f x | f <- fs]) xs

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
-- Read the name :)
--filterFoldl _ _ x [] = x
--filterFoldl check f x (y:ys) = filterFoldl check f (if (check y) then (f x y) else x) ys
filterFoldl check f x ys = foldl f x $ filter check ys

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
-- given a comparer, a list and an element, it inserts in mantaining the order
insert comp [] y = [y]
insert comp (x:xs) y
    |   comp x y    = x:(insert comp xs y)
    |   otherwise   = y:(x:xs)

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort comp xs = foldr (\x xs -> insert comp xs x) [] xs
--insertionSort comp xs = recInsertionSort comp xs []
--    where
--       recInsertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [Int]
--        recInsertionSort comp [] ys = ys
--        recInsertionSort comp (x:xs) ys = recInsertionSort comp xs (insert comp ys x)