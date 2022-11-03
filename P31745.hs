flatten :: [[Int]] -> [Int]
-- Given a list of lists, returns a list with the elements
flatten = foldl (++) []


myLength ::  String -> Int
--  returns the length of a given array
myLength xs = foldl (\x y -> x + 1) 0 xs


myReverse :: [Int] -> [Int]
-- Reverses a list of integers
myReverse = foldl (flip (:)) [] -- flip (:) == \x y -> y:x (basically we keep adding elements to the empty list)


countIn :: [[Int]] -> Int -> [Int]
--  given a 2D matrix, and a value x, returns for each subarray the countln of x
countIn matrix x = map (\xs -> repetitions x xs) matrix
    where
        repetitions :: Int -> [Int] -> Int
        repetitions x xs = length $ filter (==x) xs


firstWord :: [Char] -> [Char]
--  returns the first word in the string
firstWord = (\str -> takeWhile (/= ' ') $ dropWhile (== ' ') str)