solve1 :: [Int] -> [String] -> Int
solve1 [ans] [] = ans
solve1 (a:b:l) (word:words)
    |   word == "+" = solve1 ((b + a):l) words
    |   word == "-" = solve1 ((b - a):l) words
    |   word == "*" = solve1 ((b * a):l) words
    |   word == "/" = solve1 ((div b a):l) words
    |   otherwise =  solve1 (num:a:b:l) words
        where num = read word :: Int
solve1 l (word:words) = solve1 (num:l) words
    where num = read word :: Int

eval1 :: String -> Int
eval1 expr = solve1 [] (words expr)

solve2 :: [String] -> Int
solve2 words = head $ foldl f [] words
    where
        f :: [Int] -> String -> [Int]
        f ans [] = ans
        f (a:b:l) word
            |   word == "+" = ((b + a):l)
            |   word == "-" = ((b - a):l)
            |   word == "*" = ((b * a):l)
            |   word == "/" = ((div b a):l)
            |   otherwise =  (num:a:b:l)
                where num = read word :: Int
        f l word = (num:l)
            where num = read word :: Int

eval2 :: String -> Int
eval2 expr = solve2 (words expr)

fsmap :: a -> [a -> a] -> a
fsmap val fs = foldl apply val fs
    where apply val f = f val

divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideNconquer base divide conquer x = case (base x) of
    Just val -> val
    Nothing -> conquer x (x1,x2) (y1,y2)
        where 
            (x1,x2) = divide x
            y1 = divideNconquer base divide conquer x1
            y2 = divideNconquer base divide conquer x2

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort xs = divideNconquer base divide conquer xs
    where
        base :: [Int] -> Maybe [Int]
        base [] = Just []
        base _ = Nothing
        divide :: [Int] -> ([Int],[Int])
        divide (p:xs) = ([x | x <- xs, x <= p], [x | x <- xs, x > p])
        conquer :: [Int] -> ([Int],[Int]) -> ([Int],[Int]) -> [Int]
        conquer (p:xs) (_,_) (l,r) = l ++ [p] ++ r

data Racional = Racional Integer Integer

racional :: Integer -> Integer -> Racional
racional a b = Racional a b

numerador :: Racional -> Integer
numerador (Racional a b) = div a m
    where m = gcd a b

denominador :: Racional -> Integer
denominador (Racional a b) = div b m
    where m = gcd a b
instance Eq Racional where
    (Racional a1 b1) == (Racional a2 b2) = a1 * b2 == a2 * b1

instance Show Racional where
    show (r) = (show $ numerador r) ++ "/" ++ (show $ denominador r)

data Tree a = Node a (Tree a) (Tree a)

recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
    where recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

racionals :: [Racional]
racionals = recXnivells $ buildTree (1,1)
    where
        buildTree :: (Integer,Integer) -> Tree Racional
        buildTree (a,b) = Node (Racional a b) (buildTree (a,(a + b))) (buildTree ((a + b),b))