data Tree a = Empty | Node a (Tree a) (Tree a) 
    deriving (Show)

instance Foldable Tree where
    --foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ ini Empty = ini
    foldr f ini (Node a left right) = foldr f midResult left
        where
            midResult = f a rightResult
            rightResult = foldr f ini right

avg :: Tree Int -> Double
avg t = (fromIntegral $ sum t) / (fromIntegral $ length t)

cat :: Tree String -> String
cat Empty = ""
cat (Node a Empty Empty) = a
cat (Node a Empty right) = a ++ " " ++ (cat right)
cat (Node a left Empty) = a ++ " " ++ (cat left)
cat (Node a left right) = a ++ " " ++ (cat left) ++ " " ++ (cat right)