myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f inici = case (f inici) of
                         Just (m,n) -> m:(myUnfoldr f n)
                         Nothing -> []

myReplicate :: a -> Int -> [a]
myReplicate num times = take times $ repeat num

myIterate :: (a -> a) -> a -> [a]
myIterate f inici = inici : (myUnfoldr (\x -> Just (f x, f x)) inici)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = myUnfoldr (\ys -> (customFunc f ys)) xs
    where
        customFunc _ [] = Nothing
        customFunc f (y:ys) = Just (f y, ys)

data Bst a = Empty | Node a (Bst a) (Bst a)-- deriving Show

add :: Ord a => a -> (Bst a) -> (Bst a)

add x Empty = Node x Empty Empty
add x (Node y l r)
    | x < y          = Node y (add x l) r
    | x > y          = Node y l (add x r)
    | otherwise = Node y l r

instance (Show a)  => Show (Bst a) where
    show Empty = "."
    show (Node a l r) = "(" ++ (show a) ++ " " ++ (show l) ++ " " ++ (show r) ++ ")"

adder :: Ord a => (Bst a, [a]) -> Maybe (Bst a, (Bst a, [a]))
adder (t, []) = Nothing
adder (t, (num:nums)) = Just (newT,(newT, nums))
    where
        newT = add num t
