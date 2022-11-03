myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- recursive implementation of foldl
myFoldl _ a [] = a
myFoldl f a (b:bs) = myFoldl f (f a b) bs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- recursive implementation of foldr
myFoldr _ b [] = b
myFoldr f b (a:as) = f a (myFoldr f b as)

myIterate :: (a -> a) -> a -> [a]
-- recursive implementation of iterate
myIterate f a = a:(myIterate f (f a))

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
-- recursive implementation of until
myUntil check f a 
    |   check a = a
    |   otherwise = myUntil check f (f a)

myMap :: (a -> b) -> [a] -> [b]
-- implements map with the previously build foldr
myMap f xs = myFoldr (\x ys -> (f x):ys ) [] xs

myFilter :: (a -> Bool) -> [a] ->  [a]
-- implements filter using myFoldr
myFilter f xs = myFoldr (\x ys -> if (f x) then (x:ys) else ys) [] xs

myAll :: (a -> Bool) -> [a] -> Bool
-- implements all using foldl
myAll f xs = foldr (&&) True (map f xs)
-- myAll f xs = and $ map f xs  --Uses and, which is kinda cheating

myAny :: (a -> Bool) -> [a] -> Bool
-- implements any using foldl
myAny f xs = foldr (||) False (map f xs)

myZip :: [a] -> [b] -> [(a,b)]
-- recursive implementation of zip
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- implementation of zipWith using above functions
myZipWith f as bs = map (\(a,b) -> f a b) $ myZip as bs