data STree a = Nil | Node Int a (STree a) (STree a) deriving Show

isOk :: STree a -> Bool
-- first approach, wildly inefficient
isOk Nil = True
isOk (Node talla _ left right) = (talla == 1 + (num_nodes left) + (num_nodes right)) && (isOk left) && (isOk right)
    where
        num_nodes :: STree a -> Int
        num_nodes Nil = 0
        num_nodes (Node _ _ left right) = 1 + (num_nodes left) + (num_nodes right)


getCount :: STree a -> Int
getCount Nil = 0
getCount (Node talla _ _ _) = talla

nthElement :: STree a -> Int -> Maybe a
nthElement Nil _ = Nothing
nthElement (Node talla a left right) k
    |   k > talla   =   Nothing
    |   k <= left_nodes  =  nthElement left k
    |   k  ==  left_nodes + 1 = Just a
    |   otherwise   =   nthElement right (k - left_nodes - 1)
        where left_nodes = getCount left
-- which is easy to see that is O(h) as we are only traversing one branch. Most cases we won't even get to h prof.

mapToElements :: (a -> b) -> STree a -> [Int] -> [Maybe b]
mapToElements _ _ [] = []
mapToElements f stree (x:xs) = case val of
    Nothing -> Nothing : (mapToElements f stree xs)
    Just a -> (Just (f a)) : (mapToElements f stree xs)
    where val = nthElement stree x

instance Functor STree where
    fmap f Nil = Nil
    fmap f (Node talla a left right) = Node talla (f a) (fmap f left) (fmap f right) 