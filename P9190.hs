multEq :: Int -> Int -> [Int]
multEq a b = iterate (* (a * b)) 1

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst xs ys zs = intersect xs (smaller ys zs [])
    where
        intersect :: Eq a => [a] -> [a] -> [a]
        intersect [] _ = []
        intersect (a:as) bs
            | elem a bs = a : intersect as bs
            | otherwise = intersect as bs
        smaller :: Eq a => [a] -> [a] -> [a] -> [a]
        smaller [] _ _ = []
        smaller as [] _ = as
        smaller (a:as) (b:bs) seen
            | not (elem a seen) = a : smaller as bs (b:seen)
            | otherwise = smaller as bs (b:seen)

myIterate :: (a -> a) -> a -> [a]
myIterate getNext start = scanl (\a _ -> getNext a) start ([1..])

type SymTab a = String -> Maybe a

empty :: SymTab a
empty = \_ -> Nothing
get :: SymTab a -> String -> Maybe a
get symTab key = symTab key
set :: SymTab a -> String -> a -> SymTab a
set symTab key val = \input -> if (input == key) then (Just val) else (get symTab input)

data Expr a
    = Val a
    | Var String
    | Sum (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving Show

eval :: (Num a) => SymTab a -> Expr a -> Maybe a
eval _ (Val val) = (Just val)
eval symTab (Var key) = get symTab key
eval symTab (Sum e1 e2) = do
    a1 <- eval symTab e1
    a2 <- eval symTab e2
    return (a1 + a2)
eval symTab (Sub e1 e2) = do
    a1 <- eval symTab e1
    a2 <- eval symTab e2
    return (a1 - a2)
eval symTab (Mul e1 e2) = do
    a1 <- eval symTab e1
    a2 <- eval symTab e2
    return (a1 * a2)
