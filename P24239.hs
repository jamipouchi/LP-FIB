getVal :: Char -> Int
getVal val
    |   val == 'I' = 1
    |   val == 'V' = 5
    |   val == 'X' = 10
    |   val == 'L' = 50
    |   val == 'C' = 100
    |   val == 'D' = 500
    |   val == 'M' = 1000

roman2int :: String -> Int
roman2int (a:b:rest) = if ((getVal a) >= (getVal b)) then ((getVal a) + (roman2int (b:rest))) else (-(getVal a) + (roman2int (b:rest)))
roman2int [last] = getVal last
ronan2int [] = 0

roman2int' :: String -> Int
roman2int' [] = 0
roman2int' chars = sum $ foldr f [0] chars
    where
        f :: Char -> [Int] -> [Int]
        f c (first:rem) = if ((getVal c) >= first) then (getVal c):first:rem else (-(getVal c)):first:rem

arrels :: Float -> [Float]
arrels x = iterate (\last -> (0.5*(last + (x / last )))) x

arrel :: Float -> Float -> Float
arrel x e = findLast (arrels x) e
    where
        findLast :: [Float] -> Float -> Float
        findLast (a:b:rest) e = if (a - b <= e) then b else findLast (b:rest) e

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance  (Show a) => Show (LTree a) where
    show (Leaf a) = "{" ++ (show a) ++ "}"
    show (Node left right) = "<" ++ (show left) ++ "," ++ (show right) ++ ">"

build :: [a] -> LTree a
build [node] = Leaf node
build nodes = Node (build left) (build right)
    where 
        (left, right) = splitAt (div (length nodes + 1) 2) nodes

zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))
zipLTrees (Leaf l) (Leaf r) = Just (Leaf (l,r))
zipLTrees (Node ll lr) (Node rl rr) = do
    left <- (zipLTrees ll rl)
    right <- (zipLTrees lr rr)
    return (Node left right)
zipLTrees _ _ = Nothing