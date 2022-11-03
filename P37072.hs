data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ fe fd) = 1 + (size fe) + (size fd)

height :: Tree a -> Int
height Empty = 0
height (Node _ fe fd) = 1 + max (height fe) (height fd)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a1 fe1 fd1) (Node a2 fe2 fd2) = a1 == a2 && (equal fe1 fe2) && (equal fd1 fd2)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a1 fe1 fd1) (Node a2 fe2 fd2) = a1 == a2 && ((equal fe1 fe2 && equal fd1 fd2) || (equal fe1 fd2 && equal fe2 fd1))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a fe fd) = a:((preOrder fe) ++ (preOrder fd))

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a fe fd) = (postOrder fe) ++ (postOrder fd) ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a fe fd) = (inOrder fe) ++ [a] ++ (inOrder fd)

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst (Node a fe fd) = a:(buildRec [fe,fd])
    where
        buildRec :: [Tree a] -> [a]
        buildRec [] = []
        buildRec trees = treeValues ++ buildRec nextTrees
            where
                isNotEmpty :: Tree a -> Bool
                isNotEmpty Empty = False
                isNotEmpty _ = True
                nonEmptyTrees = [t | t <- trees, isNotEmpty t]
                treeValues = [a | (Node a _ _ ) <- nonEmptyTrees]
                nextTrees = concat $ map (\(Node _ fe fd) -> [fe, fd]) nonEmptyTrees

build :: Eq a => [a] -> [a] -> Tree a
build [] _ = Empty
build _ [] = Empty
build (preO : preOs) i = Node preO (build lp li) (build rp ri)
  where (li,_:ri) = span (/=preO) i
        (lp,rp) = splitAt (length li) preOs

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty b = b
overlap _ a Empty = a
overlap f (Node a1 fe1 fd1) (Node a2 fe2 fd2) = Node (f a1 a2) (overlap f fe1 fe2) (overlap f fd1 fd2)
                        