data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val a) = a
eval1 (Add e1 e2) = (eval1 e1) + (eval1 e2)
eval1 (Sub e1 e2) = (eval1 e1) - (eval1 e2)
eval1 (Mul e1 e2) = (eval1 e1) * (eval1 e2) 
eval1 (Div e1 e2) = (eval1 e1) `div` (eval1 e2) 

eval2 :: Expr -> Maybe Int
eval2 (Val a) = Just a
eval2 (Add e1 e2) = do
    x1 <- eval2 e1
    x2 <- eval2 e2
    return (x1 + x2)
eval2 (Sub e1 e2) = do
    x1 <- eval2 e1
    x2 <- eval2 e2
    return (x1 - x2)
eval2 (Mul e1 e2) = do
    x1 <- eval2 e1
    x2 <- eval2 e2
    return (x1 * x2)
eval2 (Div e1 e2) = do
    x1 <- eval2 e1
    x2 <- eval2 e2
    if (x2 == 0) 
        then Nothing
        else return (x1 `div` x2)

eval3 :: Expr -> Either String Int
eval3 (Val a) = Right a
eval3 (Add e1 e2) = do
    x1 <- eval3 e1
    x2 <- eval3 e2
    return (x1 + x2)
eval3 (Sub e1 e2) = do
    x1 <- eval3 e1
    x2 <- eval3 e2
    return (x1 - x2)
eval3 (Mul e1 e2) = do
    x1 <- eval3 e1
    x2 <- eval3 e2
    return (x1 * x2)
eval3 (Div e1 e2) = do
    x1 <- eval3 e1
    x2 <- eval3 e2
    if (x2 == 0) 
        then Left "div0"
        else return (x1 `div` x2)