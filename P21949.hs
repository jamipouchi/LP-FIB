solveHanoi ["1", nom1, nom2, nom3] = do
    putStrLn (nom1 ++ " -> " ++ nom2)
solveHanoi ["0", nom1, nom2, nom3] = do
    return ()
solveHanoi [numDiscs, nom1, nom2, nom3] = do
    let discs = read numDiscs :: Int
    solveHanoi [show (discs - 1), nom1, nom3, nom2]
    putStrLn (nom1 ++ " -> " ++ nom2)
    solveHanoi [show (discs - 1), nom3, nom2, nom1]
solveHanoi _ = do
    putStrLn "input incorrecta"

main = do
    line <- getLine
    solveHanoi $ words line
