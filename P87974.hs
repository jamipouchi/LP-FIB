main = do
    nom <- getLine
    let inicial = (head nom)
    if (inicial == 'A' || inicial == 'a')
        then putStrLn $ "Hello!"
        else putStrLn $ "Bye!"