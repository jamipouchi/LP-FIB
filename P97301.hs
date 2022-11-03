fizzBuzz :: [Either Int String]
fizzBuzz = map translateToFizzBuzz [0..]
    where
        translateToFizzBuzz :: Int -> Either Int String
        translateToFizzBuzz x
            |   (x `mod` 15 == 0) = Right "FizzBuzz"
            |   (x `mod` 3 == 0) = Right "Fizz"
            |   (x `mod` 5 == 0) = Right "Buzz"
            |   otherwise = Left x