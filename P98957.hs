ones :: [Integer]
-- returns an infinite list of 1
ones = repeat 1

nats :: [Integer]
-- returns ALL the natural numbers
nats = iterate (+1) 0

ints :: [Integer]
-- returns ALL the integers
ints = [ (div x 2) * (if x `mod` 2 == 0 then 1 else (-1)) | x <- (tail nats)]

triangulars :: [Integer]
-- returns ALL the triangual numbers
triangulars = [div (x * (x + 1)) 2 | x <- nats]

factorials :: [Integer]
-- returns ALL the factorials
factorials = scanl (*) 1 (tail nats)

fibs :: [Integer]
-- returns the ENTIRE fibonacci sequence
-- fibs = 0:1: (zipWith (+) fibs (tail fibs))
fibs = buildFibs 0 1
        where
            buildFibs :: Integer -> Integer -> [Integer]
            buildFibs ant act = ant:(buildFibs act (ant + act))

primes :: [Integer]
-- returns ALL the prime numbers
primes = garbell [2..]
        where
            garbell (p:xs) = p: garbell [x | x <- xs, x `mod` p /= 0]


hammings :: [Integer]
-- returns... you guessed it!... ALL the hamming numbers (those divisible by 2,3,5)
--hammings = 1 : [x | x <- [2..], x `mod` 2 == 0 || x `mod` 3 == 0 || x `mod` 5 == 0]
hammings = 1 : map (*2) hammings `merge` map (*3) hammings `merge` map (*5) hammings
            where
                merge (x:xs) (y:ys)
                    | x < y = x : xs `merge` (y:ys)
                    | x > y = y : (x:xs) `merge` ys
                    | otherwise = x : xs `merge` ys

lookNsay :: [Integer]
-- returns ALL the lookNsay numbers starting at 1
lookNsay = buildLookNsay 1
    where
        buildLookNsay prev = prev : buildLookNsay (buildNext prev )
          where
              buildNext :: Integer -> Integer
              buildNext num
                  | num == 0 = 0
                  | otherwise = (10 * repes + (num `mod` 10)) + (100 * (buildNext (num `div` (10^repes))))
                where
                    repes = toInteger(length $ takeWhile (== dig) (reverse (show num)))
                        where
                            dig:[] = show (num `mod` 10)

tartaglia :: [[Integer]]
-- returns ALL the rows of the Tartaglia triangle
tartaglia = map buildRow nats
    where
        buildRow :: Integer -> [Integer]
        buildRow num = [num `choose` k | k <- (take (fromInteger num + 1) nats)]
            where
                choose :: Integer -> Integer -> Integer
                choose n k = div (factorial n) ((factorial k) * (factorial (n - k)))
                    where
                        factorial x
                            |   x == 0 = 1
                            |   otherwise = x * factorial (x - 1)
