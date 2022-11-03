divisors :: Int -> [Int]
divisors x = [k | k <- [1..x], mod x k == 0]

nbDivisors :: Int -> Int
nbDivisors = length . divisors

moltCompost :: Int -> Bool
moltCompost x = and $ map ( < nbDivisors x ) [nbDivisors k | k <- [0..(x - 1)]]