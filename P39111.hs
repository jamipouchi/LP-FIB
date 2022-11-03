import Data.List

type Pos = (Int, Int)
dins :: Pos -> Bool
dins (x, y) = (x >= 1 && x <= 8) && (y >= 1 && y <= 8)

moviments :: Pos -> [Pos]
moviments (x,y) = [(xJump + x, yJump + y)| (xJump, yJump) <- moviments, dins (xJump + x, yJump + y)]
    where 
        moviments = [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2)]

potAnar3 :: Pos -> Pos -> Bool
potAnar3 ini desti = elem desti (concatMap moviments $ concatMap moviments $ moviments ini)

potAnar3' :: Pos -> Pos -> Bool
potAnar3' ini desti = or possibilities
    where 
        possibilities = do
            firstMov <- (moviments ini)
            secondMov <- (moviments firstMov)
            thirdMov <- (moviments secondMov)
            return (thirdMov == desti)