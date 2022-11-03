data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue xs ys) = Queue xs (a:ys)

pop :: Queue a -> Queue a
-- This fails if the queue is empty!
pop (Queue [] ys) = Queue (tail (reverse ys)) []
pop (Queue (x:xs) ys) = Queue xs ys 
 
top :: Queue a -> a
top (Queue [] ys) = last ys
top (Queue xs _) = head xs

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False

instance Eq a => Eq (Queue a)
-- The second and third pattern matches are to not throw error if they have different lengths
     where
        Queue x1s y1s == Queue x2s y2s = (x1s ++ (reverse y1s)) == (x2s ++ (reverse y2s))
instance Functor Queue where
    fmap f (Queue fst snd) = Queue (map f fst) (map f snd)

translation :: Num b => b -> Queue b -> Queue b
translation val (Queue fst snd) = fmap (+val) (Queue fst snd)

q2l :: (Queue a) -> [a]
q2l (Queue fst snd) = (fst ++ (reverse snd))

instance Applicative Queue where
    pure x = (Queue [x] [])
    f <*> q = (Queue l [])
        where
            l = (q2l f) <*> (q2l q)

instance Monad Queue where
    q >>= f = Queue l []
        where
            l = (q2l q) >>= (q2l . f)

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter pred q = do
    x <- q
    if (pred x) then return x else Queue [] []