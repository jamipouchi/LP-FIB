type SymTab a = String -> Maybe a

empty :: SymTab a
empty = (\_ -> Nothing)
get :: SymTab a -> String -> Maybe a
get symTab key = symTab key
set :: SymTab a -> String -> a -> SymTab a
set symTab key val = (\inp -> if (inp == key) then (maybe val) else (get symTab inp)