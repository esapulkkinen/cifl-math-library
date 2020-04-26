>{-# LANGUAGE Safe #-}
>module Math.Tools.Universe where
>import Math.Tools.Orthogonal
>import Math.Tools.Functor
>import Math.Tools.List
>import safe Data.Monoid
>import safe Data.Int
>import safe Data.Word

>-- | The universe class describes a listing of elements of type a, not
>-- including the 'undefined'.
>-- This class is frequently needed when dealing with properties.
>-- 
>-- This is a representation of a recursively enumerable set.
>--
>-- Note that algorithms based on this can be excessively inefficient
>-- e.g. for 64 bit integers going through all alternatives is not likely
>-- to be fully successful. The instances for big types are designed
>-- so that most likely occurring values are in the beginning of
>-- the list, e.g. for Integer type, the universe is
>-- @[0,1,-1,2,-2,...]@.
>class Universe a where
>   all_elements :: [a]

>separator :: (Universe a, Eq b) => (a -> b) -> (a -> b) -> Bool
>separator f g = and [ f a == g a | a <- all_elements ]

>-- | <https://en.wikipedia.org/wiki/Equivalence_class>
>equivalence_class :: (Universe b) => (a -> b -> Bool) -> a -> [b]
>equivalence_class eq a = [b | b <- all_elements, a `eq` b]

>different_elements :: (Universe a, Eq b) => (a -> b) -> (a -> b) -> [a]
>different_elements f g = [a | a <- all_elements, f a /= g a]

>isMonotone :: (Universe a, Ord a, Ord b) => (a -> b) -> Bool
>isMonotone f = and [if x >= y then f x >= f y else True | (x,y) <- all_elements]

>domain :: (Universe a) => (a -> b) -> [a]
>domain f = [const x $! (f $! x) | x <- all_elements]

>instance Universe Integer where { all_elements = [0..] `interleave` (map negate [1..]) }
>instance Universe Int where { all_elements = all_nums }
>instance Universe Int8 where { all_elements = all_nums }
>instance Universe Int16 where { all_elements = all_nums }
>instance Universe Int32 where { all_elements = all_nums }
>instance Universe Int64 where { all_elements = all_nums }
>instance Universe Word where { all_elements = [minBound..maxBound] }
>instance Universe Word8 where { all_elements = [minBound..maxBound] }
>instance Universe Word16 where { all_elements = [minBound..maxBound] }
>instance Universe Word32 where { all_elements = [minBound..maxBound] }
>instance Universe Word64 where { all_elements = [minBound..maxBound] }

>-- | order here is chosen based on what numbers are more
>-- likely in applications, so numbers close to zero occur first.

>all_nums :: (Num t, Bounded t, Enum t) => [t]
>all_nums = interleave [0 .. maxBound] [-1,-2..minBound]

>instance Universe () where
>   all_elements = [()]

>instance (Universe a) => Universe (Maybe a) where
>   all_elements = Nothing : map Just all_elements

>instance (Universe a, Universe b) => Universe (Either a b) where
>   all_elements = interleave (map Left all_elements) (map Right all_elements)

>instance Universe Bool where
>   all_elements = [True,False]

>instance Universe Ordering where
>   all_elements = [LT,EQ,GT]

>instance Universe Char where
>   all_elements = [minBound..maxBound]

>instance (Universe b, Universe a, Eq a) => Universe (a -> b) where
>   all_elements = map table_to_function all_functions

>instance (Universe a) => Universe [a] where
>   all_elements = all_lists all_elements

>-- | All lists produces the infinite list of all lists.

>all_lists :: [a] -> [[a]]
>all_lists lst = [] : do lsts <- all_lists lst
>                        v <- lst
>                        return (v:lsts)

>table_to_function :: (Eq a) => [(a,b)] -> a -> b
>table_to_function lst x = let Just v = lookup x lst in v

>all_functions :: (Universe d, Universe c) => [[(c,d)]]
>all_functions = all_functions_from all_elements all_elements

>all_subsets :: [a] -> [[a]]
>all_subsets d = map (map fst . filter snd) (all_functions_with_domain d)

>all_functions_with_domain :: (Universe c) => [d] -> [[(d,c)]]
>all_functions_with_domain dom = all_functions_from dom all_elements

>-- | all_functions_from only produces any results if the domain list is finite.

>all_functions_from :: [c] -> [d] -> [[(c,d)]]
>all_functions_from [] _ = [[]]
>all_functions_from (d:dr) ls = concatMap gen_list ls
>   where gen_list c = map ((d,c):) mr
>         mr = all_functions_from dr ls 

>instance (Universe a, Universe b) => Universe (a,b) where
>   all_elements = concat $ outer (,) all_elements all_elements

>instance (Universe a, Universe b, Universe c) => Universe (a,b,c) where
>   all_elements = concat (map concat (outer3_functor (,,) all_elements all_elements all_elements))

>instance (Universe a, Eq a) => Universe (Endo a) where
>   all_elements = fmap Endo all_elements

