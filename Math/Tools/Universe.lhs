>{-# LANGUAGE Safe #-}
>module Math.Tools.Universe where
>import Math.Tools.Orthogonal
>import Math.Tools.Functor
>import Math.Tools.List
>import safe Data.Monoid
>import safe Data.Int
>import safe Data.Word
>import safe qualified Data.Set as Set
>import safe Data.Set (Set)

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
>   allElements :: [a]

>separator :: (Universe a, Eq b) => (a -> b) -> (a -> b) -> Bool
>separator f g = and [ f a == g a | a <- allElements ]

>-- | <https://en.wikipedia.org/wiki/Equivalence_class>
>equivalenceClass :: (Universe b) => (a -> b -> Bool) -> a -> [b]
>equivalenceClass eq a = [b | b <- allElements, a `eq` b]

>differentElements :: (Universe a, Eq b) => (a -> b) -> (a -> b) -> [a]
>differentElements f g = [a | a <- allElements, f a /= g a]

>isMonotone :: (Universe a, Ord a, Ord b) => (a -> b) -> Bool
>isMonotone f = and [if x >= y then f x >= f y else True | (x,y) <- allElements]

>domain :: (Universe a) => (a -> b) -> [a]
>domain f = [const x $! (f $! x) | x <- allElements]

>instance Universe Integer where { allElements = [0..] `interleave` (map negate [1..]) }
>instance Universe Int where { allElements = allNums }
>instance Universe Int8 where { allElements = allNums }
>instance Universe Int16 where { allElements = allNums }
>instance Universe Int32 where { allElements = allNums }
>instance Universe Int64 where { allElements = allNums }
>instance Universe Word where { allElements = [minBound..maxBound] }
>instance Universe Word8 where { allElements = [minBound..maxBound] }
>instance Universe Word16 where { allElements = [minBound..maxBound] }
>instance Universe Word32 where { allElements = [minBound..maxBound] }
>instance Universe Word64 where { allElements = [minBound..maxBound] }

>-- | order here is chosen based on what numbers are more
>-- likely in applications, so numbers close to zero occur first.

>allNums :: (Num t, Bounded t, Enum t) => [t]
>allNums = interleave [0 .. maxBound] [-1,-2..minBound]

>instance Universe () where
>   allElements = [()]

>instance (Universe a) => Universe (Maybe a) where
>   allElements = Nothing : map Just allElements

>instance (Universe a, Universe b) => Universe (Either a b) where
>   allElements = interleave (map Left allElements) (map Right allElements)

>instance Universe Bool where
>   allElements = [True,False]

>instance Universe Ordering where
>   allElements = [LT,EQ,GT]

>instance Universe Char where
>   allElements = [minBound..maxBound]

>instance (Universe b, Universe a, Eq a) => Universe (a -> b) where
>   allElements = map tableToFunction allFunctions

>instance (Universe a) => Universe [a] where
>   allElements = allLists allElements

>-- | All lists produces the infinite list of all lists.

>allLists :: [a] -> [[a]]
>allLists lst = [] : do lsts <- allLists lst
>                       v <- lst
>                       return (v:lsts)

>tableToFunction :: (Eq a) => [(a,b)] -> a -> b
>tableToFunction lst x = let Just v = lookup x lst in v

>allFunctions :: (Universe d, Universe c) => [[(c,d)]]
>allFunctions = allFunctionsFrom allElements allElements

>allSubsets :: [a] -> [[a]]
>allSubsets d = map (map fst . filter snd) (allFunctionsWithDomain d)

>allFunctionsWithDomain :: (Universe c) => [d] -> [[(d,c)]]
>allFunctionsWithDomain dom = allFunctionsFrom dom allElements

>-- | all_functions_from only produces any results if the domain list is finite.

>allFunctionsFrom :: [c] -> [d] -> [[(c,d)]]
>allFunctionsFrom [] _ = [[]]
>allFunctionsFrom (d:dr) ls = concatMap genList ls
>   where genList c = map ((d,c):) mr
>         mr = allFunctionsFrom dr ls 

>instance (Universe a, Universe b) => Universe (a,b) where
>   allElements = concat $ functorOuter (,) allElements allElements

>instance (Universe a, Universe b, Universe c) => Universe (a,b,c) where
>   allElements = concatMap concat (outer3Functor (,,) allElements allElements allElements)

>instance (Universe a, Eq a) => Universe (Endo a) where
>   allElements = fmap Endo allElements

>allSets :: (Ord a) => [a] -> [Set a]
>allSets [] = [Set.empty]
>allSets (c:cr) = Set.empty : map (Set.insert c) (allSets cr)

>instance (Universe a, Ord a) => Universe (Set a) where
>   allElements = allSets allElements
