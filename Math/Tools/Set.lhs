>{-# LANGUAGE GADTs, ExistentialQuantification, Rank2Types #-}
>module Math.Tools.Set where
>import qualified Data.Set as Set
>import Data.Set (Set)
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow
>import qualified Data.List as List
>import Math.Tools.CoFunctor

>mapSetM :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
>mapSetM f s = Set.fold (\a mrest -> do elem <- f a
>                                       rest <- mrest
>                                       return $ Set.insert elem rest)
>                            (return Set.empty) s

>joinSet :: (Ord a) => Set (Set a) -> Set a
>joinSet s = Set.fold Set.union Set.empty s

>outerSet :: (Ord c) => (a -> b -> c) -> Set a -> Set b -> Set (Set c)
>outerSet f x y = Set.map (\e -> Set.map (\e' -> f e e')
>                                        y)
>                         x

>pairSet :: (Ord a, Ord b) => Set a -> Set b -> Set (Set (a,b))
>pairSet = outerSet (,)

>zipSet :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
>zipSet s s' = joinSet $ Set.map (\a -> Set.map (\b -> (a,b)) s') s

>all_sets :: (Ord a) => [a] -> [Set a]
>all_sets [] = [Set.empty]
>all_sets (c:cr) = Set.empty : map (Set.insert c) (all_sets cr)

>instance (Universe a, Ord a) => Universe (Set a) where
>   all_elements = all_sets all_elements
