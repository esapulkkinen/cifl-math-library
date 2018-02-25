>{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
>-- | This graph representation is based on the book "Lawvere: Sets for mathematics".
>-- vertices are such edges where @e `act` gdom == e == e `act` gcod@
>-- 
>-- Note it's probably better to use 'Math.Graph.Reversible' instead.
>module Math.Graph.Graph where
>import Prelude hiding (reverse, (.),id)
>import Control.Category 
>import Control.Monad
>import Data.Monoid
>import qualified Data.Set as Set
>import Data.Set (Set)
>import qualified Data.Map as Map
>import Data.Map (Map)
>import Math.Tools.CoFunctor
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Math.Tools.NaturalTransformation
>import Math.Tools.Prop
>import Math.Graph.Action
>import Math.Graph.GraphMonoid hiding (vertex,edge)


>type Vertex n = Bool -> n
>type Edge   n = Bool -> n
>type Graph  n   = Set (Edge n)

>source :: Edge n -> n
>source e = e False

>target :: Edge n -> n
>target e = e True

required: 

>-- |
>-- prop> x `act` (b . a) == (x `act` b) `act` a 
>-- prop> x `act` id == x

>act :: (b -> x) -> Endo b -> (b -> x)
>act e (Endo m) = e . m

>toAction :: Edge n -> Action n Bool
>toAction = Action 

>instance (Eq n) => Eq (Bool -> n) where
>   f == g = source f == source g && target f == target g

>instance (Show n) => Show (Bool -> n) where
>   show f = show (source f) ++ " -> " ++ show (target f)

>showVertex :: (Show n) => Vertex n -> String
>showVertex f = show (source f)

>instance (Ord n) => Ord (Bool -> n) where
>  compare f g = case (compare (source f) (source g),
>                      compare (target f) (target g)) of
>    { (GT,_) -> GT ; (LT,_) -> LT ; (EQ,x) -> x }

>vertex :: n -> Vertex n
>vertex x = \_ -> x

>edge :: n -> n -> Edge n
>edge x y = \v -> if not v then x else y 

>actionGraphOf :: (Ord n) => Graph n -> Set (Edge n,Edge n,Edge n)
>actionGraphOf = Set.map (\i -> (i,i `act` gdom,i `act` gcod))

>reversibleGraphOf :: (Ord n) => Graph n -> Set (Edge n,Edge n,Edge n,Edge n)
>reversibleGraphOf= Set.map (\i->(i,i `act` gdom,i `act` gcod,i `act` gnot))

>edgesFrom :: (Ord n) => Graph n -> n -> Set (Edge n)
>edgesFrom g s = Set.filter (\e -> source e == s) $ edges g  

>edgesTo :: (Ord n) => Graph n -> n -> Set (Edge n)
>edgesTo g t = Set.filter (\e -> target e == t) $ edges g

>targetsFrom :: (Ord n) => Graph n -> n -> Set n
>targetsFrom g s = Set.map target $ Set.filter (\e -> source e == s) $ edges g
  
>sourcesTo :: (Ord n) => Graph n -> n -> Set n
>sourcesTo g t = Set.map source $ Set.filter (\e -> target e == t) $ edges g  

>split :: (Ord n) => Graph n -> (Set (Vertex n), Set (Edge n))
>split = Set.partition (\e -> e `isIn` vertexSubset)

>vertices :: (Ord n) => Graph n -> Set (Vertex n)
>vertices g = fst (split g)

>edges :: (Ord n) => Graph n -> Set (Edge n)
>edges g = snd (split g)

>vertexSubset :: (Eq n) => Prop (Edge n)
>vertexSubset = Characteristic (\x -> x `act` gdom  == x)

>loopSubset :: (Eq n) => Prop (Edge n)
>loopSubset = Characteristic (\x -> x `act` gdom == x `act` gcod)

>oneLaneLoopSubset :: (Eq n) => Prop (Edge n)
>oneLaneLoopSubset = Characteristic (\x -> x `act` gnot == x)

>degenerateLoopSubset :: (Eq n) => Prop (Edge n)
>degenerateLoopSubset = vertexSubset `intersect` loopSubset

>edgeFromSubset :: (Eq n) => Vertex n -> Prop (Edge n)
>edgeFromSubset a = Characteristic (\x -> x `act` gdom == a)

>edgeToSubset :: (Eq n) => Vertex n -> Prop (Edge n)
>edgeToSubset b  = Characteristic (\x -> x `act` gcod == b)

>edgeSubset :: (Eq n) => Vertex n -> Vertex n -> Prop (Edge n)
>edgeSubset a b = edgeFromSubset a `intersect` edgeToSubset b

>toVertex :: (Eq n, MonadPlus m) => Edge n -> m (Vertex n)
>toVertex e = guard (e `isIn` vertexSubset) >> return e

creating graphs:

>emptyG :: Graph n
>emptyG = Set.empty

>vertexG :: n -> Graph n
>vertexG = Set.singleton . vertex

>edgeG :: (Ord n) => n -> n -> Graph n
>edgeG x y = Set.fromList [edge x y, vertex x, vertex y]

>verticesG :: (Ord n) => Set n -> Graph n
>verticesG = Set.map vertex

edgesG seems complicated but this is because the vertices from the
edges must also belong to the graph, e.g. graph is a set closed under
the action.

>edgesG :: (Ord n) => Set (n,n) -> Graph n
>edgesG lst = Set.map (vertex . fst) lst
>               `Set.union` Set.map (vertex . snd) lst
>               `Set.union` Set.map (uncurry edge) lst

>unionG :: (Ord n) => Graph n -> Graph n -> Graph n
>unionG = Set.union

>composeG :: (Ord n) => Graph n -> Graph n -> Graph n
>composeG g h = Set.fromList $
>    [ edge (target a) (source b) 
>    | a <- Set.elems g, b <- Set.elems h, source a  == target b ]




