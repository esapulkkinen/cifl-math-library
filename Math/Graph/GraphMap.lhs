>{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}
>module Math.Graph.GraphMap where
>import Prelude hiding (id,(.))
>import qualified Data.Set as Set
>import Data.Monoid
>import Math.Tools.Isomorphism
>import Math.Tools.NaturalTransformation
>import Data.Set (Set)
>import Data.Kind
>import Control.Category 
>import Math.Graph.Reversible
>import Control.Arrow
>import Math.Tools.Arrow
>import Math.Graph.GraphMonoid
> 
>data GraphMap (m :: Type -> Type -> Type) a b = GraphMap {
>   runGraphMap :: Graph m a -> Graph m b
> }
>
>mapGraphMap :: (Ord a, Ord b, Ord c, Ord c') => c :==: c' -> a :==: b -> GraphMap m c' a :==: GraphMap m c b
>mapGraphMap f' f = (\ (GraphMap g) -> GraphMap $ isomorphismEpimorphism fm . g . isomorphismEpimorphism fm')
>             <-> (\ (GraphMap g) -> GraphMap $ isomorphismSection fm . g . isomorphismSection fm')
>   where fm = mapG f
>         fm' = mapG f'
>instance Category (GraphMap m) where
>  id = GraphMap id
>  (GraphMap f) . (GraphMap g) = GraphMap (f . g)

>emptyGMap :: GraphMap m () b
>emptyGMap = elementGMap emptyG

>vertexGMap :: a -> GraphMap m () a
>vertexGMap x = elementGMap $ vertexG x
>
>edgeGMap :: (Ord a) => a -> a -> a -> GraphMap Three () a
>edgeGMap e x y = elementGMap $ edgeG e x y

>elementGMap :: Graph m a -> GraphMap m () a
>elementGMap = GraphMap . const

>productGMap :: (Ord b, Ord b') => GraphMap m a b -> GraphMap m a b' -> GraphMap m a (b,b')
>productGMap (GraphMap f) (GraphMap g) = GraphMap $ \a -> productG (f a) (g a)

>graphIso :: (Arrow m, Ord a, Ord b) => a :==: b -> GraphMap m a b
>graphIso (Iso f g) = GraphMap $ \ (Graph v e act) -> Graph (Set.map f v) (Set.map f e) (action act)
>   where action act m = Endo $ f . appEndo (act m) . g
