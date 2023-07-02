>{-# LANGUAGE TypeOperators, KindSignatures #-}
>module Math.Graph.GraphMap where
>import Prelude hiding (id,(.))
>import qualified Data.Set as Set
>import Data.Monoid
>import Math.Tools.Isomorphism
>import Data.Set (Set)
>import Control.Category 
>import Math.Graph.Reversible
>import Control.Arrow
> 
>data GraphMap (m :: * -> * -> *) a b = GraphMap { runGraphMap :: Graph m a -> Graph m b }
> 
>instance Category (GraphMap m) where
>  id = GraphMap id
>  (GraphMap f) . (GraphMap g) = GraphMap (f . g)

>terminal :: GraphMap m a ()
>terminal = GraphMap (const emptyG)
  
>element :: Graph m a -> GraphMap m () a
>element g = GraphMap (const g)

>graphIso :: (Arrow m, Ord a, Ord b) => a :==: b -> GraphMap m a b
>graphIso (Iso f g) = GraphMap $ \ (Graph v e act) -> Graph (Set.map f v) (Set.map f e) (action act)
>   where action act m = Endo $ f . appEndo (act m) . g
