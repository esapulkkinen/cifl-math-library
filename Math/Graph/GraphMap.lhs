>{-# LANGUAGE TypeOperators #-}
>module Math.Graph.GraphMap where
>import Prelude hiding (id,(.))
>import qualified Data.Set as Set
>import Data.Monoid
>import Math.Tools.Isomorphism
>import Data.Set (Set)
>import Control.Category 
>import Math.Graph.Reversible

>data GraphMap m a b = GraphMap { runGraphMap :: Graph m a -> Graph m b }                                                           
>instance Category (GraphMap m) where
>  id = GraphMap id
>  (GraphMap f) . (GraphMap g) = GraphMap (f . g)

>actGraph :: (Ord a) => m -> GraphMap m a a
>actGraph m = GraphMap $ \ (Graph s act) -> Graph (Set.map (appEndo (act m)) s) act

>terminal :: GraphMap m a ()
>terminal = GraphMap (const emptyG)
  
>element :: Graph m a -> GraphMap m () a
>element g = GraphMap (const g)

>graphIso :: (Ord a, Ord b) => a :==: b -> GraphMap m a b
>graphIso (Iso f g) = GraphMap $ \ (Graph s act) -> Graph (Set.map f s) (action act)
>   where action act m = Endo $ \b -> f $ act m `appEndo` (g b)
