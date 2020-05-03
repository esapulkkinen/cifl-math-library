>{-# LANGUAGE GADTs, Arrows, RankNTypes #-}
>module Math.Graph.AbstractGraph where
>-- ^ This module provides graphs as objects in a category.
>-- See <https://en.wikipedia.org/wiki/Comma_category>
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow

>data Pair a = Pair a a

>instance Functor Pair where
>  fmap f (Pair x y) = Pair (f x) (f y)

>data AGraph arr edge node where
>   AGraph :: Pair (arr edge node) -> AGraph arr edge node

>ag_domcod :: (Arrow arr) => AGraph arr edge node -> arr edge (Pair node)
>ag_domcod (AGraph (Pair ag_dom ag_cod)) = proc e -> do
>   d <- ag_dom -< e
>   c <- ag_cod -< e
>   returnA -< Pair d c

data GraphArr arr a b where
  GraphArr :: (forall edge node edge' node'.
              (AGraph arr edge node -> arr a a),
              (AGraph arr edge' node' -> arr b b),
              ((AGraph arr edge node -> AGraph arr edge' node') -> arr a b))
           -> GraphArr arr a b
           

instance (Category arr) => Category (GraphArr arr) where
   id = GraphArr (\m -> m (const id) (const id) id)
   (GraphArr f) . (GraphArr f') =
     GraphArr (\m -> m (\domg -> id) (\codg -> id)
                    (\h -> f (const id) . h . f' (const id)))
