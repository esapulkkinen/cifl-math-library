>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, Arrows, KindSignatures #-}
>{-# LANGUAGE FlexibleContexts #-}
>module Math.Graph.InGraphA where
>import Prelude hiding (id,(.))
>import qualified Data.Set as Set
>import Data.Set (Set)
>import Control.Category
>import Control.Arrow
>import Control.Applicative
>import Math.Tools.Arrow
>import Math.Tools.Set

Requires: arrows library

>import Control.Arrow.Operations
>import Control.Arrow.Transformer.Reader
>import Control.Arrow.Transformer as ArrowTransformer

>import Math.Graph.Interface
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid

>newtype InGraphA (m :: * -> * -> *) a b c = InGraphA { runInGraphA :: ReaderArrow (Graph m a) (->) b c }

>inGraphA :: InGraphA m a b c -> Graph m a -> b -> c
>inGraphA (InGraphA f) g b = runReader f (b,g)

>instance (Monoid (m Bool Bool)) => MonoidArrow (InGraphA m a) (m Bool Bool) a where
>   monoidA m = proc i -> actA -< (i,m)

>actSetA :: (Ord a) => InGraphA m a (Set a,m Bool Bool) (Set a)
>actSetA = InGraphA $ proc (a,m) -> do
>     g <- readState -< ()
>     returnA -< Set.map (\e -> action g e m) a

>actA :: InGraphA m a (a, m Bool Bool) a
>actA = proc (a,m) -> do
>    g <- readState -< ()
>    returnA -< action g a m

>instance (Ord a,GraphMonoid m Bool) => GraphArrow (InGraphA m a) a where
>  gisVertexA = isVertexA
>  gsourceA = sourceA
>  gtargetA = targetA
>  gelementsA = elementsA
>  gverticesA = verticesA
>  gedgesA    = edgesA
>  gedgesStartingFromA = edgesStartingFromA
>  gedgesEndingToA = edgesEndingToA
>  glinksA = linksA

>instance (Ord a, ReversibleGraphMonoid m Bool) => ReversibleGraphArrow (InGraphA m a) a where
>  ginverseA = inverseA
>  greversibleLinksA = reversibleLinksA

>elementsA :: (GraphMonoid m Bool, Ord a) => InGraphA m a () (Set a)
>elementsA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   returnA -< elements g

>verticesA :: (GraphMonoid m Bool, Ord a) => InGraphA m a () (Set a)
>verticesA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   returnA -< vertices g

>edgesA :: (GraphMonoid m Bool, Ord a) => InGraphA m a () (Set a)
>edgesA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   returnA -< edges g

>linksA :: (GraphMonoid m Bool, Ord e) => InGraphA m e () (Set (e,e,e))
>linksA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   edges <- runInGraphA edgesA -< ()
>   let edgelist = Set.toList edges
>   returnA -< Set.fromList $ liftA3 (,,) edgelist (map (\e -> action g e gdom) edgelist) (map (\e -> action g e gcod) edgelist)

>edgesStartingFromA :: (GraphMonoid m Bool, Ord a) => InGraphA m a a (Set a)
>edgesStartingFromA = InGraphA $ proc x -> do
>   edges <- runInGraphA edgesA -< ()
>   g <- readState -< ()
>   returnA -< Set.filter (\ e -> action g e gdom == x) edges

>edgesEndingToA :: (GraphMonoid m Bool, Ord a) => InGraphA m a a (Set a)
>edgesEndingToA = InGraphA $ proc x -> do
>   edges <- runInGraphA edgesA -< ()
>   g <- readState -< ()
>   returnA -< Set.filter (\ e -> action g e gcod == x) edges

>reversibleLinksA :: (Ord a, ReversibleGraphMonoid m Bool) => InGraphA m a () (Set ((a,a),(a,a)))
>reversibleLinksA = InGraphA $ proc edge -> do
>   g <- readState -< ()
>   edges <- runInGraphA edgesA -< ()
>   let edgelist = Set.toList edges
>       reverse_edges = map (\e -> action g e gnot) edgelist
>       domains = map (\e -> action g e gdom) edgelist
>       codomains = map (\e -> action g e gcod) edgelist
>       edgedata = zip (zip edgelist reverse_edges)
>                      (zip domains codomains)
>   returnA -< fst $ includeExcludeEdges (Set.fromList edgedata)
> where includeExcludeEdges s = Set.fold (\ ((e,re),(x,y)) (include,exclude) 
>                                          -> if not $ e `Set.member` exclude then
>                                               (Set.insert ((e,re),(x,y)) include,
>                                                Set.insert re exclude)
>                                              else (include,Set.insert re exclude)) (Set.empty,Set.empty) s

>sourceA :: (GraphMonoid m Bool) => InGraphA m a a a
>sourceA = proc e -> actA -< (e,gdom)

>targetA :: (GraphMonoid m Bool) => InGraphA m a a a
>targetA = proc e -> actA -< (e,gcod)

>inverseA :: (ReversibleGraphMonoid m Bool) => InGraphA m a a a
>inverseA = proc e -> actA -< (e,gnot)

>edgeNodesA :: (GraphMonoid m Bool) => InGraphA m a a (a,a)
>edgeNodesA = proc e -> do
>   s <- sourceA -< e
>   t <- targetA -< e
>   returnA -< (s,t)

>isVertexA :: (Eq a, GraphMonoid m Bool) => InGraphA m a a Bool
>isVertexA = proc a -> do { a' <- sourceA -< a ; returnA -< a == a' }
  
>isLoopA :: (Eq a, GraphMonoid m Bool) => InGraphA m a a Bool
>isLoopA = proc a -> do a' <- sourceA -< a
>                       b' <- targetA -< a
>                       returnA -< a' == b'

>isOneLaneLoopA :: (Eq a, ReversibleGraphMonoid m Bool) => InGraphA m a a Bool
>isOneLaneLoopA = proc a -> do { a' <- inverseA -< a ; returnA -< a == a' }

>isEdgeA :: (Eq a,GraphMonoid m Bool) => InGraphA m a (a,a,a) Bool
>isEdgeA = proc (e,x,y) -> do { x' <- sourceA -< e  ; y' <- targetA -< e ; 
>                               returnA -< x == x' && y == y' }

>instance ArrowReader (Graph m a) (InGraphA m a) where
>   readState = InGraphA readState
>   newReader (InGraphA f) = InGraphA $ newReader f
>instance Arrow (InGraphA m a) where
>   arr = InGraphA . arr ; first (InGraphA f) = InGraphA (first f)
>instance ArrowApply (InGraphA m a) where 
>   app = InGraphA $ first (arr runInGraphA) >>> app
>instance Category (InGraphA m a) where 
>    id = InGraphA id  ; (InGraphA f) . (InGraphA g) = InGraphA (f . g)

