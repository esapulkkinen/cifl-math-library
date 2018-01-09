>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, Arrows #-}
>module Math.Graph.InGraphA where
>import Prelude hiding (id,(.))
>import qualified Data.Set as Set
>import Data.Set (Set)
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow

Requires: arrows library

>import Control.Arrow.Operations
>import Control.Arrow.Transformer.Reader
>import Control.Arrow.Transformer as ArrowTransformer

>import Math.Graph.Interface
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid

>newtype InGraphA m a b c = InGraphA { runInGraphA :: ReaderArrow (Graph m a) (->) b c }

>inGraphA :: InGraphA m a b c -> Graph m a -> b -> c
>inGraphA (InGraphA f) g b = runReader f (b,g)

>instance (Monoid m) => MonoidArrow (InGraphA m a) m a where
>   monoidA m = proc i -> actA -< (i,m)

>actA :: InGraphA m a (a,m) a
>actA = InGraphA $ proc (a,m) -> do
>     g <- readState -< ()
>     ArrowTransformer.lift (uncurry (action g)) -<< (a,m)

>instance (Ord a,GraphMonoid m) => GraphArrow (InGraphA m a) a where
>  gisVertexA = isVertexA
>  gsourceA = sourceA
>  gtargetA = targetA
>  gelementsA = elementsA
>  gverticesA = verticesA
>  gedgesA    = edgesA
>  gedgesStartingFromA = edgesStartingFromA
>  gedgesEndingToA = edgesEndingToA
>  glinksA = linksA

>instance (Ord a, ReversibleGraphMonoid m) => ReversibleGraphArrow (InGraphA m a) a where
>  ginverseA = inverseA
>  greversibleLinksA = reversibleLinksA

>elementsA :: (GraphMonoid m) => InGraphA m a () (Set a)
>elementsA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   returnA -< elements g

>verticesA :: (GraphMonoid m, Ord a) => InGraphA m a () (Set a)
>verticesA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   returnA -< Set.filter (\a -> action g a gdom == a) $ elements g

>edgesA :: (GraphMonoid m, Ord a) => InGraphA m a () (Set a)
>edgesA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   returnA -< Set.filter (\a -> action g a gdom /= a) $ elements g

>linksA :: (GraphMonoid m, Ord e) => InGraphA m e () (Set (e,e,e))
>linksA = InGraphA $ proc () -> do
>   g <- readState -< ()
>   edges <- runInGraphA edgesA -< ()
>   returnA -< Set.map (\e -> (e,action g e gdom,action g e gcod)) edges

>edgesStartingFromA :: (GraphMonoid m, Ord a) => InGraphA m a a (Set a)
>edgesStartingFromA = InGraphA $ proc x -> do
>   edges <- runInGraphA edgesA -< ()
>   g <- readState -< ()
>   returnA -< Set.filter (\a -> action g a gdom == x) edges

>edgesEndingToA :: (GraphMonoid m, Ord a) => InGraphA m a a (Set a)
>edgesEndingToA = InGraphA $ proc x -> do
>   edges <- runInGraphA edgesA -< ()
>   g <- readState -< ()
>   returnA -< Set.filter (\a -> action g a gcod == x) edges

>reversibleLinksA :: (Ord a, ReversibleGraphMonoid m) => InGraphA m a () (Set ((a,a),(a,a)))
>reversibleLinksA = InGraphA $ proc edge -> do
>   g <- readState -< ()
>   edges <- runInGraphA edgesA -< ()
>   let edgedata = Set.map (\e -> ((e,action g e gnot),(action g e gdom, action g e gcod))) edges
>   returnA -< fst $ includeExcludeEdges edgedata       
> where includeExcludeEdges s = Set.fold (\ ((e,re),(x,y)) (include,exclude) 
>                                          -> if not $ e `Set.member` exclude then
>                                               (Set.insert ((e,re),(x,y)) include,
>                                                Set.insert re exclude)
>                                              else (include,Set.insert re exclude)) (Set.empty,Set.empty) s

>sourceA :: (GraphMonoid m) => InGraphA m a a a
>sourceA = proc e -> actA -< (e,gdom)

>targetA :: (GraphMonoid m) => InGraphA m a a a
>targetA = proc e -> actA -< (e,gcod)

>inverseA :: (ReversibleGraphMonoid m) => InGraphA m a a a
>inverseA = proc e -> actA -< (e,gnot)

>edgeNodesA :: (GraphMonoid m) => InGraphA m a a (a,a)
>edgeNodesA = proc e -> do
>   s <- sourceA -< e
>   t <- targetA -< e
>   returnA -< (s,t)

>isVertexA :: (Eq a, GraphMonoid m) => InGraphA m a a Bool
>isVertexA = proc a -> do { a' <- sourceA -< a ; returnA -< a == a' }
  
>isLoopA :: (Eq a, GraphMonoid m) => InGraphA m a a Bool
>isLoopA = proc a -> do a' <- sourceA -< a
>                       b' <- targetA -< a
>                       returnA -< a' == b'

>isOneLaneLoopA :: (Eq a, ReversibleGraphMonoid m) => InGraphA m a a Bool
>isOneLaneLoopA = proc a -> do { a' <- inverseA -< a ; returnA -< a == a' }

>isEdgeA :: (Eq a,GraphMonoid m) => InGraphA m a (a,a,a) Bool
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

