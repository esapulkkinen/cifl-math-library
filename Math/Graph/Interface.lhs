>{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies #-}
>{-# LANGUAGE DefaultSignatures #-}
>module Math.Graph.Interface where
>import Prelude hiding (id, (.))
>import Control.Arrow
>import qualified Data.Set as Set
>import Data.Set (Set)
>import qualified Data.Map as Map
>import Data.Map (Map)
>import Math.Tools.Set
>import Control.Category
> 
>class DigraphFactory g v e | g -> v, g -> e, v e -> g where
>   vertexGraph :: v -> g
>   edgeGraph :: e -> v -> v -> g
>   verticesFromSetGraph :: Set v -> g
>   verticesGraph :: [v] -> g
>   completeGraph :: [v] -> (v -> v -> e) -> g
>   edgeGraphFromMap :: Map e (v,v) -> g
>   edgesGraph :: [(e,(v,v))] -> g
>   loopGraph :: [(v,e)] -> g

>class ReversibleGraphFactory g v e | g -> v, g -> e, v e -> g where
>   reversibleVertexGraph :: v -> g
>   reversibleVerticesGraph :: [v] -> g
>   reversibleVerticesFromSetGraph :: Set v -> g
>   reversibleEdgeGraph :: e -> e -> v -> v -> g
>   reversibleCompleteGraph :: [v] -> (v -> v -> (e,e)) -> g
>   reversibleEdgesGraph :: [((e,e),(v,v))] -> g
>   reversibleLoopGraph :: [(v,e,e)] -> g

>class (GraphMonad m e) => LabeledGraphMonad m lbl e where
>   glabelOf :: e -> m lbl
>   gfind :: lbl -> m e

>class (Arrow arr) => GraphArrow arr e where
>  gisVertexA :: arr e Bool
>  gsourceA   :: arr e e
>  gtargetA   :: arr e e
>  gelementsA :: arr () (Set e)
>  gverticesA :: arr () (Set e)
>  gedgesA    :: arr () (Set e)
>  gedgesStartingFromA :: arr e (Set e)
>  gedgesEndingToA :: arr e (Set e)
>  glinksA    :: arr () (Set (e,e,e))

>class (GraphArrow arr e) => ReversibleGraphArrow arr e where
>  ginverseA  :: arr e e
>  greversibleLinksA :: arr () (Set ((e,e),(e,e)))

>class (Monad m, Eq e) => GraphMonad m e where
>  {-# MINIMAL gsource, gtarget, gelements, gvertices, gedges, gedgesStartingFrom, gedgesEndingTo, glinks #-}
>  gisVertex :: e -> m Bool
>  gsource   :: e -> m e
>  gtarget   :: e -> m e
>  gelements :: m (Set e)
>  gvertices :: m (Set e)
>  gedges    :: m (Set e)
>  gedgesStartingFrom :: e -> m (Set e)
>  gedgesEndingTo     :: e -> m (Set e)
>  glinks :: m (Set (e,e,e))
>  gloops :: (Ord e) => m (Set e)
>  gisLoop :: e -> m Bool
>  gisLoop e = gsource e >>= \s -> gtarget e >>= \t -> return (s == t)
>  gisVertex e = gsource e >>= \s -> return (e == s)
>  gisEdgeBetween :: e -> e -> e -> m Bool
>  gisEdgeBetween = defaultGisEdgeBetween
>  gloops = defaultGloops

>defaultGloops :: (Ord e, GraphMonad m e) => m (Set e)
>defaultGloops = do
>            edges <- gedges 
>            pairs <- mapSetM checkLoop edges
>            return $ Set.map fst $ Set.filter snd pairs
>  where checkLoop e = gisLoop e >>= \el -> return (e,el)


>class (GraphMonad m e) => ReversibleGraphMonad m e where
>  {-# MINIMAL ginverse, greversibleLinks #-}
>  ginverse :: e -> m e
>  greversibleLinks :: m (Set ((e,e),(e,e)))
>  goneLaneLoops :: (Ord e) => m (Set e)
>  gisOneLaneLoop :: e -> m Bool
>  gisOneLaneLoop = defaultGisOneLaneLoop
>  goneLaneLoops = defaultOneLaneLoops

>defaultOneLaneLoops :: (Ord e, ReversibleGraphMonad m e) => m (Set e)
>defaultOneLaneLoops = do
>   edges <- gedges
>   pairs <- mapSetM checkOneLaneLoop edges
>   return $ Set.map fst $ Set.filter snd pairs
> where checkOneLaneLoop e = gisLoop e >>= \el -> return (e,el)


>defaultGisEdgeBetween :: (GraphMonad m e) => e -> e -> e -> m Bool
>defaultGisEdgeBetween e x y = do
>   x' <- gsource e
>   y' <- gtarget e
>   return (x == x' && y == y')


>defaultGisOneLaneLoop :: (Eq e, ReversibleGraphMonad m e) => e -> m Bool
>defaultGisOneLaneLoop e = do
>     re <- ginverse e
>     l <- gisLoop e
>     return $ l && e == re

>defaultGisReversibleEdgeBetween :: (Eq e, ReversibleGraphMonad m e) => e -> e -> e -> m Bool
>defaultGisReversibleEdgeBetween e x y = do
>   xy <- gisEdgeBetween e x y
>   e' <- ginverse e
>   yx <- gisEdgeBetween e' x y
>   return (xy || yx)

