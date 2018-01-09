>{-# LANGUAGE Safe #-}
>module Math.Graph.Algorithms where
>import Data.Maybe
>import Control.Monad.State
>import qualified Data.Set as Set
>import Data.Set (Set)
>import qualified Data.Map as Map
>import Data.Map (Map)
>import qualified Math.Tools.Queue as Q
>import Math.Tools.Queue (Queue)
>import Data.Tree
>import Math.Tools.Set
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid
>import Math.Graph.GraphMonad
>import Math.Graph.InGraphMonad

>-- | Compute depth first search on a graph, starting from a given root vertex.
>dfsM :: (Monad m, Ord a, GraphMonoid mon) => a -> InGraphM mon a m [a]
>dfsM root = evalStateT (dfsIntM root) Set.empty

>dfsIntM :: (Monad m, Ord a, GraphMonoid mon) => a -> StateT (Set a) (InGraphM mon a m) [a]
>dfsIntM root = do
>   excludeSet <- get
>   if root `Set.member` excludeSet then return [] else do
>     let newExcludes = Set.insert root excludeSet
>     put newExcludes
>     nodeset <- lift $ nodesLinkedFromM root
>     nextLevel <- mapM dfsIntM (Set.toList nodeset)
>     return $ root : concat nextLevel

>-- | <https://en.wikipedia.org/wiki/Breadth-first_search Breadth first search>:
>-- bfsM produces a map from child to parent nodes in a bfs tree.

>bfsM :: (Monad m, Ord n, GraphMonoid mon) => n -> InGraphM mon n m (Map n n)
>bfsM root = evalStateT bfsIntM (Q.singleton root, Map.singleton root 0)

>bfsIntM :: (Monad m, GraphMonoid mon, Ord n) => StateT (Queue n, Map n Integer) (InGraphM mon n m) (Map n n)
>bfsIntM = do
>   (q,distmap) <- get
>   case Q.dequeue q of
>     Just (current,q') -> do
>         nodeset <- lift $ nodesLinkedFromM current
>         let cdist = succ $ fromJust $ Map.lookup current distmap
>         let newNodes = Set.filter (\n -> isNothing (Map.lookup n distmap)) nodeset
>         let distmapchange = Map.fromSet (const cdist) newNodes
>         let distmap' = Map.union distmap distmapchange
>         let q'' = foldr Q.enqueue q' newNodes
>         put (q'',distmap')
>         r <- bfsIntM
>         return $ Map.fromSet (const current) newNodes `Map.union` r
>     Nothing -> return (Map.empty)


>-- | Spanning tree rooted at a given graph vertex.
>spanningTreeM :: (Monad m, GraphMonoid mon, Ord n) => n -> InGraphM mon n m (Tree n)
>spanningTreeM n = evalStateT (spanningTreeIntM n) (Set.empty)
>
>-- | Spanning forest rooted at the given list of vertices.
>spanningForestM :: (Monad m, GraphMonoid mon, Ord n) => [n] -> InGraphM mon n m (Forest n)
>spanningForestM lst = evalStateT (spanningForestIntM lst) (Set.empty)

>spanningTreeIntM :: (Monad m, GraphMonoid mon, Ord n) => n -> StateT (Set n) (InGraphM mon n m) (Tree n)
>spanningTreeIntM n = do
>   modify (Set.insert n)
>   excludeSet' <- get
>   nodeset <- lift $ nodesLinkedFromM n
>   modify (Set.union nodeset)
>   forest  <- spanningForestIntM (Set.toList (nodeset Set.\\ excludeSet'))
>   return $ Node n forest

>spanningForestIntM :: (Monad m, GraphMonoid mon, Ord n)
>                => [n] -> StateT (Set n) (InGraphM mon n m) (Forest n)
>spanningForestIntM lst = mapM spanningTreeIntM lst
