>{-# LANGUAGE ImplicitParams, Rank2Types, ConstraintKinds #-}
>module Math.Graph.Implicit where
>-- ^ Dynamically scoped graph reader implementation for pure computations
>-- 
>-- This can be convenient in pure computations for reading graph data.
>-- This module reserves the implicit parameter name @ ?currentgraph @ for this use.
>-- 
>-- To introduce an implicit graph, use evalInGraphI operation.
>-- 
>-- If there are more than one graph, it's sometimes useful to give
>-- a separate name for them by using implicit parameters,
>-- e.g.  @(?graph1 `evalInGraphI` verticesI) `mappend` (?graph2 `evalInGraphI` verticesI)@.
>-- 
>-- Example:
>-- prop> completeG ["a","b","c"] (++) `evalInGraphI` edgesI == fromList ["aa","ab", "ac", "ba", "bb", "bc", "ca", "cb", "cc"]
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid
>import Data.Set (Set)
>import qualified Data.Set as Set
>
>type GraphCtx mon e = (?currentgraph :: Graph mon e, GraphMonoid mon)
>type ReversibleGraphCtx mon e = (GraphCtx mon e, ReversibleGraphMonoid mon)
>
>actI :: (GraphCtx mon e) => e -> mon -> e
>actI e m = action ?currentgraph e m

>evalInGraphI :: (GraphMonoid mon) => Graph mon e -> ((GraphCtx mon e) => res) -> res
>evalInGraphI g f = let ?currentgraph = g in f

>currentGraphI :: (GraphCtx mon e) => Graph mon e
>currentGraphI = ?currentgraph

>isTargetVertexI :: (GraphCtx mon e, Eq e) => e -> Bool
>isTargetVertexI e = targetI e == e

>isSourceVertexI :: (GraphCtx mon e, Eq e) => e -> Bool
>isSourceVertexI e = sourceI e == e

>isVertexI :: (GraphCtx mon e, Eq e) => e -> Bool
>isVertexI e = isTargetVertexI e && isSourceVertexI e

>isEdgeI :: (GraphCtx mon e, Eq e) => e -> Bool
>isEdgeI e = not (isVertexI e)

>hasPathI :: (GraphCtx mon e, Eq e) => [e] -> Bool
>hasPathI [c] = isEdgeI c
>hasPathI (c:d:cr) = isEdgeI c && targetI c == sourceI d && hasPathI (d:cr)
>hasPathI [] = False
>
>isEdgeBetweenI :: (GraphCtx mon e, Eq e) => e -> e -> e -> Bool
>isEdgeBetweenI e source target = sourceI e == source && targetI e == target

>isLoopI :: (GraphCtx mon e, Eq e) => e -> Bool
>isLoopI e = sourceI e == targetI e
>
>isOneLaneLoopI :: (ReversibleGraphCtx mon e, Eq e) => e -> Bool
>isOneLaneLoopI e = inverseI e == e

>sourceI :: (GraphCtx mon e) => e -> e
>sourceI e = action ?currentgraph e gdom

>targetI :: (GraphCtx mon e) => e -> e
>targetI e = action ?currentgraph e gcod

>inverseI :: (ReversibleGraphCtx mon e) => e -> e
>inverseI e = action ?currentgraph e gnot

>elementsI :: (GraphCtx mon e) => Set e
>elementsI = elements ?currentgraph

>verticesI :: (GraphCtx mon e, Eq e) => Set e
>verticesI = Set.filter (\a -> sourceI a == a) elementsI

>edgesI :: (GraphCtx mon e, Eq e) => Set e
>edgesI = Set.filter (\a -> sourceI a /= a) elementsI

>edgesStartingFromI :: (GraphCtx mon e, Eq e) => e -> Set e
>edgesStartingFromI x = Set.filter (\a -> sourceI a == x) edgesI

>edgesEndingToI :: (GraphCtx mon e, Eq e) => e -> Set e
>edgesEndingToI x = Set.filter (\a -> targetI a == x) edgesI

>nodesLinkedFromI :: (GraphCtx mon e, Ord e) => e -> Set e
>nodesLinkedFromI x = Set.map targetI $ edgesStartingFromI x

>linksI :: (GraphCtx mon e, Ord e) => Set (e,e,e)
>linksI = Set.map (\e -> (e, sourceI e, targetI e)) elementsI

>reversibleLinksI :: (ReversibleGraphCtx mon e, Ord e)
>   => Set ((e,e), (e,e))
>reversibleLinksI = fst $ includeExcludeEdges els
>   where els = Set.map (\e -> ((e,inverseI e), (sourceI e, targetI e))) elementsI
>         includeExcludeEdges s = Set.fold (\ ((e,re),(x,y)) (include,exclude)
>                         -> if not $ e `Set.member` exclude then
>                               (Set.insert ((e,re),(x,y)) include,
>                                Set.insert re exclude)
>                             else (include, Set.insert re exclude)) (Set.empty, Set.empty) s


