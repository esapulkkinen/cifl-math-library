>{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
>-- | This module supports digraphs and reversible graphs with clearly separated vertex and edge sets.
>-- This is often needed in practical applications of graphs.
>module Math.Graph.Digraph where
>
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid
>import Math.Graph.Interface
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Data.Text (Text)
> 
>type Digraph v e = Graph Three (GraphElem v e)
>type ReversibleGraph v e = Graph Four (GraphElem v e)
>type GenericGraph mon v e = Graph mon (GraphElem v e)

>instance (Ord v, Ord e) => DigraphFactory (Graph Three (GraphElem v e)) v e where
>   vertexGraph = vertexGE
>   edgeGraph = edgeGE
>   verticesFromSetGraph = verticesFromSetGE
>   verticesGraph = verticesGE
>   completeGraph = completeGE
>   edgeGraphFromMap = edgesFromMapGE
>   edgesGraph = edgesGE
>   loopGraph = loopGE

instance (Ord v, Ord e) => DigraphFactory (Graph Four (GraphElem v e)) v e where
    vertexGraph = vertexGE
    edgeGraph e v1 v2 = reversibleEdgeGE e e v1 v2
    verticesFromSetGraph = verticesFromSetGE
    verticesGraph = verticesGE
    completeGraph lst f = reversibleCompleteGE lst (\v1 v2 -> let x = f v1 v2 in (x,x))
    edgeGraphFromMap m = edgesGraph $ Map.toList m
    edgesGraph lst = reversibleEdgesGE $ map (\ (e,(v1,v2)) -> ((e,e),(v1,v2))) lst
    loopGraph = reversibleOneLaneLoopGE

>instance (Ord v, Ord e) => ReversibleGraphFactory (Graph Four (GraphElem v e)) v e where
>   reversibleVertexGraph = vertexGE
>   reversibleVerticesGraph = verticesGE
>   reversibleVerticesFromSetGraph = verticesFromSetGE
>   reversibleEdgeGraph = reversibleEdgeGE
>   reversibleCompleteGraph = reversibleCompleteGE
>   reversibleEdgesGraph = reversibleEdgesGE
>   reversibleLoopGraph = reversibleLoopGE

>vertexGE :: v -> GenericGraph m v e
>vertexGE x = Graph (Set.singleton (Vertex x)) Set.empty vertexEndo

>verticesFromSetGE :: (Ord e, Ord v) => Set v -> GenericGraph m v e
>verticesFromSetGE s = Graph (Set.map Vertex s) Set.empty vertexEndo

>verticesGE :: (Ord v, Ord e) => [v] -> GenericGraph m v e
>verticesGE = verticesFromSetGE . Set.fromList

>edgeGE :: (Ord v, Ord e) => e -> v -> v -> Digraph v e
>edgeGE e x y = Graph (Set.fromList [Vertex x, Vertex y]) (Set.singleton (Edge e)) $
>   edgesEndoE [(e,(x,y))]

>completeGE :: (Ord v, Ord e) => [v] -> (v -> v -> e) -> Digraph v e
>completeGE vertices edge = edgesGE [(edge x y, (x,y))
>                                   | x <- vertices, y <- vertices]

>edgesFromMapGE :: (Ord e, Ord v) => Map e (v,v) -> Digraph v e
>edgesFromMapGE edgemap = Graph es ks (edgesFromMapEndoE edgemap)
>    where ks = Set.map Edge $ Map.keysSet edgemap
>          es = Set.fromList (map (Vertex . fst) emap ++ map (Vertex . snd) emap)
>          emap = Map.elems edgemap

>edgesGE :: (Ord v, Ord e) => [(e,(v,v))] -> Digraph v e
>edgesGE lst = Graph (Set.fromList $ map (Vertex . fst . snd) lst ++ map (Vertex . snd . snd) lst)
>                    (Set.fromList $ map (Edge . fst) lst)
>                    (edgesEndoE lst)

>loopGE :: (Ord v, Ord e) => [(v,e)] -> Digraph v e
>loopGE lst = Graph (Set.fromList $ map (Vertex . fst) lst)
>                   (Set.fromList $ map (Edge . snd) lst)
>                   (loopEndoE lst)

>reversibleOneLaneLoopGE :: (Ord v, Ord e) => [(v,e)] -> ReversibleGraph v e
>reversibleOneLaneLoopGE lst = Graph (Set.fromList $ map (Vertex . fst) lst)
>                                    (Set.fromList $ map (Edge . snd) lst)
>                                    (reversibleOneLaneLoopEndoE lst)

>reversibleLoopGE :: (Ord v, Ord e) => [(v,e,e)] -> ReversibleGraph v e
>reversibleLoopGE lst = Graph (Set.fromList $ map (Vertex . fst3) lst)
>                             (Set.fromList $ map (Edge . snd3) lst ++ map (Edge . thr3) lst)
>                             (reversibleLoopEndoE lst)
>   where fst3 ~(a,_,_) = a
>         snd3 ~(_,b,_) = b
>         thr3 ~(_,_,c) = c


>reversibleEdgeGE :: (Ord v, Ord e) => e -> e -> v -> v -> ReversibleGraph v e
>reversibleEdgeGE e re x y = reversibleEdgesGE [((e,re),(x,y))]

>reversibleCompleteGE :: (Ord v, Ord e) => [v] -> (v -> v -> (e,e)) -> ReversibleGraph v e
>reversibleCompleteGE vertices edges = reversibleEdgesGE $
>   [(edges from to, (from, to)) | from <- vertices, to <- vertices]

>reversibleEdgesGE :: (Ord v, Ord e) => [((e,e),(v,v))] -> ReversibleGraph v e
>reversibleEdgesGE lst = Graph (Set.fromList (sources ++ targets)) (Set.fromList (edges ++ reversedEdges)) (reversibleEdgesEndoE lst)
>   where edges = map (Edge . fst . fst) lst
>         reversedEdges = map (Edge . snd . fst) lst
>         sources = map (Vertex . fst . snd) lst
>         targets = map (Vertex . snd . snd) lst

>subobjectClassifierGraphGE :: ReversibleGraph Text Text
>subobjectClassifierGraphGE = reversibleEdgesGE [
>    (("leave","enter"),("in","out")),
>    (("foray","foray"),("in","in"))]
