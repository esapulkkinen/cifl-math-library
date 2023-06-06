>{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
>module Math.Graph.Digraph where
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid
>import Math.Graph.Interface
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Data.Text (Text)

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

>vertexGE :: v -> Graph m (GraphElem v e)
>vertexGE x = Graph (Set.singleton (Vertex x)) vertexEndo

>verticesFromSetGE :: (Ord e, Ord v) => Set v -> GenericGraph m v e
>verticesFromSetGE s = Graph (Set.map Vertex s) vertexEndo

>verticesGE :: (Ord v, Ord e) => [v] -> GenericGraph m v e
>verticesGE = verticesFromSetGE . Set.fromList

>edgeGE :: (Ord v, Ord e) => e -> v -> v -> Digraph v e
>edgeGE e x y = Graph (Set.fromList [Edge e, Vertex x, Vertex y]) $
>   edgesEndoE [(e,(x,y))]

>completeGE :: (Ord v, Ord e) => [v] -> (v -> v -> e) -> Digraph v e
>completeGE vertices edge = edgesGE [(edge x y, (x,y))
>                                   | x <- vertices, y <- vertices]

>edgesFromMapGE :: (Ord e, Ord v) => Map e (v,v) -> Digraph v e
>edgesFromMapGE edgemap = Graph (ks `Set.union` es)
>                               (edgesFromMapEndoE edgemap)
>    where ks = Set.map Edge $ Map.keysSet edgemap
>          es = Set.fromList (map (Vertex . fst) emap ++ map (Vertex . snd) emap)
>          emap = Map.elems edgemap

>edgesGE :: (Ord v, Ord e) => [(e,(v,v))] -> Digraph v e
>edgesGE lst = Graph (Set.fromList (map (Edge . fst) lst ++ map (Vertex . fst . snd) lst ++ map (Vertex . snd . snd) lst))
>                     (edgesEndoE lst)

>loopGE :: (Ord v, Ord e) => [(v,e)] -> Digraph v e
>loopGE lst = Graph (Set.fromList (map (Vertex . fst) lst ++ map (Edge . snd) lst)) (loopEndoE lst)

>reversibleEdgeGE :: (Ord v, Ord e) => e -> e -> v -> v -> ReversibleGraph v e
>reversibleEdgeGE e re x y = reversibleEdgesGE [((e,re),(x,y))]

>reversibleCompleteGE :: (Ord v, Ord e) => [v] -> (v -> v -> (e,e)) -> ReversibleGraph v e
>reversibleCompleteGE vertices edges = reversibleEdgesGE $
>   [(edges from to, (from, to)) | from <- vertices, to <- vertices]

>reversibleEdgesGE :: (Ord v, Ord e) => [((e,e),(v,v))] -> ReversibleGraph v e
>reversibleEdgesGE lst = Graph (Set.fromList (edges ++ reversedEdges ++ sources ++ targets)) (reversibleEdgesEndoE lst)
>   where edges = map (Edge . fst . fst) lst
>         reversedEdges = map (Edge . snd . fst) lst
>         sources = map (Vertex . fst . snd) lst
>         targets = map (Vertex . snd . snd) lst

>subobjectClassifierGraphGE :: ReversibleGraph Text Text
>subobjectClassifierGraphGE = reversibleEdgesGE [
>    (("leave","enter"),("in","out")),
>    (("foray","foray"),("in","in"))]
