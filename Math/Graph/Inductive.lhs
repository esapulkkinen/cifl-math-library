>{-# LANGUAGE Safe #-}
>module Math.Graph.Inductive where
>import Data.Maybe
>import Data.Set (Set)
>import Data.Map (Map)
>import qualified Data.Set as Set
>import qualified Data.Map as Map
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid
>import qualified Data.Graph.Inductive.Graph as HGraph
>
>convertToInductiveGraph :: (Monad m, Ord a, HGraph.Graph gr, GraphMonoid mon) => InGraphM mon a m (gr a a)
>convertToInductiveGraph = do
>   vertices <- verticesM
>   let vertexlist = zip (Set.toList vertices) [0..]
>   let vertices' = Map.fromList vertexlist
>   edges <- linksM
>   let edges' = map (\(e,s,t) -> (vertices' Map.! s, vertices' Map.! t,e)) (Set.toList edges)
>   return $ HGraph.mkGraph (map (\ (a,b) -> (b,a)) vertexlist) edges'

Note that labels must be unique for both vertices and links when
converting from labeled graph.

>fromInductiveGraph :: (Ord a, HGraph.Graph gr) => gr a a -> Graph Three a
>fromInductiveGraph gr = edgesG $ map (\(s,t,e) -> (e,(fromJust (HGraph.lab gr s),
>                                                      fromJust (HGraph.lab gr t))))
>                               $ HGraph.labEdges gr

>fromUnlabeledGraph :: (HGraph.Graph gr) => gr () Int -> Graph Three Int
>fromUnlabeledGraph gr = edgesG $ map (\(s,t,e) -> (e,(s,t))) $ HGraph.labEdges gr
