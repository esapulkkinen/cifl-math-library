>{-# LANGUAGE Safe,FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeOperators, TypeFamilies, DeriveGeneric, DeriveDataTypeable #-}
>{-# LANGUAGE OverloadedStrings, UndecidableInstances #-}
>{-# LANGUAGE GADTs #-}
>-- | Graph representation as a set with action of a monoid.
>-- See Lawvere,Rosebrugh: Sets for Mathematics for details.
>--
>-- This module implements graphs using a representation as a set with an action of a monoid.
>-- 
>-- For directed graph, the monoid 'Three' is used, which represents operations of id,source and target
>--
>-- For undirected graph, the monoid 'Four' is used, which represents operations of id,source, target and inverse.
>--
>-- See "Math.Graph.InGraphMonad" for how to read graphs in monadic context
>-- 
>-- See "Math.Graph.InGraphA" for how to read graphs in arrow context
>-- 
>module Math.Graph.Reversible where
>import qualified Data.Set as Set
>import Data.Set (Set)
>import Data.Text (Text)
>import Data.Tree
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow hiding ((<+>))
>import Control.Monad.Reader
>import qualified Data.Graph.Inductive.Graph as HGraph
>import Data.Typeable
>import GHC.Generics hiding ((:*:), (:+:))
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Data.Monoid
>import Math.Tools.Set
>import Math.Tools.CoFunctor
>import Math.Tools.NaturalTransformation
>import Math.Tools.OppositeArrow
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Math.Tools.Median
>import Math.Tools.Isomorphism
>import Math.Graph.GraphMonoid
>import Math.Graph.Interface

>import Math.Number.Group

>import Math.Graph.Action ((:<-:))
>import qualified Math.Graph.Action as Action
>import Math.Matrix.Interface
>import Text.PrettyPrint ((<+>))
>import Math.Tools.PrettyP (PpShow,pp, pPrint)
>import qualified Math.Tools.PrettyP as PrettyP

>-- | Graph representation containing a set of elements.
>-- an element of a graph is either an edge or a vertex.
>--
>-- the action_endomorphism allows obtaining source vertex, target vertex and inverse edge of an element
>-- See 'Math.Graph.GraphMonoid' for details.
>--
>-- Note that the monoid data structure contains only names of the monoid elements -
>-- the action_endomorphism is then used to convert it to actual operation.
>data Graph m a = Graph {
> elements :: !(Set a),
> action_endomorphism :: m a a -> Endo a }
>  deriving (Typeable, Generic)

>idG :: (Monoid m) => Graph GEndo m
>idG = monoidG (Set.singleton mempty)

>monoidG :: Set a -> Graph GEndo a
>monoidG s = Graph s (\ (GEndo x) -> x)

>action :: Graph m a -> a -> m a a -> a
>action g x m = action_endomorphism g m `appEndo` x

>action_rep :: Graph m a -> Endo a :<-: m a a
>action_rep = Action.Action . action_endomorphism 

>inverseImageG :: (m a a -> n a a) -> Graph n a -> Graph m a
>inverseImageG f (Graph s act) = Graph s (act . f)

>instance (Eq a, Universe (m a a)) => Eq (Graph m a) where
>  g@(Graph s _) == g'@(Graph s' _) = s == s' &&
>    and [ action g sx i == action g' sx i | i <- all_elements, sx <- Set.toList s]

>instance (Ord a, Universe (m a a)) => Ord (Graph m a) where
>  g@(Graph s _) <= g'@(Graph s' _) = s <= s' &&
>    and [ action g sx i <= action g' sy i | i <- all_elements, 
>          sx <- Set.toList s,
>          sy <- Set.toList s']

>instance (MedianAlgebra a, Universe (m a a)) => MedianAlgebra (Graph m a) where
>   med g@(Graph e _) g'@(Graph e' _) g''@(Graph e'' _) =
>     Graph (map3 med e e' e'') (\m -> Endo $
>                                 \a -> med (action g a m)
>                                      (action g' a m)
>                                      (action g'' a m))
>    where map3 f x y z = Set.map (\ (a,(b,c)) -> f a b c) $
>                            zipSet x (zipSet y z)

>emptyG :: Graph m b
>emptyG = Graph Set.empty $ const $ Endo $ const (error "empty graph")

>vertexG :: a -> Graph m a
>vertexG x = Graph (Set.singleton x) vertexEndo

>verticesFromSetG :: Set a -> Graph m a
>verticesFromSetG s = Graph s vertexEndo

>verticesG :: (Ord a) => [a] -> Graph m a
>verticesG = verticesFromSetG . Set.fromList

>edgeG :: (Ord a) => a -> a -> a -> Graph Three a
>edgeG e x y = Graph (Set.fromList [e,x,y]) (edgesEndo [(e,(x,y))])


vectorG :: Set a -> Graph Lin a
vectorG v = Graph v $ \ (Lin m) -> Endo $ \w -> m <<*> w

>appG :: Set a -> Graph (->) a
>appG p = Graph p Endo

>binopG :: Set b -> (arr b b -> b -> b) -> Graph arr b
>binopG x op = Graph x $ \i -> Endo $ \j -> op i j

>outerG (Graph e act) (Graph e' act') = Graph ee ract
>   where ee = joinSet $ outerSet (,) e e'
>         ract (m,n) = Endo $ \(a,b) -> (act m `appEndo` a, act' n `appEndo` b)

>data EitherMonoid m a b where
>   EitherMonoid :: m a a -> m b b -> EitherMonoid m (Either a b) (Either a b)

>discriminatedUnionG :: (Ord a, Ord b)
>                    => Graph m a -> Graph m b -> Graph (EitherMonoid m) (Either a b)
>discriminatedUnionG (Graph e act) (Graph e' act') = Graph ee ract
>   where ee = Set.map Left e `Set.union` Set.map Right e'
>         ract (EitherMonoid m n) = Endo $ either (\a -> Left $ act m `appEndo` a)
>                                                 (\b -> Right $ act' n `appEndo` b)

>intersectionG :: (Ord a) => Graph m a -> Graph m a -> Graph m a
>intersectionG g1 g2 = Graph ee ract
>  where ee = elements g1 `Set.intersection` elements g2
>        ract m = Endo $ \z -> let e1 = action_endomorphism g1 m `appEndo` z
>                                  e2 = action_endomorphism g2 m `appEndo` z
>                               in if e1 == e2 then e1 else error "intersectionG: ambiguous"

>complementG :: (ReversibleGraphMonoid m a) => Graph m a -> Graph m a
>complementG (Graph e act) = Graph e ract
>   where ract m = act (m `mappend` gnot)

>instance (Ord a, Show a) => Semigroup (Graph m a) where
>   (<>) = unionG

>instance (Ord a, Show a) => Monoid (Graph m a) where
>   mempty = emptyG
>   mappend = unionG

>instance (Ord a, Show a, ReversibleGraphMonoid m a) => SetLike (Graph m a) where
>   sintersection = intersectionG
>   sunion = unionG
>   scomplement = complementG

>unionG :: (Ord a, Show a) => Graph m a -> Graph m a -> Graph m a
>unionG g1 g2 = Graph ee (Endo . f)
>  where ee = elements g1 `Set.union` elements g2
>        f m z | z `Set.member` elements g1 && z `Set.member` elements g2 = 
>                    let res1 = action g1 z m
>                        res2 = action g2 z m
>                     in if res1 == res2 then res1
>                            else error $ "unionG: cannot unify:" ++ show res1 ++ "with:" ++ show res2
>              | z `Set.member` elements g1 = action g1 z m
>              | z `Set.member` elements g2 = action g2 z m
>              | otherwise   = error $ "unionG: element not in graph:" ++ show z

>-- | In a complete graph, every pair of vertices (x,y) has a single link.
>--
>-- @
>-- completeG \["1","2","3"\] \(++\) == [1, 2, 3] ; [11 = 1 -> 1, 12 = 1 -> 2, 13 = 1 -> 3, 21 = 2 -> 1,
>--                                              22 = 2 -> 2, 23 = 2 -> 3, 31 = 3 -> 1, 32 = 3 -> 2, 33 = 3 -> 3]
>-- @
>-- 
>--  Note though that edge names produced from pair of vertices by the function given must be unique,
>--  no overlaps are allowed, so it is required that for call 'completeG vertices edge'
>-- 
>--  prop> forall v,w : vertices . (v != v' || w != w') => edge v w != edge v' w'
>-- 
>completeG :: (Ord a) => [a] -> (a -> a -> a) -> Graph Three a
>completeG vertices edge = edgesG [(edge x y,(x,y))
>                                 | x <- vertices, y <- vertices ]

>edgesFromMapG :: (Ord a) => Map a (a,a) -> Graph Three a
>edgesFromMapG edgemap = Graph (Map.keysSet edgemap `Set.union` Set.fromList (map fst emap ++ map snd emap))
>                              (edgesFromMapEndo edgemap)
>    where emap = Map.elems edgemap

>edgesG :: (Ord a) => [(a,(a,a))] -> Graph Three a
>edgesG lst = Graph (Set.fromList (map fst lst ++ map (fst . snd) lst ++ map (snd . snd) lst))
>                   (edgesEndo lst)

>-- | given a list of (v,e), the loopG contains looped edges of the form e : v -> v.
>loopG :: (Ord a) => [(a,a)] -> Graph Three a
>loopG lst = Graph (Set.fromList (map fst lst ++ map snd lst)) (loopEndo lst)

>-- | converts from undirected graph to directed graph
>reversibleToDirectedG :: Graph Four a -> Graph Three a
>reversibleToDirectedG = inverseImageG three_to_four

>-- | inverts all edges
>inverseGraphG :: (Group (m a a)) => Graph m a -> Graph m a
>inverseGraphG = inverseImageG ginvert

>-- | mapG uses an isomorphism of graph elements to permute a graph.
>mapG :: (Arrow m, Ord a,Ord b) => a :==: b -> (Graph m a) :==: (Graph m b)
>mapG f =  (\ (Graph s act) -> Graph (Set.map (f =<) s) $
>                              \m -> amap f =< (act (arr (isomorphism_section f) . m . arr (isomorphism_epimorphism f))))
>      <-> (\ (Graph s act) -> Graph (Set.map ((invertA f) =<) s) $
>                              \m -> amap (invertA f) =< (act (arr (isomorphism_epimorphism f) . m . arr (isomorphism_section f))))

>reversibleEdgeG :: (Ord a) => a -> a -> a -> a -> Graph Four a
>reversibleEdgeG e re x y = reversibleEdgesG [((e,re),(x,y))]

>-- | 'edges' argument has inputs 'from' and 'to' and should return names of
>-- forward and backward edges.
>reversibleCompleteG :: (Ord a) => [a]  -> (a -> a -> (a,a)) -> Graph Four a
>reversibleCompleteG vertices edges = reversibleEdgesG $
>   [(edges from to,(from,to)) | from <- vertices, to <- vertices ]

>-- | The input data contains ((edge,reverse-edge),(startNode,endNode))
>reversibleEdgesG :: (Ord a) => [((a,a),(a,a))] -> Graph Four a
>reversibleEdgesG lst = Graph (Set.fromList (edges ++ reversedEdges ++ sources ++ targets))
>                             (reversibleEdgesEndo lst)
>  where edges   = map (fst . fst) lst
>        reversedEdges = map (snd . fst) lst
>        sources = map (fst . snd) lst
>        targets = map (snd . snd) lst         

>subobjectClassifierGraphG :: Graph Four Text
>subobjectClassifierGraphG = reversibleEdgesG [
>                                    (("leave","enter"),("in","out")),
>                                    (("foray","foray"),("in","in"))]
