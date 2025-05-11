>{-# LANGUAGE Safe,FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeOperators, TypeFamilies, DeriveGeneric, DeriveDataTypeable #-}
>{-# LANGUAGE OverloadedStrings, UndecidableInstances #-}
>{-# LANGUAGE GADTs, LambdaCase, Arrows #-}
>{-# LANGUAGE ScopedTypeVariables #-}
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
>import Data.Binary
>import Data.Set (Set)
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow hiding ((<+>))
>import Control.Monad.Reader
>import qualified Data.Graph.Inductive.Graph as HGraph
>import Data.Typeable
>import GHC.Generics hiding ((:*:), (:+:))
>import Data.Map (Map)
>import qualified Data.Map as Map
>import qualified Data.Text as T
>import Data.Monoid
>import Math.Tools.Set
>import Math.Tools.CoFunctor
>import Math.Tools.NaturalTransformation
>import Math.Tools.OppositeArrow
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Math.Tools.Median
>import Math.Tools.Isomorphism
>import Math.Tools.Visitor
>import Math.Graph.GraphMonoid
>import Math.Graph.Interface
>import qualified Math.Graph.Action as Action
>import Math.Graph.Action ((:<-:))

>import Math.Number.Group
>import Math.Graph.Show
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
>--
>data Graph m a = Graph {
> vertices :: !(Set a),
> edges    :: !(Set a),
> action_endomorphism :: m Bool Bool -> Endo a }
>  deriving (Typeable, Generic)

>elements :: (Ord a) => Graph m a -> Set a
>elements (Graph v e _) = Set.union v e

>action :: Graph m a -> a -> m Bool Bool -> a
>action g x m = action_endomorphism g m `appEndo` x

>domAction :: (GraphMonoid m Bool) => Graph m a -> a -> a
>domAction g a = action g a gdom

>codAction :: (GraphMonoid m Bool) => Graph m a -> a -> a
>codAction g a = action g a gcod

>invAction :: (ReversibleGraphMonoid m Bool) => Graph m a -> a -> a
>invAction g a = action g a gnot

>setAction :: (Ord a) => Graph m a -> Set a -> m Bool Bool -> Set a
>setAction g s m = Set.map (\x -> action g x m) s

>listAction :: (Ord a) => Graph m a -> [a] -> m Bool Bool -> [a]
>listAction g s m = map (\x -> action g x m) s

>action_rep :: Graph m a -> Endo a :<-: m Bool Bool
>action_rep = Action.Action . action_endomorphism 

>inverseImageG :: (m Bool Bool -> n Bool Bool) -> Graph n a -> Graph m a
>inverseImageG f (Graph v e act) = Graph v e (act . f)

>instance (Eq a, Universe (m Bool Bool)) => Eq (Graph m a) where
>  g@(Graph v e _) == g'@(Graph v' e' _) = v == v' && e == e' &&
>    and [ action g ee i == action g' ee i | i <- allElements, ee <- Set.toList e]

>instance (Ord a, Universe (m Bool Bool)) => Ord (Graph m a) where
>  g@(Graph v e _) <= g'@(Graph v' e' _) = v <= v' && e <= e' &&
>    and [ action g ee i <= action g' ee' i | i <- allElements, (ee,ee') <- zip (Set.toList e) (Set.toList e')]

instance (MedianAlgebra a, Universe (m a a)) => MedianAlgebra (Graph m a) where
   med g@(Graph e _) g'@(Graph e' _) g''@(Graph e'' _) =
     Graph (map3 med e e' e'') (\m -> Endo $ \s -> 
                                  med (action g s m)
                                      (action g' s m)
                                      (action g'' s m))
    where map3 f x y z = Set.map (\ (a,(b,c)) -> f a b c) $
                            zipSet x (zipSet y z)

>emptyG :: Graph m b
>emptyG = Graph Set.empty Set.empty $ const $ Endo $ const (error "empty graph")

>vertexG :: a -> Graph m a
>vertexG x = Graph (Set.singleton x) Set.empty vertexEndo

>verticesFromSetG :: Set a -> Graph m a
>verticesFromSetG s = Graph s Set.empty vertexEndo

>verticesG :: (Ord a) => [a] -> Graph m a
>verticesG = verticesFromSetG . Set.fromList

>edgeG :: (Ord a) => a -> a -> a -> Graph Three a
>edgeG e x y = Graph (Set.fromList [x,y]) (Set.singleton e) (edgesEndo [(e,(x,y))])

vectorG :: Set a -> Graph Lin a
vectorG v = Graph v $ \ (Lin m) -> Endo $ \w -> m <<*> w

>binopG :: Set b -> Set b -> (arr Bool Bool -> b -> b) -> Graph arr b
>binopG v e op = Graph v e $ \i -> Endo $ \j -> op i j

outerG :: (Ord a) => Graph (,) a -> Graph (,) a -> Graph (,) (a,a)
outerG (Graph e act) (Graph e' act') = Graph ee ract
   where ee = joinSet $ outerSet (,) e e'
         ract (m,n) = Endo $ \ s -> zipSet (act m `appEndo` s) (act' n `appEndo` s)

>data EitherMonoid m a b where
>   EitherMonoid :: m Bool Bool -> m Bool Bool -> EitherMonoid m a a

>discriminatedUnionG :: (Ord a, Ord b)
>                    => Graph m a -> Graph m b -> Graph (EitherMonoid m) (Either a b)
>discriminatedUnionG (Graph v e act) (Graph v' e' act') = Graph vv ee ract
>   where ee = Set.map Left e `Set.union` Set.map Right e'
>         vv = Set.map Left v `Set.union` Set.map Right v'
>         ract (EitherMonoid m n) = Endo $ \case
>            (Left a) -> Left $ act m `appEndo` a
>            (Right b) -> Right $ act' n `appEndo` b
> 
>  -- \s -> Set.map Left (act m `appEndo` (Set.map (\ (Left x) -> x) $ Set.filter isLeft s))
>  --                                          `Set.union` Set.map Right (act' n `appEndo` (Set.map (\ (Right y) -> y) $ Set.filter isRight s))

>intersectionG :: (Ord a) => Graph m a -> Graph m a -> Graph m a
>intersectionG g1 g2 = Graph ev ee ract
>  where ev = vertices g1 `Set.intersection` vertices g2
>        ee = edges g1 `Set.intersection` edges g2
>        ract m = Endo $ \z -> let e1 = action g1 z m
>                                  e2 = action g2 z m
>                               in if e1 == e2 then e1 else error "intersectionG: ambiguous"

>complementG :: (ReversibleGraphMonoid m Bool) => Graph m a -> Graph m a
>complementG (Graph v e act) = Graph v e ract
>   where ract m = act (m `mappend` gnot)

>instance (Ord a, Show a, Show (m Bool Bool)) => Semigroup (Graph m a) where
>   (<>) = unionG

>instance (Ord a, Show a, Show (m Bool Bool)) => Monoid (Graph m a) where
>   mempty = emptyG
>   mappend = (<>)

>instance (Ord a, Show a, Show (m Bool Bool), ReversibleGraphMonoid m Bool) => SetLike (Graph m a) where
>   sintersection = intersectionG
>   sunion = unionG
>   scomplement = complementG

>productG :: (Ord a, Ord b) => Graph m a -> Graph m b -> Graph m (a,b)
>productG g1 g2 = Graph ev ee (Endo . act)
>  where ev = joinSet (pairSet (vertices g1) (vertices g2))
>        ee = joinSet (pairSet (edges g1) (edges g2))
>        act m (a,b) = (action g1 a m, action g2 b m)

>-- | union of two graphs.
>-- Notice: calls 'error' if two edges with same name point to different vertices.
>-- Notice: calls 'error' if action is called with input that is not in vertices or edges.
>unionG :: (Ord a, Show a, Show (m Bool Bool)) => Graph m a -> Graph m a -> Graph m a
>unionG g1 g2 = Graph ev ee (Endo . f)
>  where ee = edges g1 `Set.union` edges g2
>        ev = vertices g1 `Set.union` vertices g2
>        f m z | z `Set.member` elements g1 && z `Set.member` elements g2 = 
>                    let res1 = action g1 z m
>                        res2 = action g2 z m
>                     in if res1 == res2 then res1
>                            else error $ "unionG: cannot unify: " ++ show res1
>                                                                  ++ " with: " ++ show res2
>                                                                  ++ " action: " ++ show m ++ "(" ++ show z ++ ")"
>              | z `Set.member` elements g1 = action g1 z m
>              | z `Set.member` elements g2 = action g2 z m
>              | otherwise   = error $ "unionG: element not in graph: " ++ show z

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
>edgesFromMapG edgemap = Graph (Set.fromList (map fst emap ++ map snd emap))
>                              (Map.keysSet edgemap)
>                              (edgesFromMapEndo edgemap)
>    where emap = Map.elems edgemap

>edgesG :: (Ord a) => [(a,(a,a))] -> Graph Three a
>edgesG lst = Graph (Set.fromList $ map (fst . snd) lst ++ map (snd . snd) lst) 
>                   (Set.fromList (map fst lst))
>                   (edgesEndo lst)

>-- | given a list of (v,e), the loopG contains looped edges of the form e : v -> v.
>loopG :: (Ord a) => [(a,a)] -> Graph Three a
>loopG lst = Graph (Set.fromList $ map fst lst) (Set.fromList $ map snd lst) (loopEndo lst)

>-- | converts from undirected graph to directed graph
>reversibleToDirectedG :: Graph Four a -> Graph Three a
>reversibleToDirectedG = inverseImageG threeToFour

>-- | inverts all edges
>inverseGraphG :: (Group (m Bool Bool)) => Graph m a -> Graph m a
>inverseGraphG = inverseImageG ginvert

>-- | mapG uses an isomorphism of graph elements to permute a graph.
>mapG :: (Ord a, Ord b) => a :==: b -> (Graph m a) :==: (Graph m b)
>mapG f =  (\ (Graph v e act) -> Graph (Set.map (f =<) v) (Set.map (f =<) e) $
>                              \m -> Endo $ isomorphismEpimorphism f . appEndo (act m) . isomorphismSection f)
>      <-> (\ (Graph v e act) -> Graph (Set.map ((invertA f) =<) v) (Set.map ((invertA f) =<) e) $
>                              \m -> Endo $ isomorphismSection f . appEndo (act m) . isomorphismEpimorphism f)

>reversibleEdgeG :: (Ord a) => a -> a -> a -> a -> Graph Four a
>reversibleEdgeG e re x y = reversibleEdgesG [((e,re),(x,y))]

>-- | 'edges' argument has inputs 'from' and 'to' and should return names of
>-- forward and backward edges.
>reversibleCompleteG :: (Ord a) => [a]  -> (a -> a -> (a,a)) -> Graph Four a
>reversibleCompleteG vertices edges = reversibleEdgesG $
>   [(edges from to,(from,to)) | from <- vertices, to <- vertices ]

>-- | The input data contains ((edge,reverse-edge),(startNode,endNode))
>reversibleEdgesG :: (Ord a) => [((a,a),(a,a))] -> Graph Four a
>reversibleEdgesG lst = Graph (Set.fromList $ sources ++ targets)
>                             (Set.fromList $ edges ++ reversedEdges)
>                             (reversibleEdgesEndo lst)
>  where edges   = map (fst . fst) lst
>        reversedEdges = map (snd . fst) lst
>        sources = map (fst . snd) lst
>        targets = map (snd . snd) lst         

>-- | For a list of (v,e), this produces a reversible graph of e : v -> v
>oneLaneLoopG :: (Ord a) => [(a,a)] -> Graph Four a
>oneLaneLoopG lst = reversibleEdgesG $ map (\ (v,e) -> ((e,e),(v,v))) lst
>
>-- | For a list of (v, (e1,e2)), this produces a reversible graph of (e1,e2) : v -> v
>--  where e2 is the inverse edge of e1.
>twoLaneLoopG :: (Ord a) => [(a,(a,a))] -> Graph Four a
>twoLaneLoopG lst = reversibleEdgesG $ map (\ (v,(e1,e2)) -> ((e1,e2),(v,v))) lst


>subobjectClassifierGraphG :: Graph Four Text
>subobjectClassifierGraphG = reversibleEdgesG [
>                                    (("leave","enter"),("in","out")),
>                                    (("foray","foray"),("in","in"))]


>instance (Binary a, Ord a) => Binary (Graph Three a) where
>  put g = do
>    let lnks = Set.map (\e -> (e,g `domAction` e, g `codAction` e)) $ edges g
>    put (vertices g)
>    put lnks
>  get = do
>    (vertices :: Set a) <- get
>    (edges :: Set (a,a,a)) <- get
>    return $ edgesG $ map (\ (a,b,c) -> (a,(b,c))) $ Set.toList edges


>instance (Ord a, PpShow a) => Show (Graph Three a) where { show = pPrint }
>instance (Ord a, PpShow a) => Show (Graph Four a) where { show = pPrint }

>instance (Ord a, PpShow a) => PpShow (Graph Three a) where
>  pp g = maybe base veprint $ let
>           v = vertices g
>           e = edges g
>           lnks = Set.map (\e -> (e,g `domAction` e, g `codAction` e)) e
>         in return $ (Set.toList v, map eprint $ Set.toList lnks)

>instance (Ord a, PpShow a) => PpShow (Graph Four a) where
>  pp g = maybe base veprint $ return $ (Set.toList v, map bidirectionalEprint $ Set.toList lnks)
>   where v = vertices g
>         lnks = Set.map (\e -> ((e,g `invAction` e),(g `domAction` e, g `codAction` e))) $ edges g
>         lnks' = fst $ includeExcludeEdges lnks
>         includeExcludeEdges = Set.fold (\ ((e,re),(x,y)) (include,exclude) 
>                                          -> if not $ e `Set.member` exclude then
>                                               (Set.insert ((e,re),(x,y)) include,
>                                                Set.insert re exclude)
>                                              else (include,Set.insert re exclude)) (Set.empty,Set.empty)

>instance (Monad m, Ord e, GraphMonoid mon Bool) => GraphMonad (ReaderT (Graph mon e) m) e where
>   gisVertex e = ask >>= \g -> return $ e == g `codAction` e
>   gsource e = ask >>= \g -> return $ g `domAction` e
>   gtarget e = ask >>= \g -> return $ g `codAction` e
>   gelements = ask >>= \g -> return $ elements g
>   gvertices = ask >>= \g -> return $ vertices g
>   gedges = ask >>= \g -> return $ edges g
>   gedgesStartingFrom x = ask >>= \g -> return $ Set.filter (\e -> g `domAction` e == x) $ edges g
>   gedgesEndingTo x = ask >>= \g -> return $ Set.filter (\e -> g `codAction` e == x) $ edges g
>   glinks = ask >>= \g -> return $ Set.map (\e -> (e,g `domAction` e, g `codAction` e)) $ edges g

>instance (Monad m, Ord e, ReversibleGraphMonoid mon Bool) => ReversibleGraphMonad (ReaderT (Graph mon e) m) e where
>   ginverse e = ask >>= \g -> return $ g `invAction` e
>   greversibleLinks = ask >>= \g -> return $ fst $ includeExcludeEdges $ edgedata g
>     where edgedata g = Set.map (\e -> ((e,g `invAction` e), (g `domAction` e, g `codAction` e))) $ edges g
>           includeExcludeEdges = Set.fold (\ ((e,re),(x,y)) (include,exclude) 
>                                          -> if not $ e `Set.member` exclude then
>                                               (Set.insert ((e,re),(x,y)) include,
>                                                Set.insert re exclude)
>                                              else (include,Set.insert re exclude)) (Set.empty,Set.empty)

>instance (Ord a, GraphMonoid m Bool) => Visitor (Graph m a) where
>   data Fold (Graph m a) b = GraphFold {
>     graphfold_initial :: b,
>     graphfold_vertex  :: a -> b -> b,
>     graphfold_edge    :: a -> a -> a -> b -> b
>   }
>   visit z g = let
>      e = Set.map (\e -> (e,g `domAction` e, g `codAction` e)) (edges g)
>      v = vertices g
>      esf = SetFold (\ (a,b,c) r -> graphfold_edge z a b c r) (graphfold_initial z)
>      eres = visit esf e
>      vsf = SetFold (graphfold_vertex z) eres
>    in visit vsf v
