>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
>-- | This module provides ability to inspect graph data in a specialized monad.
>--
>-- Examples:
>-- @
>-- let example1 = reversibleCompleteG ["a","b","c"] (\\a b -> (a ++ b, b ++ a))
>-- @
>--
>-- prop> show example1 == "([a, b, c] ; [aa = a <-> a, ba / ab = b <-> a, bb = b <-> b, ca / ac = c <-> a, cb / bc = c <-> b, cc = c <-> c])"
>-- 
>-- prop> example1 `inGraphM` edgesEndingToM "a" == return (fromList ["aa","ba","ca"])
>-- 
>-- prop> example1 `inGraphM` verticesM == return (fromList ["a","b","c"])
>--
>-- prop> example1 `inGraphM` edgesM == return (fromList ["aa","ab","ac","ba","bb","bc","ca","cb","cc"])
>--
>-- prop> example1 `inGraphM` do { x <- sourceM "ab" ; y <- targetM "ab" ; return (x,y) } == return ("a","b")
>module Math.Graph.InGraphMonad where
>import Math.Tools.I
>import Math.Tools.Visitor
>import Math.Tools.Set
>import qualified Data.Set as Set
>import Data.Set (Set)
>import Data.List
>import Control.Monad.Reader
>import Control.Monad.Writer
>import Control.Applicative
>import Control.Monad.Trans.Class
>import Math.Graph.GraphMonoid
>import Math.Graph.Interface
>import Math.Graph.Reversible

>newtype InGraphM mon e m a = InGraphM { runInGraphM :: ReaderT (Graph mon e) m a }

>instance MonadTrans (InGraphM mon e) where
>   lift x = InGraphM $ lift x

>instance (Functor m) => Functor (InGraphM mon e m) where
>   fmap f (InGraphM mr) = InGraphM $ fmap f mr

>instance (Applicative m) => Applicative (InGraphM mon e m) where
>  pure x = InGraphM $ pure x
>  (InGraphM mf) <*> (InGraphM mx) = InGraphM $ mf <*> mx

>instance (Applicative m, Semigroup a) => Semigroup (InGraphM mon e m a) where
>   (<>) = liftA2 (<>)

>instance (Monoid a, Applicative m) => Monoid (InGraphM mon e m a) where
>   mempty = pure mempty
>   mappend f g = liftA2 mappend f g

>instance (Monad m) => Monad (InGraphM mon e m) where
>  return = inGraphM_return
>  (>>=) = inGraphM_bind

>{-# INLINE inGraphM_bind #-}
>{-# INLINE inGraphM_return #-}
>inGraphM_bind :: (Monad m) => InGraphM mon e m a -> (a -> InGraphM mon e m b) -> InGraphM mon e m b
>inGraphM_bind ~(InGraphM m) f = InGraphM $ m >>= (runInGraphM . f)
>inGraphM_return :: (Monad m) => a -> InGraphM mon e m a
>inGraphM_return x = InGraphM (return x)

>instance (MonadIO m) => MonadIO (InGraphM mon e m) where
>  liftIO m = InGraphM $ liftIO m

>instance (MonadWriter w m) => MonadWriter w (InGraphM mon e m) where
>   writer (a,x) = InGraphM $ writer (a,x)
>   tell x = InGraphM $ tell x
>   listen (InGraphM x) = InGraphM $ listen x
>   pass (InGraphM f) = InGraphM (pass f)

>instance (Monad m) => MonadReader (Graph mon e) (InGraphM mon e m) where
>   ask = currentGraphM
>   local f (InGraphM x) = InGraphM (local f x)
>   reader f = InGraphM (reader f)

>inGraphM :: Graph mon e -> InGraphM mon e m a -> m a
>inGraphM g ~(InGraphM m) = runReaderT m g

>currentGraphM :: (Monad m) => InGraphM mon e m (Graph mon e)
>currentGraphM = InGraphM ask

>actM :: (Monad m) => e -> mon Bool Bool -> InGraphM mon e m e
>actM e m = currentGraphM >>= \g -> return $ action g e m

>actSetM :: (Monad m, Ord e) => Set e -> mon Bool Bool -> InGraphM mon e m (Set e)
>actSetM e m = do { g <- currentGraphM ; return $ Set.map (\ee -> action g ee m) e }

>elementsM :: (Monad m, Ord e) => InGraphM mon e m (Set e)
>elementsM = InGraphM $ do { g <- ask ; return (elements g) }

>borderM :: (GraphMonoid mon Bool, Monad m) => e -> InGraphM mon e m (e,e)
>borderM e = actM e gdom >>= \a -> actM e gcod >>= \b -> return $ (a,b)

>-- reversibleBorderM returns domain and codomain of the element, but sorted. This ensures
>-- comparing the border with another such border produces deterministic result.
>reversibleBorderM :: (ReversibleGraphMonoid mon Bool, Monad m, Ord e) => e -> InGraphM mon e m (e,e)
>reversibleBorderM e = actM e gdom >>= \a -> actM e gcod >>= \b -> return $ sortPair (a,b)

>instance (GraphMonoid mon Bool, Monad m, Ord e) => GraphMonad (InGraphM mon e m) e where
>  gelements = elementsM
>  gvertices = verticesM
>  gedges = edgesM
>  gedgesStartingFrom = edgesStartingFromM
>  gedgesEndingTo = edgesEndingToM
>  glinks = linksM
>  gsource = sourceM
>  gtarget = targetM
>  gisVertex = isVertexM

>instance (ReversibleGraphMonoid mon Bool, Monad m, Ord e) => ReversibleGraphMonad (InGraphM mon e m) e where
>   ginverse = inverseM
>   greversibleLinks = reversibleLinksM

>verticesM :: (Ord e, GraphMonoid mon Bool, Monad m) => InGraphM mon e m (Set e)
>verticesM = InGraphM $ ask >>= (return . vertices)

>edgesM :: (Ord e, GraphMonoid mon Bool, Monad m) => InGraphM mon e m (Set e)
>edgesM = InGraphM $ ask >>= (return . edges)

>nodesLinkedFromM :: (Monad m, Ord e, GraphMonoid mon Bool)
>                  => e -> InGraphM mon e m (Set e)
>nodesLinkedFromM node = do edges <- edgesStartingFromM node
>                           mapSetM targetM edges

>edgesStartingFromM :: (Monad m, Ord e, GraphMonoid mon Bool)
>                   => e -> InGraphM mon e m (Set e)
>edgesStartingFromM x = InGraphM $ do
>                         edges <- runInGraphM edgesM
>                         g <- ask
>                         return $ Set.filter (\ e -> action g e gdom == x) $ edges

>edgesEndingToM :: (Monad m, Ord e, GraphMonoid mon Bool)
>               => e -> InGraphM mon e m (Set e)
>edgesEndingToM x = InGraphM $ do
>                         edges <- runInGraphM edgesM
>                         g <- ask
>                         return $ Set.filter (\ e -> action g e gcod == x) $ edges

>linksM :: (Monad m, Ord e, GraphMonoid mon Bool) => InGraphM mon e m (Set (e,e,e))
>linksM = InGraphM $ do 
>            g <- ask
>            edges <- runInGraphM edgesM
>            return $ Set.map (\e -> (e,action g e gdom, action g e gcod)) edges

>reversibleLinksM :: (Monad m, Ord e, ReversibleGraphMonoid mon Bool) => InGraphM mon e m (Set ((e,e),(e,e)))
>reversibleLinksM = InGraphM $ do
>                     g <- ask
>                     edges <- runInGraphM edgesM
>                     let edgedata = Set.map (\e -> ((e,action g e gnot), (action g e gdom, action g e gcod))) edges
>                     return $ fst $ includeExcludeEdges edgedata
>    where includeExcludeEdges = Set.fold (\ ((e,re),(x,y)) (include,exclude) 
>                                          -> if not $ e `Set.member` exclude then
>                                               (Set.insert ((e,re),(x,y)) include,
>                                                Set.insert re exclude)
>                                              else (include,Set.insert re exclude)) (Set.empty,Set.empty)

>sourceM :: (GraphMonoid mon Bool, Monad m) => e -> InGraphM mon e m e
>sourceM e = e `actM` gdom

>targetM :: (GraphMonoid mon Bool, Monad m) => e -> InGraphM mon e m e
>targetM e = e `actM` gcod

>inverseM :: (ReversibleGraphMonoid mon Bool, Monad m) => e -> InGraphM mon e m e
>inverseM e = e `actM` gnot

>isSourceVertexM :: (GraphMonoid mon Bool, Monad m, Eq e) => e -> InGraphM mon e m Bool
>isSourceVertexM element = sourceM element >>= \s -> return (s == element)

>-- | equally valid implementation. These are equivalent in normal graphs
>-- because Bool -> Bool structure has 4 elements, which are related by
>-- 'not' operation which switches source and target. From
>-- Lawvere,Rosebrugh: Sets for Mathematics, Exercise 10.29. Note however
>-- that we are not requiring ReversibleGraphMonoid, which actually
>-- contains this 'not' operation.  This is a reason to separate these
>-- three operations (isSourceVertexM, isTargetVertexM, isVertexM) when
>-- digraphs are used.
>--
>-- The left action of the monoid causes the following constraints:
>-- In particular, these are valid when the monoid is on the two-element set:
>-- 
>-- prop> source . target == source
>--
>-- prop> target . source == target
>--
>-- prop> source . source == source
>--
>-- prop> target . target == target
>--
>-- Due to the first two, it's not necessary to distinguish these operations.
>isTargetVertexM :: (GraphMonoid mon Bool, Monad m, Eq e) => e -> InGraphM mon e m Bool
>isTargetVertexM element = targetM element >>= \s -> return (s == element)

>isVertexM :: (GraphMonoid mon Bool, Monad m, Eq e) => e -> InGraphM mon e m Bool
>isVertexM e = liftA2 (&&) (isSourceVertexM e) (isTargetVertexM e)

>isEdgeM :: (GraphMonoid mon Bool, Monad m, Eq e) => e -> InGraphM mon e m Bool
>isEdgeM element = isVertexM element >>= (return . not)

>hasPathM :: (GraphMonoid mon Bool, Monad m, Eq e) => [e] -> InGraphM mon e m Bool
>hasPathM [c] = isEdgeM c
>hasPathM (c:d:cr) = do e <- isEdgeM c
>                       t <- targetM c
>                       s <- sourceM d
>                       b <- hasPathM (d:cr)
>                       return $ e && (t == s) && b
>hasPathM []  = return False

>isEdgeBetweenM :: (Monad m, GraphMonoid mon Bool, Eq e)
>        => e -> e -> e -> InGraphM mon e m Bool
>isEdgeBetweenM edge x y = do s <- sourceM edge
>                             t <- targetM edge
>                             return (x == s && y == t)

>isLoopM :: (Eq e, Monad m, GraphMonoid mon Bool) => e -> InGraphM mon e m Bool
>isLoopM edge = do { s <- sourceM edge ; t <- targetM edge ; return (s == t) }

>isOneLaneLoopM :: (Eq e, Monad m, ReversibleGraphMonoid mon Bool)
>               => e -> InGraphM mon e m Bool
>isOneLaneLoopM edge = inverseM edge >>= \ie -> return (ie == edge)

>isBandM :: (Eq e, Monad m, ReversibleGraphMonoid mon Bool)
>        => e -> InGraphM mon e m Bool
>isBandM edge = inverseM edge >>= \ie -> return (ie /= edge)


liftStateGraph :: (Monad m) => InGraphM mon e m a -> StateT (Graph mon e) m a
liftStateGraph m = ask >>= \g -> lift (inGraphM g m)

>liftReaderGraph :: (Monad m, MonadTrans t, MonadReader (Graph mon e) (t m))
>                => InGraphM mon e m a -> t m a
>liftReaderGraph m = ask >>= \g -> lift (inGraphM g m)


