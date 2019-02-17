>{-# LANGUAGE Safe,RankNTypes #-}
>module Math.Graph.GraphMonad where
>import Data.Monoid
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid

>-- | This monad is based on idea from "Mac Lane: Categories for the working
>-- mathematician" e.g. monad defined by the functor T(A) = G x A, where G
>-- is a group or monoid.

>data GraphM g a = GraphM { runGraphM :: (g,a) }

>instance Functor (GraphM g) where
>   fmap f (GraphM (a,b)) = GraphM (a,f b)

>instance (Monoid g) => Applicative (GraphM g) where
>   pure x = GraphM $ pure x
>   (GraphM f) <*> (GraphM x) = GraphM $ f <*> x

>instance (Monoid g) => Monad (GraphM g) where
>  return x = GraphM (mempty,x)
>  (GraphM (a,x)) >>= g = GraphM $ let (GraphM (b,y)) = g x in (mappend a b,y)
>  fail msg = GraphM (mempty,error msg)

>runGraphActionM :: (a -> g -> a) -> GraphM g a -> a
>runGraphActionM f (GraphM (action,res)) = f res action

>actionM :: (Monad m, Ord a) => Graph mon a -> mon -> a -> m a
>actionM g m x = inGraphM g (x `actM` m)
  
>source :: (Monad m, GraphMonoid mon, Ord a) => Graph mon a -> a -> m a
>source g = actionM g gdom

>target :: (Monad m, GraphMonoid mon, Ord a) => Graph mon a -> a -> m a
>target g = actionM g gcod

>inverseGM :: (Monad m, ReversibleGraphMonoid mon, Ord a) => Graph mon a -> a -> m a
>inverseGM g = actionM g gnot


>actMG :: g -> GraphM g ()
>actMG x = GraphM $ (x,())

>data GraphMT m g a = GraphMT { runGraphMT :: (forall b. b -> g -> m b) -> m a }

>instance (Functor m) => Functor (GraphMT m g) where
>   fmap f (GraphMT x) = GraphMT $ \act -> fmap f (x act)

>instance (Monoid g, Applicative m) => Applicative (GraphMT m g) where
>   pure x = GraphMT $ \act -> act x mempty
>   (GraphMT f) <*> (GraphMT x) = GraphMT $ \act -> f act <*> x act

>instance (Monoid g, Monad m) => Monad (GraphMT m g) where
>   return x = GraphMT $ \act -> act x mempty
>   (GraphMT f) >>= g = GraphMT $ \act -> f act >>= \a -> runGraphMT (g a) act
>   fail msg = GraphMT $ \_ -> fail msg

>actMT :: (Monad m) => g -> GraphMT m g a -> GraphMT m g a
>actMT x (GraphMT f) = GraphMT $ \act -> f act >>= \y -> act y x
