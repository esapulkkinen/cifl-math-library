>{-# LANGUAGE Safe,RankNTypes, GADTs, FlexibleInstances, MultiParamTypeClasses #-}
>module Math.Graph.GraphMonad where
>import Data.Monoid
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad
>import Math.Graph.GraphMonoid
>import safe Control.Monad.Writer.Class

>-- | This monad is based on idea from "Mac Lane: Categories for the working
>-- mathematician" e.g. monad defined by the functor T(A) = G x A, where G
>-- is a group or monoid.

>newtype GraphM g a = GraphM { runGraphM :: (g,a) }

>instance Functor (GraphM g) where
>   fmap f ~(GraphM ~(a,b)) = GraphM (a,f b)

>instance (Monoid g) => Applicative (GraphM g) where
>   pure x = GraphM $ pure x
>   ~(GraphM f) <*> ~(GraphM x) = GraphM $ f <*> x

>instance (Monoid g) => Monad (GraphM g) where
>  return = pure
>  ~(GraphM ~(a,x)) >>= g = let
>       ~(GraphM ~(b,y)) = g x
>     in GraphM (mappend a b,y)

>instance (Monoid g) => MonadFail (GraphM g) where
>  fail msg = GraphM (mempty,error msg)

>-- <https://downloads.haskell.org/ghc/latest/docs/libraries/mtl-2.2.2/Control-Monad-Writer-Lazy.html#t:MonadWriter>
>instance (Monoid g) => MonadWriter g (GraphM g) where
>   writer (a,b) = GraphM (b,a)
>   tell b = GraphM (b,())
>   listen ~(GraphM ~(a,b)) = GraphM (a,(b,a))
>   pass ~(GraphM ~(a, ~(b,f))) = GraphM (f a, b)

>runGraphActionM :: (a -> g -> a) -> GraphM g a -> a
>runGraphActionM f ~(GraphM ~(action,res)) = f res action

>actMG :: g -> GraphM g ()
>actMG x = GraphM $ (x,())

>data GraphMT m g a = GraphMT { runGraphMT :: (forall b. b -> g -> m b) -> m a }

>instance (Functor m) => Functor (GraphMT m g) where
>   fmap f ~(GraphMT x) = GraphMT $ \act -> fmap f (x act)

>instance (Monoid g, Applicative m) => Applicative (GraphMT m g) where
>   pure x = GraphMT $ \act -> act x mempty
>   ~(GraphMT f) <*> ~(GraphMT x) = GraphMT $ \act -> f act <*> x act

>instance (Monoid g, Monad m) => Monad (GraphMT m g) where
>   ~(GraphMT f) >>= g = GraphMT $ \act -> f act >>= \a -> runGraphMT (g a) act

>instance (Monoid g, MonadFail m) => MonadFail (GraphMT m g) where
>   fail msg = GraphMT $ \_ -> fail msg

>actMT :: (Monad m) => g -> GraphMT m g a -> GraphMT m g a
>actMT x ~(GraphMT f) = GraphMT $ \act -> f act >>= \y -> act y x
