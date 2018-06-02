>{-# LANGUAGE FlexibleContexts, InstanceSigs #-}
>module Math.Tools.Monad where
>import Math.Tools.Adjunction hiding (AdjM, runAdjM)
>import Math.Tools.CoMonad
>import Control.Applicative
>import Control.Arrow
>import Control.Monad.Trans.Writer

import Control.Monad.Trans.List

>-- | <https://ncatlab.org/nlab/show/idemponent+monad>
>class (Monad m) => ModalMonad m where
>   splitM :: m a -> m (m a) -- inverse to join

>class (Monad m) => NegatableMonad m where
>   negateM :: m a -> m a

>class (Monad m) => KnowledgeMonad m where
>   knownM :: m a -> a

>data AdjM f g a = AdjM { runAdjM :: g (f a) }

>instance (Monoid a) => ModalMonad ((->) a) where
>   splitM :: (a -> b) -> (a -> a -> b)
>   splitM f = \a b -> f (a `mappend` b)

>instance (Monoid a) => KnowledgeMonad ((->) a) where
>   knownM :: (a -> b) -> b
>   knownM f = f mempty

>instance (Functor f, Functor g) => Functor (AdjM f g) where
>   fmap f (AdjM m) = AdjM $ fmap (fmap f) m

>instance (Applicative f, Applicative g) => Applicative (AdjM f g) where
>   pure x = AdjM $ pure (pure x)
>   (AdjM f) <*> (AdjM x) = AdjM $ liftA2 (<*>) f x

>instance (Adjunction f g, Applicative f, Applicative g)
> => Monad (AdjM f g) where
>   return x = AdjM $ unit x
>   (AdjM m) >>= f = AdjM $ fmap (counit . fmap (runAdjM . f)) m

>instance (Adjunction g f) => Comonad (AdjM f g) where
>   extract (AdjM m) = counit m
>   duplicate (AdjM m) = AdjM $ fmap (fmap AdjM . unit) m

>appM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
>appM2 f x y = x >>= \a -> y >>= \b -> f a b
  
>appM3 :: (Monad m) => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
>appM3 f x y z = x >>= \a -> y >>= \b -> z >>= \c -> f a b c

>-- | type from tweet by Chris Martin, but well known.
>(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
>f <=< g = runKleisli (Kleisli f <<< Kleisli g)

>fromMaybeM :: (Monad m) => Maybe a -> m a
>fromMaybeM = maybe (fail "fromMaybeM: Nothing") return

>bind2 :: (Monad m) => m (m a) -> (a -> m b) -> m b
>bind2 f g = f >>= \v -> v >>= g

>join3 :: (Monad m) => m (m (m b)) -> m b
>join3 = (`bind2` id)

>-- | This is strange, you can remove arbitrary monad with this.
>reduceM :: (Monad m) => m (a -> m b) -> a -> m b
>reduceM f g = f >>= \v -> v g

>mapK :: (Monad m) => m (a -> m b) -> m a -> m b
>mapK f g = f >>= \v -> g >>= \x -> v x
