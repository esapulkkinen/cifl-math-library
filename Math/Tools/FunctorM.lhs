>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.FunctorM where
>import Math.Tools.Universe
>import Control.Arrow

Monadic version of functor. This is a generalization of mapM to arbitrary data structures.

>class FunctorM f where
>   mapMF     :: (Monad m) => (a -> m b) -> f a -> m (f b)

>sequenceM :: (Monad m, FunctorM f) => f (m b) -> m (f b)
>sequenceM = mapMF id

>instance FunctorM [] where
>   mapMF = mapM

>instance FunctorM (Either a) where
>   mapMF _ (Left x) = return (Left x)
>   mapMF f (Right y) = f y >>= (return . Right)

>instance FunctorM Maybe where
>   mapMF _ Nothing = return Nothing
>   mapMF f (Just x) = f x >>= (return . Just)

