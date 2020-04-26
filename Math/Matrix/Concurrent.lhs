>{-# LANGUAGE Unsafe, TypeOperators #-}
>module Math.Matrix.Concurrent (parMatrix) where
>import Control.Parallel
>import Control.Parallel.Strategies
>import Math.Matrix.Interface
>
>-- | strict in 'a' and 'b' arguments, which are evaluated in parallel.
>parMatrix :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> (f :*: g) c
>parMatrix f row col = Matrix $ flip fmap row $ \a ->
>                       a `par` flip fmap col $ \b ->
>                       b `pseq` f a b

>-- | parallel matrix evaluation for traversable instances.
>-- computes each row of the matrix in parallel. Uses parallel strategies.
>parMatrixTraversable :: (Traversable f, Traversable g) => (a -> b -> c) -> f a -> g b -> (f :*: g) c
>parMatrixTraversable f row col = Matrix $
>   withStrategy (parTraversable $ evalTraversable rseq) $
>   flip fmap row $ \a -> flip fmap col $ \b -> f a b
