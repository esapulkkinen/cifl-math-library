>{-# LANGUAGE TypeOperators #-}
>module Math.Matrix.Concurrent where
>import Control.Parallel
>import Math.Matrix.Interface
>
>-- | strict in second and third argument, which are evaluated in parallel.
>parMatrix :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> (f :*: g) c
>parMatrix f row col = Matrix $ flip fmap row $ \a ->
>                       a `par` flip fmap col $ \b ->
>                       b `pseq` f a b