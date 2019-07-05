>{-# LANGUAGE Trustworthy, TypeOperators #-}
>module Math.Matrix.Concurrent (parMatrix) where
>import Control.Parallel
>import Math.Matrix.Interface
>
>-- | strict in 'a' and 'b' arguments, which are evaluated in parallel.
>parMatrix :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> (f :*: g) c
>parMatrix f row col = Matrix $ flip fmap row $ \a ->
>                       a `par` flip fmap col $ \b ->
>                       b `pseq` f a b
