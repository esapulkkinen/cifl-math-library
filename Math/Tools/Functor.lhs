>{-# LANGUAGE MultiParamTypeClasses, LambdaCase, Safe #-}
>module Math.Tools.Functor where
>import Control.Applicative

>class BiFunctor op where
>   bimap :: (a -> b) -> (c -> d) -> op a c -> op b d

>infixr 8 `interleave`

>class (Functor f) => InterleaveFunctor f where
>   interleave :: f a -> f a -> f a

For more than three argument versions of zipWith, use (>>=).
liftA2 is general version of zipWith.

>funzip :: (Functor f) => f (a,b) -> (f a, f b)
>funzip m = (fmap fst m, fmap snd m)

>fst3 :: (a,b,c) -> a
>fst3 (a,_,_) = a
>snd3 :: (a,b,c) -> b
>snd3 (_,b,_) = b
>thrd3 :: (a,b,c) -> c
>thrd3 (_,_,c) = c

>funzip3 :: (Functor f) => f (a,b,c) -> (f a, f b, f c)
>funzip3 m = (fmap fst3 m,fmap snd3 m,fmap thrd3 m)

>unzipWith :: (Functor f) => (a -> (b,c)) -> f a -> (f b, f c)
>unzipWith f m = (fmap (fst . f) m, fmap (snd . f) m)

>fzipWith3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
>fzipWith3 = liftA3

>fzip :: (Applicative f) => f a -> f b -> f (a,b)
>fzip = liftA2 (,)

>infixl 8 <&>

>(<&>) :: (Applicative f) => f a -> f b -> f (a,b)
>(<&>) = fzip

>fzip3 :: (Applicative f) => f a -> f b -> f c -> f (a,b,c)
>fzip3 = liftA3 (,,)

>fzip4 :: (Applicative f) => f a -> f b -> f c -> f d -> f (a,b,c,d)
>fzip4 = liftA4 (,,,)

>liftA4 :: (Applicative f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
>liftA4 f x y z t = pure f <*> x <*> y <*> z <*> t

apply :: (Applicative f) => f (a -> b) -> f a -> f b
apply = (<*>)
