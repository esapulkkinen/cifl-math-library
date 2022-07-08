>{-# LANGUAGE TypeFamilies #-}
>module Math.Matrix.Algebra where
>import Math.Matrix.Interface

>-- <https://en.wikipedia.org/wiki/Associative_algebra>
>data Alg r a = Alg { run_algebra :: (r -> a) -> a }

>instance (Num a, Num r) => VectorSpace (Alg r a) where
>  type Scalar (Alg r a) = r
>  vzero = aindex 0
>  vnegate (Alg f) = Alg $ \s -> negate (f s)
>  (Alg f) %+ (Alg g) = Alg $ \s -> f s + g s
>  a %* (Alg f) = Alg $ \s -> s a * f s

>iscenter :: (Num a) => Alg r a -> Alg r Bool
>iscenter (Alg f) = Alg $ \s -> s a * f s == f s * s a

>arun :: Alg r r -> r
>arun (Alg f) = f id
>
>ainverse_image :: ((r -> a) -> r' -> a) -> Alg r' a -> Alg r a
>ainverse_image g (Alg f) = Alg (f . g)

>algebra_map :: ((r -> a) -> r' -> b) -> (b -> a) -> Alg r' b -> Alg r a
>algebra_map g f (Alg h) = Alg (f . h . g)

>aunit :: (Num r) => Alg r a
>aunit = aindex 1

>aindex :: r -> Alg r a
>aindex x = Alg $ \f -> f x
>
>aelem :: a -> Alg r a
>aelem x = Alg $ const x
>

>asum :: (Num r, Num a) => r -> r -> Alg r a
>asum a b = Alg $ \f -> f a + f b

>aprod :: (Num r, Num a) => r -> r -> Alg r a
>aprod a b = Alg $ \f -> f a * f b


>testAlgebraStructureMapPlus :: (Num r, Num a, VectorSpace a)
>  => r -> r -> Alg r a
>testAlgebraStructureMapPlus a b = Alg $ \f -> f (a + b) %- f a %- f b

>testAlgebraStructureMapTimes :: (Num r, Num a, VectorSpace a)
> => r -> r -> Alg r a
>testAlgebraStructureMapTimes a b = Alg $ \f -> f (a * b) %- f a * f b
