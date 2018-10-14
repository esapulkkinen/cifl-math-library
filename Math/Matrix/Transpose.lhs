>{-# LANGUAGE Safe,MultiParamTypeClasses, FlexibleInstances #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Matrix.Transpose where
>import Math.Tools.CoFunctor
>import Math.Matrix.Interface

>data Dual m a = Dual (m a -> a)

instance (Functor m) => CoFunctor (Dual m) where
  inverse_image f (Dual g) = Dual (g . fmap f)

instance VectorSpace (Dual m a) where
  type Scalar (Dual m a)  
  a %* (Dual f) = Dual (\x -> a * f x)

>instance (Num a) => Num (Dual m a) where
>  (Dual f) + (Dual g) = Dual $ \x -> f x + g x
>  (Dual f) - (Dual g) = Dual $ \x -> f x - g x
>  (Dual f) * (Dual g) = Dual $ \x -> f x * g x
>  negate (Dual f) = Dual $ \x -> negate (f x)
>  abs (Dual f) = Dual $ \x -> abs (f x)
>  signum (Dual f) = Dual $ \x -> signum (f x)
>  fromInteger i = Dual $ const (fromInteger i)
