>{-# LANGUAGE Safe,MultiParamTypeClasses, FlexibleInstances #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Matrix.Transpose where
>import Math.Tools.CoFunctor
>import Math.Matrix.Interface

>data TDual m a = TDual (m a -> a)

instance (Functor m) => CoFunctor (Dual m) where
  inverse_image f (Dual g) = Dual (g . fmap f)

instance VectorSpace (Dual m a) where
  type Scalar (Dual m a)  
  a %* (Dual f) = Dual (\x -> a * f x)

>instance (Num a) => Num (TDual m a) where
>  (TDual f) + (TDual g) = TDual $ \x -> f x + g x
>  (TDual f) - (TDual g) = TDual $ \x -> f x - g x
>  (TDual f) * (TDual g) = TDual $ \x -> f x * g x
>  negate (TDual f) = TDual $ \x -> negate (f x)
>  abs (TDual f) = TDual $ \x -> abs (f x)
>  signum (TDual f) = TDual $ \x -> signum (f x)
>  fromInteger i = TDual $ const (fromInteger i)
