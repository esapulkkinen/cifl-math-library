>{-# LANGUAGE Safe,FlexibleInstances,MultiParamTypeClasses, TypeFamilies #-}
>module Math.Matrix.FreeVectorSpace where
>import qualified Data.Set as Set
>import Math.Matrix.Interface

>-- | <https://en.wikipedia.org/wiki/Free_module>
>--   <http://drexel28.wordpress.com/2011/04/19/free-vector-spaces/>

>data FreeFS x f = FSum (Set.Set x -> f)

>instance (Num f) => Num (FreeFS x f) where
>   (FSum a) + (FSum b) = FSum $ \s -> a s + b s
>   (FSum a) - (FSum b) = FSum $ \s -> a s - b s
>   (*) = undefined
>   negate (FSum a) = FSum $ \s -> negate (a s)
>   abs (FSum a) = FSum $ \s -> abs (a s)
>   fromInteger i = FSum $ \s -> fromInteger i
>   signum (FSum a) = FSum $ \s -> signum (a s)

>instance (Num f) => VectorSpace (FreeFS x f) where
>   type Scalar (FreeFS x f) = f
>   vzero = FSum $ const 0
>   vnegate (FSum f) = FSum $ negate . f
>   x %* (FSum a) = FSum $ \s -> x * (a s)
>   (FSum a) %+ (FSum b) = FSum $ \s -> a s + b s
>   
