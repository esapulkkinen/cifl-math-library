>{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Matrix.CStar where
>import Data.Complex
>import Math.Matrix.Interface
> 
>data CStar v = CStar { runCStar :: v -> v }
>
>translationO :: (VectorSpace v) => v -> CStar v -> CStar v
>translationO x (CStar f) = CStar $ \v -> f (x %+ v)

>scalingO :: (VectorSpace v) => Scalar v -> CStar v -> CStar v
>scalingO x (CStar f) = CStar $ \v -> f (x %* v)

>symmetryO :: (Num a, ComplexVectorSpace v a) => CStar v -> CStar v
>symmetryO = scalingO (0 :+ 1)

