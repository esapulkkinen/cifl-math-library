>{-# LANGUAGE TypeFamilies #-}
>module Math.Number.Dual where
>-- ^ implementation of dual numbers, <https://en.wikipedia.org/wiki/Dual_number>
>import Math.Matrix.Interface

>data DualNumber a = DualNumber a a

>instance (Num a) => VectorSpace (DualNumber a) where
>   type Scalar (DualNumber a) = a
>   vzero = DualNumber 0 0
>   vnegate (DualNumber a b) = DualNumber (negate a) (negate b)
>   (DualNumber a b) %+ (DualNumber a' b') = DualNumber (a + a') (b + b')
>   k %* (DualNumber a b) = DualNumber (k * a) (k * b)

>instance (Num a) => Num (DualNumber a) where
>   (DualNumber a b) + (DualNumber a' b') = DualNumber (a + a') (b + b')
>   (DualNumber a b) - (DualNumber a' b') = DualNumber (a - a') (b - b')
>   (DualNumber a b) * (DualNumber a' b') = DualNumber (a * a') (a*b'+b*a')
>   negate (DualNumber a b) = DualNumber (negate a) (negate b)
>   abs (DualNumber a _) = DualNumber a 0
>   signum (DualNumber a b) = DualNumber (signum a) (signum b)
>   fromInteger i = DualNumber (fromInteger i) 0

>slope :: (Fractional a) => DualNumber a -> a
>slope (DualNumber a b) = b / a

>dual_exp :: (Floating a) => DualNumber a -> DualNumber a
>dual_exp (DualNumber a b) = exp a %* DualNumber 1 b
