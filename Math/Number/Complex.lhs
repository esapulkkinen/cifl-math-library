>{-# LANGUAGE TypeOperators, FlexibleInstances #-}
>module Math.Number.Complex where
>import Data.Complex
>import Math.Number.Stream
>import Math.Matrix.Interface
>
>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>complex_sin :: (Floating a) => Complex a -> Complex a
>complex_sin (a :+ b) = sin a * cosh b :+ cos a * sinh b

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>complex_cos :: (Floating a) => Complex a -> Complex a
>complex_cos (a :+ b) = cos a * cosh b :+ negate (sin a * sinh b)

>-- | Multiplication of complex numbers. Doesn't require RealFloat.
>complex_mul :: (Num a) => Complex a -> Complex a -> Complex a
>complex_mul (a :+ b) (a' :+ b') = (a*a' - b*b') :+ (a*b'+b*a')

>-- | using euler's formula <https://en.wikipedia.org/wiki/Euler%27s_formula>
>complex_exp :: (Floating a) => Complex a -> Complex a
>complex_exp (a :+ b) = (exp a :+ 0) `complex_mul` (cos b :+ sin b)


>type ComplexStream a = (Stream :*: Complex) a

>instance (RealFloat a) => Num ((Stream :*: Complex) a) where
>   (Matrix x) + (Matrix y) = Matrix $ x + y
>   (Matrix x) - (Matrix y) = Matrix $ x - y
>   (Matrix x) * (Matrix y) = Matrix $ x * y
>   negate (Matrix x) = Matrix (negate x)
>   abs (Matrix x) = Matrix (abs x)
>   signum (Matrix x) = Matrix (signum x)
>   fromInteger i = Matrix $ fromInteger i
>instance (RealFloat a) => Fractional ((Stream :*: Complex) a) where
>   (Matrix x) / (Matrix y) = Matrix $ x / y
>   recip (Matrix x) = Matrix $ recip x
>   fromRational x = Matrix $ fromRational x

>instance (RealFloat a) => Floating ((Stream :*: Complex) a) where
>   pi = Matrix $ pi
>   exp = Matrix . exp . cells
>   log = Matrix . log . cells
>   sqrt = Matrix . sqrt . cells
>   (Matrix x) ** (Matrix y) = Matrix (x ** y)
>   sinh = Matrix . sinh . cells
>   cosh = Matrix . cosh . cells
>   tanh = Matrix . tanh . cells
>   asinh = Matrix . asinh . cells
>   acosh = Matrix . acosh . cells
>   atanh = Matrix . atanh . cells

>instance (Show a) => Show (ComplexStream a) where
>   show (Matrix (Pre x xr)) = show x ++ "," ++ show xr
