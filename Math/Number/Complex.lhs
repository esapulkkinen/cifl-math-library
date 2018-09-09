>module Math.Number.Complex where
>import Data.Complex
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

