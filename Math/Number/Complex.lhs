>{-# LANGUAGE TypeOperators, FlexibleInstances #-}
>module Math.Number.Complex where
>import Data.Complex
>import qualified Math.Number.Stream as S
>import Math.Matrix.Interface
>import qualified Math.Number.R as R
>
>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>complex_sin :: (Floating a) => Complex a -> Complex a
>complex_sin (a :+ b) = sin a * cosh b :+ cos a * sinh b

>complex_pi_ :: (Floating a) => Complex a
>complex_pi_ = pi :+ 0

>-- | compared to Prelude instance of Floating for Complex,
>-- this implementation doesn't attempt to compare scalars for equality
>-- so complex_sqrt (0 :+ 0) == (0 :+ NaN).
>complex_sqrt :: (RealFloat a) => Complex a -> Complex a
>complex_sqrt z@(x:+y) = u :+ (if y < 0*y then -v else v)
>  where (u,v) = if x < 0*x then (v',u') else (u',v')
>        v' = abs y / (u'*2)
>        u' = sqrt ((magnitude z + abs x)/2)

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>complex_cos :: (Floating a) => Complex a -> Complex a
>complex_cos (a :+ b) = cos a * cosh b :+ negate (sin a * sinh b)

>-- | Multiplication of complex numbers. Doesn't require RealFloat.
>complex_mul :: (Num a) => Complex a -> Complex a -> Complex a
>complex_mul (a :+ b) (a' :+ b') = (a*a' - b*b') :+ (a*b'+b*a')

>-- | using euler's formula <https://en.wikipedia.org/wiki/Euler%27s_formula>
>complex_exp :: (Floating a) => Complex a -> Complex a
>complex_exp (a :+ b) = (exp a :+ 0) `complex_mul` (cos b :+ sin b)

>complex_integral :: (Foldable t, Functor t) => (Rational -> t (Complex R.R))
> -> (Complex R.R -> Complex R.R) -> Complex R.R
>complex_integral b f = res f :+ resi f
>  where res f = R.real $ \eps -> eps * sum (fmap ((`R.approximate` eps) . realPart . f) (b eps))
>        resi f = R.real $ \eps -> eps * sum (fmap ((`R.approximate` eps) . imagPart . f) (b eps))

>fourier :: (Foldable t, Functor t) => (Rational -> t (Complex R.R)) -> (Complex R.R -> Complex R.R) -> Complex R.R -> Complex R.R
>fourier b f t = complex_integral b (\x -> f x `complex_mul` complex_exp ((0 :+ 1) `complex_mul` complex_pi_ `complex_mul` x `complex_mul` t))

>type ComplexStream a = (S.Stream :*: Complex) a

>instance (RealFloat a) => Num ((S.Stream :*: Complex) a) where
>   (Matrix x) + (Matrix y) = Matrix $ x + y
>   (Matrix x) - (Matrix y) = Matrix $ x - y
>   (Matrix x) * (Matrix y) = Matrix $ x * y
>   negate (Matrix x) = Matrix (negate x)
>   abs (Matrix x) = Matrix (abs x)
>   signum (Matrix x) = Matrix (signum x)
>   fromInteger i = Matrix $ fromInteger i
>instance (RealFloat a) => Fractional ((S.Stream :*: Complex) a) where
>   (Matrix x) / (Matrix y) = Matrix $ x / y
>   recip (Matrix x) = Matrix $ recip x
>   fromRational x = Matrix $ fromRational x

>instance (RealFloat a) => Floating ((S.Stream :*: Complex) a) where
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
>   show (Matrix (S.Pre x xr)) = show x ++ "," ++ show xr
