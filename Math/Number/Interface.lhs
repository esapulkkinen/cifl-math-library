>{-# LANGUAGE Safe, TypeOperators, UnicodeSyntax, FlexibleInstances #-}
>{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
>module Math.Number.Interface where
>import safe Control.Applicative
>import safe Data.Monoid
>import safe Data.Ratio
>import safe Math.Number.StreamInterface
>import safe Math.Matrix.Interface
>import safe Math.Tools.CoFunctor

>nthroot :: (Floating a) => a -> a -> a
>nthroot x k = exp (log x / k)

>logroot :: (Floating a) => a -> a -> a
>logroot x k = exp (log x / log k)

>-- | Dedekind cut. Notice typically in constructive reals,
>-- it's possible to implement comparison between rational and real
>-- by utilizing denominator of the rational as describing desired accuracy
>-- but comparison between two reals is undecidable.
>-- Note, naming is chosen so the percentage mark is at side that
>-- can always be compared.
>class (Ord q) => DedekindCut q r where
>  {-# MINIMAL (%<), (<%) #-}
>  (%<) :: q -> r -> Bool
>  (<%) :: r -> q -> Bool
>  (%>) :: q -> r -> Bool
>  (>%) :: r -> q -> Bool
>  is_apart :: r -> q -> r -> Bool
>  x %> y = y <% x
>  x >% y = y %< x
>  is_apart x q y = (x <% q && q %< y) || (y <% q && q %< x)

>class Approximations str a where
>   floating_approximations :: a -> str Double
>   rational_approximations :: a -> str Rational

>class Infinitary a where
>   infinite :: a
> 
>class (Infinitary a) => PotentiallyInfinite a where
>   is_infinite :: a -> Bool

>class Numerics a where
>   newtons_method :: (a -> a) -> a -> a

>class RationalRoots a where
>   rational_power :: a -> Rational -> a

>instance RationalRoots Float where
>   rational_power x r = x ** (fromRational r)

>instance RationalRoots Double where
>   rational_power x r = x ** (fromRational r)

>class (Num r) => DifferentiallyClosed r where
>   derivate :: (r -> r) -> r -> r
>   integral :: (r,r) -> (r -> r) -> r

>integral_vector :: (VectorSpace b, Applicative f, Enum a, Num a)
>  => (f a, f a) -> (a -> b) -> f a -> f b
>integral_vector (x,y) f eps = fmap vsum $ liftA3 (\ x' y' eps -> map f [x',x' + eps .. y']) x y eps

>derivate_vector :: (Applicative t, DifferentiallyClosed r) => t (r -> r) -> t r -> t r
>derivate_vector = liftA2 derivate

>class DifferentialOperator t where
>   partial :: (DifferentiallyClosed a) => (t a -> a) -> t a -> t a

>partial1_2 :: (DifferentiallyClosed a) => (a -> b -> a) -> a -> b -> a
>partial1_2 f x y = derivate (\x0 -> f x0 y) x

>partial2_2 :: (DifferentiallyClosed a) => (b -> a -> a) -> b -> a -> a
>partial2_2 f x y = derivate (\y0 -> f x y0) y

>partial1_3 :: (DifferentiallyClosed a) => (a -> b -> c -> a) -> a -> b -> c -> a
>partial1_3 f x y z = derivate (\x0 -> f x0 y z) x

>partial2_3 :: (DifferentiallyClosed a) => (b -> a -> c -> a) -> b -> a -> c -> a
>partial2_3 f x y z = derivate (\y0 -> f x y0 z) y

>partial3_3 :: (DifferentiallyClosed a) => (b -> c -> a -> a) -> b -> c -> a -> a
>partial3_3 f x y z = derivate (\z0 -> f x y z0) z

>partial1_4 :: (DifferentiallyClosed a) => (a -> b -> c -> d -> a) -> a -> b -> c -> d -> a
>partial1_4 f x y z t = derivate (\x0 -> f x0 y z t) x

>partial2_4 :: (DifferentiallyClosed a) => (b -> a -> c -> d -> a) -> b -> a -> c -> d -> a
>partial2_4 f x y z t = derivate (\y0 -> f x y0 z t) y

>partial3_4 :: (DifferentiallyClosed a) => (b -> c -> a -> d -> a) -> b -> c -> a -> d -> a
>partial3_4 f x y z t = derivate (\z0 -> f x y z0 t) z

>partial4_4 :: (DifferentiallyClosed a) => (b -> c -> d -> a -> a) -> b -> c -> d -> a -> a
>partial4_4 f x y z t = derivate (\t0 -> f x y z t0) t

>class (Show r) => ShowPrecision r where
>   show_at_precision :: r -> Integer -> String


>-- | <https://en.wikipedia.org/wiki/Differential_form>
>differential :: (DifferentiallyClosed r) => (r -> r) -> r -> Endo r 
>differential f x0 = Endo $ \dx -> dx * derivate f x0

>-- | computes \(f'(x)*g(x) - f(x)*g'(x)\)
>derivate_commutator :: (DifferentiallyClosed r) => (r -> r) -> (r -> r) -> r -> r
>derivate_commutator f g x = derivate f x * g x - f x * (derivate g x)


>-- | computes \(f'(x)*g(x) + f(x)*g'(x)\). Notice this is product rule.
>derivate_anticommutator :: (DifferentiallyClosed r) => (r -> r) -> (r -> r) -> r -> r
>derivate_anticommutator f g x = derivate f x * g x + f x * (derivate g x)

>derivates :: (StreamBuilder str, DifferentiallyClosed r)
> => (r -> r) -> str (r -> r)
>derivates f = pre f $ fmap derivate $ derivates f



>-- | <http://en.wikipedia.org/wiki/Atan2 Atan2>
>atan2_generic :: (Floating a) => a -> a -> a
>atan2_generic y x = 2 * atan ((sqrt (x*x+y*y) - x) / y)

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>cot :: (Floating a) => a -> a
>cot x = cos x / sin x

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>sec :: (Floating a) => a -> a
>sec x = 1 / cos x

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>csc :: (Floating a) => a -> a
>csc x = 1 / sin x

>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>arsinh :: (Floating a) => a -> a
>arsinh = asinh
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>arcosh :: (Floating a) => a -> a
>arcosh = acosh
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>artanh :: (Floating a) => a -> a
>artanh = atanh
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>arcoth :: (Floating a) => a -> a
>arcoth x = log ((1 + x)/(x - 1)) / 2
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>arsech :: (Floating a) => a -> a
>arsech x = log ((1/x) + sqrt(1/(x*x)-1))
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>arcsch :: (Floating a) => a -> a
>arcsch x = log ((1/x) + sqrt(1/(x*x)+1))

>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>coth :: (Floating a) => a -> a
>coth x = cosh x / sinh x
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>sech :: (Floating a) => a -> a
>sech x = 1 / cosh x
>
>-- | <https://en.wikipedia.org/wiki/Hyperbolic_function>
>csch :: (Floating a) => a -> a
>csch x = 1 / sinh x

>-- | <https://en.wikipedia.org/wiki/Line_integral Line integral>

>line_integral :: (DifferentiallyClosed r) => (r -> r) -> (r -> r) -> (r,r) -> r
>line_integral f r (a,b) = integral (a,b) $ \t ->
>   f (r t) * abs (derivate r t)


>instance ShowPrecision Double where
>  show_at_precision r _ = show r

>instance ShowPrecision Int where
>  show_at_precision r _ = show r

>instance ShowPrecision Integer where
>  show_at_precision r _ = show r

>instance ShowPrecision Float where
>  show_at_precision r _ = show r


>-- | compare to a certain precision, appropriate for floating point
>-- style numeric types. first argument is precision.
>precisionCompare :: (Ord a, Num a) => a -> a -> a -> Bool
>precisionCompare prec a b = abs (a - b) < prec


>instance DedekindCut Rational Rational where
>   x <% y = x < y
>   x %< y = x < y

>instance DedekindCut Rational Float where
>   x <% r = x < fromRational r
>   r %< x = fromRational r < x

>instance DedekindCut Float Float where
>   x <% r = x < r
>   r %< x = r < x
>instance DedekindCut Double Double where
>   x <% r = x < r
>   r %< x = r < x

>instance DedekindCut Rational Double where
>   x <% r = x < fromRational r
>   r %< x = fromRational r < x

>instance DedekindCut Integer Float where
>   x <% r = x < fromInteger r
>   r %< x = fromInteger r < x
>
>instance DedekindCut Integer Double where
>   x <% i = x < fromInteger i
>   i %< x = fromInteger i < x

>instance DedekindCut Integer Rational where
>   x %< y = fromInteger x < y
>   x <% y = x < fromInteger y

>instance DedekindCut Int Rational where
>   x %< y = fromIntegral x < y
>   x <% y = x < fromIntegral y

>instance DedekindCut Int Float where
>   x %< y = fromIntegral x < y
>   x <% y = x < fromIntegral y

>instance DedekindCut Int Double where
>   x %< y = fromIntegral x < y
>   x <% y = x < fromIntegral y

>instance DedekindCut Word Word where
>   x %< y = x < y
>   x <% y = x < y
>instance DedekindCut Int Int where
>   x %< y = x < y
>   x <% y = x < y

>instance DedekindCut Word Rational where
>   x %< y = fromIntegral x < y
>   x <% y = x < fromIntegral y

>instance DedekindCut Word Float where
>   x %< y = fromIntegral x < y
>   x <% y = x < fromIntegral y

>instance DedekindCut Word Double where
>   x %< y = fromIntegral x < y
>   x <% y = x < fromIntegral y

>instance DedekindCut Integer Integer where
>   x %< y = x < y
>   x <% y = x < y

