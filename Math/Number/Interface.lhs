>{-# LANGUAGE Safe, TypeOperators, UnicodeSyntax, FlexibleInstances #-}
>module Math.Number.Interface where
>import safe Control.Applicative
>import safe Data.Monoid
>import safe Data.Ratio
>import safe Math.Number.Stream
>import safe Math.Matrix.Interface

>nthroot :: (Floating a) => a -> a -> a
>nthroot x k = exp (log x / k)

>logroot :: (Floating a) => a -> a -> a
>logroot x k = exp (log x / log k)

>class Approximations a where
>   floating_approximations :: a -> Stream Double
>   rational_approximations :: a -> Stream Rational

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

>derivates :: (DifferentiallyClosed r) => (r -> r) -> Stream (r -> r)
>derivates f = Pre f $ fmap derivate $ derivates f

>-- | <https://en.wikipedia.org/wiki/Taylor_series>
>taylor :: (Fractional a, DifferentiallyClosed a) => (a -> a) -> a -> (Stream :*: Stream) a
>taylor f a = Matrix $ sum_stream $ mapper <$> sub_powers
>   where mapper p = liftA3 (\a b c -> a*b*c) der divider p
>         divider = fmap (1/) factorial
>         der = derivates f <*> constant a
>         sub_powers = cells $ stream_powers (s_z-fromNum a)


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

>instance Approximations Float where
>  floating_approximations = constant . realToFrac
>  rational_approximations = constant . toRational

>instance Approximations Double where
>  floating_approximations = constant
>  rational_approximations = constant . toRational

>-- | compare to a certain precision, appropriate for floating point
>-- style numeric types. first argument is precision.
>precisionCompare :: (Ord a, Num a) => a -> a -> a -> Bool
>precisionCompare prec a b = abs (a - b) < prec

>instance Infinitary (Closure Rational) where
>   infinite = InfiniteRational

>instance Infinitary (Closure Integer) where
>   infinite = InfiniteInteger
