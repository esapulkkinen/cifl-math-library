>{-# LANGUAGE Safe, TypeOperators #-}
>module Math.Number.Interface where
>import Control.Applicative
>import Data.Ratio
>import Math.Number.Stream
>import Math.Matrix.Interface

>nthroot :: (Floating a) => a -> a -> a
>nthroot x k = exp (log x / k)

>logroot :: (Floating a) => a -> a -> a
>logroot x k = exp (log x / log k)

>class RationalRoots a where
>   rational_power :: a -> Rational -> a

>instance RationalRoots Float where
>   rational_power x r = x ** (fromRational r)

>instance RationalRoots Double where
>   rational_power x r = x ** (fromRational r)

>class (Num r) => DifferentiallyClosed r where
>   derivate :: (r -> r) -> r -> r
>   integral :: (r,r) -> (r -> r) -> r

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
>         sub_powers = cells $ stream_powers (z-fromNum a)

>-- | <https://en.wikipedia.org/wiki/Line_integral Line integral>

>line_integral :: (DifferentiallyClosed r) => (r -> r) -> (r -> r) -> (r,r) -> r
>line_integral f r (a,b) = integral (a,b) $ \t ->
>   f (r t) * abs (derivate r t)
