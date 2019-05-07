>{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
>{-# LANGUAGE TypeOperators #-}
>module Math.Number.R where
>import Control.Applicative
>import Control.Monad
>import Data.Char
>import Data.Ratio
>import Data.Monoid
>import Data.Complex
>import Math.Tools.PrettyP
>import Math.Matrix.Covector
>import Math.Matrix.Interface
>import Math.Number.Interface
>import Math.Number.Stream
>import qualified Control.Monad.Fix
>
>-- | This real representation takes 'epsilon' as input as in epsilon-delta proof.
>data R = Limit { approximate_endo :: Endo Rational }

>instance Limiting R where
>   data Closure R = RClosure { runRClosure :: R }
>   limit (Pre x z@(Pre y _)) = RClosure $ real $ \eps ->
>       if abs (y `approximate` eps - x `approximate` eps) < eps
>         then y `approximate` eps
>         else runRClosure (limit z) `approximate` eps
>   approximations (RClosure r) = fmap f $ fmap (1 /) $ power 10
>      where f p = real $ \eps -> r `approximate` min p eps

>instance Closed R where
>   accumulation_point = runRClosure

>lim :: Stream R -> R
>lim = runRClosure . limit

>real :: (Rational -> Rational) -> R
>real = Limit . Endo

>approximate :: R -> Rational -> Rational
>approximate = appEndo . approximate_endo

>rational_approximations :: R -> Stream Rational
>rational_approximations r = fmap (approximate r) $ fmap (1/) $ power 10

>floating_approximations :: R -> Stream Double
>floating_approximations r = fmap fromRational $ rational_approximations r

>instance Show R where
>   show x = show $ (fromRational (x `approximate` (1 % 1000000000000000)) :: Double)

>instance PpShow R where
>   pp x = pp $ (fromRational (x `approximate` (1 % 1000000000000000)) :: Double)

>instance CompleteSpace R

>class MetricSpace s where
>   distance :: s -> s -> R

>epsilon :: R
>epsilon = real id

>infinite :: R
>infinite = liftR (1/) epsilon

>-- | this propagates accuracy without changing it.
>liftR :: (Rational -> Rational) -> R -> R
>liftR g (Limit (Endo f)) = real $ g . f

>-- | this propagates accuracy without changing it
>liftR2 :: (Rational -> Rational -> Rational) -> R -> R -> R
>liftR2 h (Limit f) (Limit g) = real $ \eps -> h (f `appEndo` eps) (g `appEndo` eps)

>-- | inverseImage transforms accuracy computation of the real.
>inverseImage :: (Rational -> Rational) -> R -> R
>inverseImage g (Limit (Endo f)) = real $ f . g

>-- | The first argument is the function lifted. The second argument
>-- describes change in accuracy by the function.
>liftWithAccuracy :: (Rational -> Rational) -> (Rational -> Rational) -> R -> R
>liftWithAccuracy f acc r = liftR f (inverseImage acc r)


>-- | approx_compose will use the second real as the level
>--   of approximation to compute the first.
>approximate_as :: R -> R -> R
>approximate_as (Limit f) (Limit g) = Limit (f `mappend` g)

>limit_compose :: (Rational -> R) -> R -> R
>limit_compose  g (Limit f) = real $
> \eps -> approximate_endo (g (f `appEndo` eps)) `appEndo` eps


>inverseImageEndo :: Endo Rational -> Endo R
>inverseImageEndo g = Endo $ \ (Limit f) -> real $ appEndo g . appEndo f


>instance Num R where
>   (+) = liftR2 (+)
>   (-) = liftR2 (-)
>   (Limit f) * (Limit g) = real $ \eps -> f `appEndo` (eps / 2)
>                                        * g `appEndo` (eps / 2)
>   negate = liftR negate
>   abs = liftR abs
>   signum = liftR signum
>   fromInteger i = real $ const (fromInteger i)




>instance Fractional R where
>   (Limit f) / (Limit g) = real $ \eps -> f `appEndo` (eps / 2)
>                                        / g `appEndo` (eps / 2)
>   recip (Limit f) = real $ \eps -> recip (f `appEndo` (recip eps))
>   fromRational x  = real $ const x

>instance ConjugateSymmetric R where
>   conj = id

>-- | R as an infinite dimensional vector space over the rationals.
>instance VectorSpace R where
>   type Scalar R = Rational
>   vzero = real $ const 0
>   vnegate = liftR negate
>   (Limit f) %+ (Limit g) = real $ \eps -> f `appEndo` eps + g `appEndo` eps
>   x %* (Limit f) = real $ \df -> x %* f `appEndo` df

>-- | This lifts a rational function to a real function.
>-- This computes accuracy using the formula @dx = df / f'(x)@.
>-- The accuracy behaves badly when @f'(x) = 0@ due to divide-by-zero.
>differentialLiftR :: (Rational -> Rational) -> R -> R
>differentialLiftR f (Limit (Endo g)) = Limit $ Endo $ \df -> f $ g (1 / (derivate_rational df f (g df)))

>-- | derivate for rational functions. The first argument is epsilon.
>derivate_rational :: Rational -> (Rational -> Rational) -> Rational -> Rational
>derivate_rational eps f i = (f (i + eps) - f (i - eps)) / (2*eps)

>derivate_r :: (R -> R) -> R -> R
>derivate_r f (Limit x) = real $ \eps ->
>    ((f (real $ \eps' -> x `appEndo` eps' + eps)
>      - f (real $ \eps'' -> x `appEndo` eps'' - eps))
>     `approximate` eps)/(2*eps)

>instance DifferentiallyClosed R where
>   derivate = derivate_r
>   integral = integral_r

>taylor :: (R -> R) -> R -> (Stream :*: Stream) R
>taylor f a = Matrix $ sum_stream $ fmap mapper sub_powers
>  where mapper p = liftA3 (\a b c -> (a*c)/b) der factorial p
>        der = derivates f <*> constant a
>        sub_powers = cells $ stream_powers (z - fromNum a)

>newtons_method :: (R -> R) -> R -> R
>newtons_method f x = lim $ iterate_stream iteration x 
>  where iteration z' = z' - f z' / derivate f z'

>sqrt_r :: R -> R
>sqrt_r v = newtons_method (\x -> x*x - v) 1

>log_by_newton :: R -> R
>log_by_newton v = newtons_method (\x -> exp x - v) 1

>exp_r :: R -> R
>exp_r x = runRClosure $ limit $ do
>   xappr <- approximations $ RClosure x
>   sum_stream $ liftA2 (/) (index_powers (constant xappr)) factorial

>-- | Using Simon Plouffe's BPP digit extraction algorithm for computing pi.
>-- See <https://secure.wikimedia.org/wikipedia/en/wiki/Pi Pi> for details.
>pi_r :: R
>pi_r = lim $ sum_stream $ do
>   k <- naturals
>   let kr = k % 1
>   return $ fromRational $ (16^^(negate k))*(4/(8*kr+1) - 2/(8*kr+4)
>                              - 1/(8*kr+5) - 1/(8*kr+6))

>-- <http://en.wikipedia.org/wiki/Logarithm Logarithm>
>log_r :: R -> R
>log_r a = let a' = (a-1)/(a+1) in
>  2* (lim $ sum_stream $ do   
>     n <- naturals
>     return $ (1 / (2 * fromIntegral n + 1))*a'^(2*n+1))

>integral_r :: (R,R) -> (R -> R) -> R
>integral_r (x,y) f = foldable_integral acc f
>   where acc = map fromRational . integral_accuracy f x y

>foldable_integral :: (Foldable t, Functor t) => (Rational -> t b) -> (b -> R) -> R
>foldable_integral border f = real $ \eps ->
>   foldable_simple_integral border ((`approximate` eps) . f) eps

>foldable_simple_integral :: (Num a, Foldable t, Functor t) =>
>  (a -> t b) -> (b -> a) -> a -> a
>foldable_simple_integral border f eps = eps * (sum $ fmap f $ border eps)

>integral_accuracy :: (Fractional t) => (t -> R) -> R -> R -> Rational -> [Rational]
>integral_accuracy f x y eps = [x',x'+eps..y']
>  where accuracy z = derivate_rational eps (\i -> f (fromRational i) `approximate` eps) z
>        x' = x `approximate` (eps / accuracy (x `approximate` eps))
>        y' = y `approximate` (eps / accuracy (y `approximate` eps))

>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>asin_r :: R -> R
>asin_r x = integral (0,x) (\z -> 1 / sqrt(1 - z*z))
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>acos_r :: R -> R
>acos_r x = (pi_r / 2) - asin_r x
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>atan_r :: R -> R
>atan_r x = integral (0,x) (\z -> (1 / (z*z+1)))


>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>sin_by_series :: R -> R
>sin_by_series x = lim $ fst $ uninterleave $ sum_stream $ liftA2 (*) filterStream
>                                   $ liftA2 (/) (index_powers $ constant x)
>                                   $ factorial
>        where filterStream = Pre 0 $ Pre 1 $ Pre 0 $ Pre (negate 1) $ filterStream
>              
>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>cos_by_series :: R -> R
>cos_by_series x = lim $ fst $ uninterleave $ sum_stream $ liftA2 (*) filterStream
>                                   $ liftA2 (/) (index_powers $ constant x) factorial
>        where filterStream = Pre 1 $ Pre 0 $ Pre (negate 1) $ Pre 0 $ filterStream

>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions inverse trigonometric functions>
>-- <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance Floating R where
>   pi = pi_r
>   exp = exp_r
>   log = log_r
>   sqrt = sqrt_r
>   x ** y = exp (y * log x)
>   sin = sin_by_series
>   cos = cos_by_series
>   tan x = sin x / cos x
>   asin = asin_r
>   acos = acos_r
>   atan = atan_r
>   sinh x = (exp x - exp (negate x)) / 2
>   cosh x = (exp x + exp (negate x)) / 2
>   tanh x = sinh x / cosh x
>   asinh x = log $ x + sqrt (x*x+1)
>   acosh x = log $ x + sqrt (x*x-1)
>   atanh x = log ((1+x)/(1-x)) / 2
