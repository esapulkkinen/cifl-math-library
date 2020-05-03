>{-# LANGUAGE Safe, TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
>{-# LANGUAGE TypeOperators, DataKinds, UndecidableInstances #-}
>{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
>module Math.Number.R (
> R(Limit), lim, real, approximate, rational_approximations,
> floating_approximations, epsilon, infinite_r, liftR,
> liftR2, inverseImage, liftWithAccuracy, approximate_as,
> limit_compose, inverseImageEndo, differentialLiftR, derivate_rational,
> derivate_r, newtons_method, sqrt_r, log_by_newton,
> exp_r, pi_r, log_r, integral_r, foldable_integral, foldable_simple_integral,
> integral_accuracy, asin_r, acos_r, atan_r, sin_by_series,
> cos_by_series
> ) where
>import safe Control.Applicative
>import safe Control.Monad
>import safe Data.Char
>import safe Data.Ratio
>import safe Data.Monoid
>import safe Data.Complex
>import safe Math.Tools.PrettyP
>import safe Math.Matrix.Covector
>import safe Math.Matrix.Interface
>import safe Math.Tools.Median
>import safe Math.Number.Interface
>import safe Math.Number.Stream
>import safe qualified Control.Monad.Fix
>import safe GHC.TypeLits
>
>
>-- | This real representation takes 'epsilon' as input as in epsilon-delta proof.
>data R = Limit { approximate_endo :: (Endo Rational) }

>instance (TypeError (Text "Cannot compare reals for equality." :$$:
>                     Text "Real equality is undecidable."))
>  => Eq R where
>   x == y = error "cannot compare reals for equality"


>instance Limiting R where
>   data Closure R = RClosure { runRClosure :: !R }
>   limit ~(Pre x ~z@(Pre y _)) = RClosure $ real $ \eps ->
>       if abs (y `approximate` eps - x `approximate` eps) < eps
>         then y `approximate` eps
>         else runRClosure (limit z) `approximate` eps
>   approximations (RClosure r) = fmap f $ fmap (1 /) $ power 10
>      where f p = real $ \eps -> r `approximate` min p eps

>instance Fractional (Closure R) where
>  (RClosure x) / (RClosure y) = RClosure (x/y)
>  recip (RClosure x) = RClosure (recip x)
>  fromRational r = RClosure (fromRational r)
>
>liftRClosure :: (R -> R) -> Closure R -> Closure R
>liftRClosure f (RClosure x) = RClosure (f x)
> 
>liftRClosure2 :: (R -> R -> R) -> Closure R -> Closure R -> Closure R
>liftRClosure2 f (RClosure x) (RClosure y) = RClosure (f x y)
>
>instance Floating (Closure R) where
>  pi = RClosure pi
>  exp = liftRClosure exp
>  log = liftRClosure log
>  sqrt = liftRClosure sqrt
>  (**) = liftRClosure2 (**)
>  logBase = liftRClosure2 logBase
>  sin = liftRClosure sin
>  cos = liftRClosure cos
>  tan = liftRClosure tan
>  asin = liftRClosure asin
>  acos = liftRClosure acos
>  atan = liftRClosure atan
>  sinh = liftRClosure sinh
>  cosh = liftRClosure cosh
>  tanh = liftRClosure tanh
>  asinh = liftRClosure asinh
>  acosh = liftRClosure acosh
>  atanh = liftRClosure atanh

>instance Num (Closure R) where
>  (RClosure x) + (RClosure y) = RClosure (x + y)
>  (RClosure x) - (RClosure y) = RClosure (x - y)
>  (RClosure x) * (RClosure y) = RClosure (x * y)
>  abs (RClosure x) = RClosure (abs x)
>  signum (RClosure x) = RClosure (signum x)
>  negate (RClosure x) = RClosure (negate x)
>  fromInteger i = RClosure (fromInteger i)

>instance Infinitary (Closure R) where
>   infinite = RClosure infinite


>instance Closed R where
>   accumulation_point = runRClosure

>lim :: Stream R -> R
>lim = runRClosure . limit

>real :: (Rational -> Rational) -> R
>real = Limit . Endo

>approximate :: R -> Rational -> Rational
>approximate = appEndo . approximate_endo

>rational_approximations_r :: R -> Stream Rational
>rational_approximations_r r = fmap (approximate r) $ fmap (1/) $ power 10

>instance Approximations R where
>   floating_approximations = floating_approximations_r
>   rational_approximations = rational_approximations_r

>floating_approximations_r :: R -> Stream Double
>floating_approximations_r r = fmap fromRational $ rational_approximations r

>instance Show R where
>   show x = show $! (fromRational (x `approximate` (1 % 1000000000000000)) :: Double)

>instance PpShow R where
>   pp x = pp $! (fromRational (x `approximate` (1 % 1000000000000000)) :: Double)

>instance CompleteSpace R

>epsilon :: R
>epsilon = real id

>infinite_r :: R
>infinite_r = liftR (1/) epsilon

>instance Infinitary R where
>   infinite = infinite_r

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
>-- This computes accuracy using the formula \(dx = df / f'(x)\).
>-- The accuracy behaves badly when \(f'(x) = 0\) due to divide-by-zero.
>differentialLiftR :: (Rational -> Rational) -> R -> R
>differentialLiftR f (Limit (Endo g)) = Limit $ Endo $ \df -> f $ g (1 / (derivate_rational df f (g df)))

>-- | derivate for rational functions. The first argument is epsilon.
>derivate_rational :: Rational -> (Rational -> Rational) -> Rational -> Rational
>derivate_rational eps f i = (f (i + eps) - f (i - eps)) / (2*eps)

>-- | computes derivate. \[{{df}\over{dt}} = \lim_{\epsilon\rightarrow 0}{{f(t+\epsilon)-f(t-\epsilon)}\over{2\epsilon}}\]
>derivate_r :: (R -> R) -> R -> R
>derivate_r f (Limit x) = real $ \eps ->
>    ((f (real $ \eps' -> x `appEndo` eps' + eps)
>      - f (real $ \eps'' -> x `appEndo` eps'' - eps))
>     `approximate` eps)/(2*eps)


>instance DifferentiallyClosed R where
>   derivate = derivate_r
>   integral = integral_r

>-- | newton's method for finding root of function.
>-- \[x_{i+1} = x_i - {{f(x_i)}\over{f'(x_i)}}\]
>newtons_method_r :: (R -> R) -> R -> R
>newtons_method_r f x = lim $ iterate_stream iteration x 
>  where iteration z' = z' - f z' / derivate f z'
> 
>instance Numerics R where
>   newtons_method = newtons_method_r

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
>   return $! fromRational $! (16^^(negate k))*(4/(8*kr+1) - 2/(8*kr+4)
>                              - 1/(8*kr+5) - 1/(8*kr+6))

>-- <http://en.wikipedia.org/wiki/Logarithm Logarithm>
>log_r :: R -> R
>log_r a = let a' = (a-1)/(a+1) in
>  2* (lim $ sum_stream $ do   
>     n <- naturals
>     return $! (1 / (2 * fromIntegral n + 1))*a'^(2*n+1))

>integral_r :: (R,R) -> (R -> R) -> R
>integral_r (x,y) f = foldable_integral acc f
>   where acc = map fromRational . integral_accuracy f x y

>foldable_integral :: (Foldable t, Functor t) => (Rational -> t b) -> (b -> R) -> R
>foldable_integral border f = real $ \eps ->
>   foldable_simple_integral border ((`approximate` eps) . f) eps

>foldable_simple_integral :: (Num a, Foldable t, Functor t) =>
>  (a -> t b) -> (b -> a) -> a -> a
>foldable_simple_integral border f eps = eps * (sum $! fmap f $! border eps)

>integral_accuracy :: (Fractional t) => (t -> R) -> R -> R -> Rational -> [Rational]
>integral_accuracy f x y eps = [x',x'+eps..y']
>  where accuracy z = derivate_rational eps mf z
>        mf i = f (fromRational i) `approximate` eps
>        x' = x `approximate` (eps / accuracy (x `approximate` eps))
>        y' = y `approximate` (eps / accuracy (y `approximate` eps))

>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>asin_r :: R -> R
>asin_r x = integral (0,x) $ \z -> 1 / sqrt(1 - z*z)
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>acos_r :: R -> R
>acos_r x = (pi_r / 2) - asin_r x
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>atan_r :: R -> R
>atan_r x = integral (0,x) $ \z -> (1 / (z*z+1))

>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>sin_by_series :: R -> R
>sin_by_series x = lim $ fst $ uninterleave $ sum_stream $ liftA2 (*) filterStream
>                  $ liftA2 (/) (index_powers $ constant x)
>                  $ factorial
>   where filterStream = Pre 0 $ Pre 1 $ Pre 0 $ Pre (negate 1) $ filterStream
>              
>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>cos_by_series :: R -> R
>cos_by_series x = lim $ fst $ uninterleave $ sum_stream $ liftA2 (*) filterStream
>                  $ liftA2 (/) (index_powers $ constant x) factorial
>   where filterStream = Pre 1 $ Pre 0 $ Pre (negate 1) $ Pre 0 $ filterStream

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
