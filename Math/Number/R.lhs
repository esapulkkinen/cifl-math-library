>{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
>{-# HLINT ignore "Use camelCase" #-}
>{-# LANGUAGE Safe, TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
>{-# LANGUAGE TypeOperators, DataKinds, UndecidableInstances #-}
>{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, GADTs #-}
>module Math.Number.R (
> R(Limit), lim, real, approximate, rational_approximations, 
> floating_approximations, epsilon, infinite_r, liftR, derivate_generic_stream,
> liftR2, inverseImage, liftWithAccuracy, approximate_as, 
> limit_compose, inverseImageEndo, differentialLiftR, derivate_rational,
> derivate_r, newtons_method_r, sqrt_r, log_by_newton,
> exp_r, pi_r, log_r, integral_r, foldable_integral, foldable_simple_integral,
> integral_accuracy, asin_r, acos_r, atan_r, sin_by_series, inverse_of_r,
> cos_by_series, gamma_r, lub_r, glb_r, computable_r, unsafe_real_to_rational,
> floor_r, ceiling_r, round_r, truncate_r, properFraction_r,
> approximately_less_than_or_equal_r, approximately_less_than_r,
> approximately_greater_than_or_equal_r, approximately_greater_than_r,
> approximately_equal_to_r, max_r, min_r, real_to_float, real_to_double
> ) where
>import safe Control.Applicative
>import safe Control.Monad
>import safe Data.Char
>import safe Data.Ratio
>import safe Data.Monoid
>import safe Data.Complex
>import safe Data.Type.Equality
>import safe Math.Tools.PrettyP
>import safe Math.Matrix.Covector
>import safe Math.Matrix.Interface
>import safe Math.Number.StreamInterface
>import safe Math.Tools.Median
>import safe Math.Number.Interface
>import safe Math.Number.Stream
>import safe qualified Control.Monad.Fix
>import safe GHC.TypeLits
>
>
>-- | This real representation takes 'epsilon' as input as in epsilon-delta proof.
>newtype R = Limit { approximate_endo :: Endo Rational }

>-- | this real_to_rational operation sets precision \(\epsilon\) to 0 and produces
>-- corresponding rational number.
>-- Many algorithms for reals will go to infinite loop, if this is used,
>-- or produce divide-by-zero. In particular, this is expected for irrationals.
>-- if rational number is produced, it's exact.
>-- However, computations on rational numbers, where real number was created
>-- using fromRational, will produce that rational. It also allows operations
>-- lifted from rationals to work, producing the result you'd obtain from
>-- rational numbers.
>unsafe_real_to_rational :: R -> Rational
>unsafe_real_to_rational r = r `approximate` 0

>floor_r :: (Integral b) => R -> b
>floor_r r = floor (r `approximate` 1)

>truncate_r :: (Integral b) => R -> b
>truncate_r r = truncate (r `approximate` 1)

>round_r :: (Integral b) => R -> b
>round_r r = round (r `approximate` (1%2))

>ceiling_r :: (Integral b) => R -> b
>ceiling_r r = ceiling (r `approximate` (1%2))

>properFraction_r :: (Integral b) => R -> (b, R)
>properFraction_r r = let res = floor_r r in (res, r - fromIntegral res)

Can't implement Real or RealFrac, because (Ord R) is not available.

instance Real R where
   toRational = unsafe_real_to_rational

instance RealFrac R where
   properFraction = properFraction_r
   truncate = truncate_r
   round = round_r
   ceiling = ceiling_r
   floor = floor_r

>approximately_less_than_or_equal_r :: Rational -> R -> R -> Bool
>approximately_less_than_or_equal_r eps f g = f `approximate` eps <= g `approximate` eps

>approximately_less_than_r :: Rational -> R -> R -> Bool
>approximately_less_than_r eps f g = f `approximate` eps < g `approximate` eps

>approximately_greater_than_or_equal_r :: Rational -> R -> R -> Bool
>approximately_greater_than_or_equal_r eps f g = f `approximate` eps >= g `approximate` eps

>approximately_greater_than_r :: Rational -> R -> R -> Bool
>approximately_greater_than_r eps f g = f `approximate` eps > g `approximate` eps

>approximately_equal_to_r :: Rational -> R -> R -> Bool
>approximately_equal_to_r eps f g = f `approximate` eps == g `approximate` eps

>max_r :: R -> R -> R
>max_r = liftR2 max
>
>min_r :: R -> R -> R
>min_r = liftR2 min

>-- | tricky stuff. We use the denominator from the rational to determine
>-- what accuracy to use for comparison, then use rational comparison.
>-- Notice that this doesn't really allow good control of precision
>-- of computation, since rational numbers are normalized by default.
>instance DedekindCut Rational R where
>  r %< e = r < (e `approximate` (eps_rational r))
>  e <% r = (e `approximate` (eps_rational r)) < r

>eps_rational :: Rational -> Rational
>eps_rational r = 1 % denominator r

>instance DedekindCut Float R where
>  r %< e = r < fromRational (e `approximate` eps_float)
>  e <% r = fromRational (e `approximate` eps_float) < r

>eps_float :: Rational
>eps_float = 1 % (floatRadix (1 :: Float) ^ floatDigits (1 :: Float))

>real_to_double :: R -> Double
>real_to_double r = fromRational (r `approximate` eps_double)

>real_to_float :: R -> Float
>real_to_float r = fromRational (r `approximate` eps_float)

>instance DedekindCut Double R where
>  r %< e = r < fromRational (e `approximate` eps_double)
>  e <% r = fromRational (e `approximate` eps_double) < r

>eps_double :: Rational
>eps_double = 1 % (floatRadix (1 :: Double) ^ floatDigits (1 :: Double))

>instance DedekindCut Int R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance DedekindCut Integer R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance DedekindCut Word R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance Limiting Stream R where
>   data Closure Stream R = RClosure { runRClosure :: !R }
>   limit ~(Pre x ~z@(Pre y _)) = RClosure $ real $ \eps ->
>       if abs (y `approximate` eps - x `approximate` eps) < eps
>         then y `approximate` eps
>         else runRClosure (limit z) `approximate` eps
>   approximations ~(RClosure r) = fmap f $ fmap (1 /) $ power 10
>      where f p = real $ \eps -> r `approximate` min p eps

>instance Infinitesimal Stream R where
>  epsilon_stream = Pre 1.0 $ fmap (/10.0) epsilon_stream

>instance Fractional (Closure Stream R) where
>  (RClosure x) / (RClosure y) = RClosure (x/y)
>  recip (RClosure x) = RClosure (recip x)
>  fromRational r = RClosure (fromRational r)
>
>liftRClosure :: (R -> R) -> Closure Stream R -> Closure Stream R
>liftRClosure f = \(RClosure x) -> RClosure (f x)
> 
>liftRClosure2 :: (R -> R -> R) -> Closure Stream R -> Closure Stream R -> Closure Stream R
>liftRClosure2 f (RClosure x) (RClosure y) = RClosure (f x y)
>
>instance Floating (Closure Stream R) where
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

>instance Num (Closure Stream R) where
>  (RClosure x) + (RClosure y) = RClosure (x + y)
>  (RClosure x) - (RClosure y) = RClosure (x - y)
>  (RClosure x) * (RClosure y) = RClosure (x * y)
>  abs (RClosure x) = RClosure (abs x)
>  signum (RClosure x) = RClosure (signum x)
>  negate (RClosure x) = RClosure (negate x)
>  fromInteger i = RClosure (fromInteger i)

>instance Infinitary (Closure Stream R) where
>   infinite = RClosure infinite

>instance Closed R where
>   accumulation_point = runRClosure

>lim :: Stream R -> R
>lim = runRClosure . limit

>real :: (Rational -> Rational) -> R
>real = Limit . Endo

>-- |
>-- approximate takes a real \(r\), and a rational \(q\)
>-- and produces a rational \(q_2\) such that \(|r - q_2| \le q\)
>--
>-- \(r_1 (r_2 q) = (r_1 r_2) q\), where \(r_1,r_2 \in {\mathbf{R}}\), \(q \in {\mathbf{Q}}\).
>--
>-- that is,
>-- \[{\mathbf{approximate}}(r_1) \circ {\mathbf{approximate}}(r_2) = {\mathbf{approximate}}(r_1 r_2)\]
>-- that is,
>-- \[{\mathbf{approximate}}(r_1,{\mathbf{approximate}}(r_2,q)) = {\mathbf{approximate}}(r_1 r_2, q)\]
>-- that is
>-- \[{\mathbf{approximate}}(r_1,q_3)=q_2 \Leftrightarrow |r_1 - q_2| \le q_3\]
>-- \[{\mathbf{approximate}}(r_2,q) =q_3 \Leftrightarrow |r_2 - q_3| \le q\]
>-- \[{\mathbf{approximate}}(r_1 r_2, q)=q_2 \Leftrightarrow |r_1 r_2 - q_2| \le q\]
>--  \[{\mathbf{approximate}}(r_1 r_2, q)={\mathbf{approximate}}(r_1,q_3)={\mathbf{approximate}}(r_1,{\mathbf{approximate}}(r_2,q))\]
>--  \[\Leftrightarrow |r_1 - q_2| \le q_3 \land |r_2 - q_3| \le q\]
>approximate :: R -> Rational -> Rational
>approximate = appEndo . approximate_endo

>rational_approximations_r :: R -> Stream Rational
>rational_approximations_r r = fmap (approximate r) $ fmap (1/) $ power 10

>instance Approximations Stream R where
>   floating_approximations = floating_approximations_r
>   rational_approximations = rational_approximations_r

>floating_approximations_r :: R -> Stream Double
>floating_approximations_r r = fmap fromRational $ rational_approximations r

>instance Show R where
>   show x = show $! (fromRational (x `approximate` (1 % 1000000000000000)) :: Double)

>instance PpShow R where
>   pp x = pp $! (fromRational (x `approximate` (1 % 1000000000000000)) :: Double)

>instance CompleteSpace R

>gamma_r :: R -> R
>gamma_r z = integral (epsilon,1) $ \t -> (log (1/t))**(z-1)

>epsilon :: R
>epsilon = real id

>infinite_r :: R
>infinite_r = liftR (1/) epsilon

>instance Infinitary R where
>   infinite = infinite_r

>-- | this propagates accuracy without changing it.
>liftR :: (Rational -> Rational) -> R -> R
>liftR g = \ (Limit (Endo f)) -> real $ g . f

>-- | this propagates accuracy without changing it
>liftR2 :: (Rational -> Rational -> Rational) -> R -> R -> R
>liftR2 h (Limit f) (Limit g) = real $ \eps -> h (f `appEndo` eps) (g `appEndo` eps)

>-- | For \(a,b \in {\mathbb Q}\) and \(0 \in (a,b) \) the invocation \(f(a)(b)\) of \(f\)
>-- in \(computable\_r(f)\) must produce
>-- approximation \(q\) of another real \(r\) as rational number, such that
>-- \( r - q \in (a,b) \). 
>computable_r :: (Rational -> Rational -> Rational) -> R
>computable_r f = real $ \eps -> f (-eps/2) (eps/2)

>-- | inverseImage transforms accuracy computation of the real.
>inverseImage :: (Rational -> Rational) -> R -> R
>inverseImage g (Limit (Endo f)) = real $ f . g

>-- | The first argument is the function lifted. The second argument
>-- describes change in accuracy by the function.
>liftWithAccuracy :: (Rational -> Rational) -> (Rational -> Rational) -> R -> R
>liftWithAccuracy f acc = \r -> liftR f (inverseImage acc r)

>-- | approx_compose will use the second real as the level
>--   of approximation to compute the first.
>approximate_as :: R -> R -> R
>approximate_as (Limit f) (Limit g) = Limit (f `mappend` g)

>limit_compose :: (Rational -> R) -> R -> R
>limit_compose  g (Limit f) = real $
> \eps -> approximate_endo (g (f `appEndo` eps)) `appEndo` eps


>inverseImageEndo :: Endo Rational -> Endo R
>inverseImageEndo g = Endo $ \ (Limit f) -> real $ appEndo g . appEndo f
> 

>lub_r :: [R] -> Maybe R
>lub_r [] = Nothing
>lub_r (Limit c:cr) = maybe (Just (Limit c)) f (lub_r cr)
>   where f (Limit v) = Just $ real $ \eps ->
>              max (v `appEndo` eps) (c `appEndo` eps)
>
>glb_r :: [R] -> Maybe R
>glb_r [] = Nothing
>glb_r (Limit c:cr) = maybe (Just (Limit c)) f (glb_r cr)
>   where f (Limit v) = Just $ real $ \eps ->
>             min (v `appEndo` eps) (c `appEndo` eps)

>-- | Note zero reals are special, we avoid divide by zero by implementing
>-- zero as epsilon.
>instance Num R where
>   (+) = liftR2 (+)
>   (-) = liftR2 (-)
>   f * g = real $ \eps -> f `approximate` (eps/2)
>                        * g `approximate` (eps/2)
>   negate = liftR negate
>   abs = liftR abs
>   signum = liftR signum
>   fromInteger 0 = real id
>   fromInteger i = real $ const (fromInteger i)

>-- | For division, if a rational approximation of the divisor
>-- is zero, we increase accuracy. Notice this makes
>-- a real number constructed from "real (const 0)" behave
>-- much worse (nontermination) than "epsilon"
>-- (which just increases accuracy)
>instance Fractional R where
>   (Limit f) / (Limit g) = real checker
>     where divisor eps = g `appEndo` (eps/2)
>           dividend eps = f `appEndo` (eps/2)
>           checker eps
>              | 0 /= divisor eps = dividend eps * recip (divisor eps)
>              | otherwise = checker (eps/2)
>   recip (Limit f) = real checker
>     where divisor eps = (f `appEndo` recip eps)
>           checker eps = if 0 /= divisor eps then recip (divisor eps)
>                               else checker (eps/2)
>   fromRational 0  = real id
>   fromRational x  = real $ const x


>instance ConjugateSymmetric R where
>   conj (Limit f) = real (conj . appEndo f)

>-- | R as an infinite dimensional vector space over the rationals.
>instance VectorSpace R where
>   type Scalar R = Rational
>   vzero = epsilon
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
>derivate_rational eps f i = (f (i + eps) - fi) / eps
>   where fi = f i

>derivate_generic_stream :: (Fractional a) => (a -> a) -> Stream a -> Stream a
>derivate_generic_stream f (Pre i z@(Pre di r)) = let
>    f' di' i' = (f (i' + di') - f i') / di'
>  in Pre (f' di i) (derivate_generic_stream (\dd -> f' dd i) z)

>-- | computes derivate. \[{{df}\over{dt}} = \lim_{\epsilon\rightarrow 0}{{f(t+\epsilon)-f(t)}\over{\epsilon}}\]
>derivate_r :: (R -> R) -> R -> R
>derivate_r f = \ xx@(Limit x) -> let fx = f $! xx in real $ \eps ->
>    ((f (real $ \eps' -> x `appEndo` eps' + eps) - fx)
>     `approximate` eps)/eps
>        -- computing fx separately is an optimization that should allow
>        -- sharing the value of 'fx' to many invocations at different precision.
> 
>instance DifferentiallyClosed R where
>   derivate = derivate_r
>   integral = integral_r

>-- | newton's method for finding root of function.
>-- \[x_{i+1} = x_i - {{f(x_i)}\over{f'(x_i)}}\]
>newtons_method_r :: (R -> R) -> R -> R
>newtons_method_r f = \x -> lim $ iterate_stream iteration x 
>  where iteration = \z' -> z' - f z' / derivate f z'
> 
>instance Numerics R where
>   newtons_method = newtons_method_r

>-- | square root of a real number \(x\), computed with newton's method as root of \(t \mapsto t^2 - x\).
>sqrt_r :: R -> R
>sqrt_r v = newtons_method (\x -> x*x - v) 1

>-- | logarithm of a real number \(x\), computed with newton's method as root of \(t \mapsto e^t - x\).
>log_by_newton :: R -> R
>log_by_newton v = newtons_method (\x -> exp x - v) 1

>inverse_of_r :: (R -> R) -> R -> R
>inverse_of_r f = \ v -> newtons_method (\x -> f x - v) 1

>exp_r :: R -> R
>exp_r x = runRClosure $ limit $ do
>   xappr <- approximations $ RClosure x
>   sum_stream $ liftA2 (/) (index_powers (constant xappr)) factorial

>-- | Using Simon Plouffe's BPP digit extraction algorithm for computing pi.
>-- See <https://plouffe.fr/Simon%20Plouffe.htm>
>-- See <https://secure.wikimedia.org/wikipedia/en/wiki/Pi Pi> for details.
>-- \[\pi = \sum_{k=0}^{\infty}{16^{-k}}({{4}\over{8k+1}}-{{2}\over{8k+4}}-{1\over{8k+5}}-{1\over{8k+6}})\]
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
>     return $! (1 / (2 * fromIntegral n + 1))*a'^(2*n+1))

>integral_r :: (R,R) -> (R -> R) -> R
>integral_r (x,y) = \f -> let acc = map fromRational . integral_accuracy f x y
>                          in foldable_integral acc f

>foldable_integral :: (Foldable t, Functor t) => (Rational -> t b) -> (b -> R) -> R
>foldable_integral border = \f -> real $ \eps ->
>   foldable_simple_integral border ((`approximate` eps) . f) eps

>foldable_vector_integral :: (VectorSpace (v a), Scalar (v a) ~ a, Foldable t, Functor t)
>  => (a -> t b) -> (b -> v a) -> a -> v a
>foldable_vector_integral border f = \eps -> eps %* (vsum $! fmap f $! border eps)

>foldable_simple_integral :: (Num a, Foldable t, Functor t) =>
>  (a -> t b) -> (b -> a) -> a -> a
>foldable_simple_integral border f = \eps -> sum $ fmap ((eps*) . f) $ border eps

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
>acos_r x = integral (x,1) $ \z -> 1 / sqrt(1 - z*z)
> -- (pi_r / 2) - asin_r x
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>atan_r :: R -> R
>atan_r x = integral (0,x) $ \z -> (1 / (z*z+1))

>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- Computed using taylor series expansion of sin function.
>-- \[\sin(x) = \sum_{k=0}^{\infty}{{{(-1)^n}\over{(2n+1)!}}{x^{2n+1}}}\]
>sin_by_series :: R -> R
>sin_by_series x = lim $ fst $ uninterleave $ sum_stream $ liftA2 (*) (fmap fromIntegral filterStream)
>                  $ liftA2 (/) (index_powers $ constant x)
>                  $ factorial
>   where filterStream = Pre 0 $ Pre 1 $ Pre 0 $ Pre (negate 1) $ filterStream
>              
>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- Computed using taylor series expansion of cos function.
>-- \[\cos(x) = \sum_{k=0}^{\infty}{{{(-1)^n}\over{(2n)!}}{x^{2n}}}\]
>cos_by_series :: R -> R
>cos_by_series x = lim $ fst $ uninterleave $ sum_stream $ liftA2 (*) (fmap fromIntegral filterStream)
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
