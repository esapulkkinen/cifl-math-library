>{-# LANGUAGE Safe, TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
>{-# LANGUAGE TypeOperators, DataKinds, UndecidableInstances #-}
>{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, GADTs #-}
>{-# LANGUAGE BangPatterns #-}
>module Math.Number.R (
> R(Limit), lim, real, approximate, rationalApproximations, average,
> floatingApproximations, epsilon, infiniteR, liftR, derivateGenericStream,
> liftR2, inverseImage, liftWithAccuracy, approximateAs, 
> limitCompose, inverseImageEndo, differentialLiftR, derivateRational,
> derivateR, newtonsMethodR, sqrtR, logByNewton,
> expR, piR, logR, integralR, foldableIntegral, foldableSimpleIntegral,
> integralAccuracy, asinR, acosR, atanR, sinBySeries, inverseOfR,
> cosBySeries, gammaR, lubR, glbR, computableR, unsafeRealToRational,
> floorR, ceilingR, roundR, truncateR, properFractionR,
> approximatelyLessThanOrEqualR, approximatelyLessThanR,
> approximatelyGreaterThanOrEqualR, approximatelyGreaterThanR,
> approximatelyEqualToR, maxR, minR, realToFloat, realToDouble,
> Limited(..), supremum, infinum, boundedApproximations, upperBounds,
> lowerBounds, piByIntegration, integralAccuracySimple, asinBySeries, acosBySeries, integrateRationalR, integrateRealR,
> atanBySeries, errorFunctionR
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
>import safe Data.List.NonEmpty (NonEmpty)
>import safe qualified Data.List.NonEmpty as NonEmpty
>
>-- | This real representation takes 'epsilon' as input as in epsilon-delta proof.
>newtype R = Limit { approximate_endo :: Endo Rational }


>-- | this realToRational operation sets precision \(\epsilon\) to 0 and produces
>-- corresponding rational number.
>-- Many algorithms for reals will go to infinite loop, if this is used,
>-- or produce divide-by-zero. In particular, this is expected for irrationals.
>-- if rational number is produced, it's exact.
>-- However, computations on rational numbers, where real number was created
>-- using fromRational, will produce that rational. It also allows operations
>-- lifted from rationals to work, producing the result you'd obtain from
>-- rational numbers.
>unsafeRealToRational :: R -> Rational
>unsafeRealToRational r = r `approximate` 0

>floorR :: (Integral b) => R -> b
>floorR r = floor (r `approximate` 1)

>truncateR :: (Integral b) => R -> b
>truncateR r = truncate (r `approximate` 1)

>roundR :: (Integral b) => R -> b
>roundR r = round (r `approximate` (1%2))

>ceilingR :: (Integral b) => R -> b
>ceilingR r = ceiling (r `approximate` (1%2))

>properFractionR :: (Integral b) => R -> (b, R)
>properFractionR r = let res = floorR r in (res, r - fromIntegral res)

Can't implement Real or RealFrac, because (Ord R) is not available.

instance Real R where
   toRational = unsafeRealToRational

instance RealFrac R where
   properFraction = properFraction_r
   truncate = truncate_r
   round = round_r
   ceiling = ceiling_r
   floor = floor_r

>approximatelyLessThanOrEqualR :: Rational -> R -> R -> Bool
>approximatelyLessThanOrEqualR eps f g = (g - f) `approximate` (eps/2) > (-eps)

>approximatelyLessThanR :: Rational -> R -> R -> Bool
>approximatelyLessThanR eps f g = (f - g) `approximate` (eps/2) < eps

>approximatelyGreaterThanOrEqualR :: Rational -> R -> R -> Bool
>approximatelyGreaterThanOrEqualR eps f g = (g - f) `approximate` (eps/2) < eps

>approximatelyGreaterThanR :: Rational -> R -> R -> Bool
>approximatelyGreaterThanR eps f g = (f - g) `approximate` (eps/2) > eps

>approximatelyEqualToR :: Rational -> R -> R -> Bool
>approximatelyEqualToR eps f g = abs (f - g) `approximate` (eps/2) < eps

>average :: R -> R -> R
>average = liftR2 (\x y -> (x+y)/2)

>maxR :: R -> R -> R
>maxR = liftR2 max
>
>minR :: R -> R -> R
>minR = liftR2 min

>-- | tricky stuff. We use the denominator from the rational to determine
>-- what accuracy to use for comparison, then use rational comparison.
>-- Notice that this doesn't really allow good control of precision
>-- of computation, since rational numbers are normalized by default.
>instance DedekindCut Rational R where
>  r %< e = r < (e `approximate` (epsRational r))
>  e <% r = (e `approximate` (epsRational r)) < r

>epsRational :: Rational -> Rational
>epsRational r = 1 % denominator r

>instance DedekindCut Float R where
>  r %< e = r < fromRational (e `approximate` epsFloat)
>  e <% r = fromRational (e `approximate` epsFloat) < r

closure_less_eq :: R -> R -> Closure Stream Bool
closure_less_eq x y = 

>epsFloat :: Rational
>epsFloat = 1 % (floatRadix (1 :: Float) ^ floatDigits (1 :: Float))

>realToDouble :: R -> Double
>realToDouble r = fromRational (r `approximate` epsDouble)

>realToFloat :: R -> Float
>realToFloat r = fromRational (r `approximate` epsFloat)

>instance DedekindCut Double R where
>  r %< e = r < fromRational (e `approximate` epsDouble)
>  e <% r = fromRational (e `approximate` epsDouble) < r

>epsDouble :: Rational
>epsDouble = 1 % (floatRadix (1 :: Double) ^ floatDigits (1 :: Double))

>instance DedekindCut Int R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance DedekindCut Integer R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance DedekindCut Word R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance Limiting ((->) Rational) R where
>   data Closure ((->) Rational) R = RQClosure { runRQClosure :: !R }
>   limit f = RQClosure $ real $ \eps ->
>      let !eps' = eps/2
>      in if abs (f eps `approximate` eps - f eps' `approximate` eps') < eps
>       then f eps' `approximate` eps'
>       else runRQClosure (limit f) `approximate` eps'
>   approximations (RQClosure r) = \eps -> real $ \eps' -> r `approximate` min eps eps'

>instance StreamBuilder ((->) Rational) where
>   pre x f = \eps -> if eps >= 1 then x else f (eps*2)

>instance StreamObserver ((->) Rational) where
>   shead f = f (1%2)
>   stail f = \eps -> f (eps*2)

>instance Limiting Stream R where
>   data Closure Stream R = RClosure { runRClosure :: !R }
>   limit ~(Pre x ~z@(Pre y _)) = RClosure $ real $ \ !eps ->
>       if abs (y `approximate` eps - x `approximate` eps) < eps
>         then y `approximate` eps
>         else runRClosure (limit z) `approximate` eps
>   approximations ~(RClosure r) = fmap f $ fmap (1 /) $ power 10
>      where f p = real $ \eps -> r `approximate` min p eps

>instance ConjugateSymmetric (Closure Stream R) where
>   conj (RClosure r) = RClosure (conj r)
> 
>instance Infinitesimal Stream R where
>  epsilonStream = Pre 1.0 $ fmap (*0.1) epsilonStream

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
>  (+) = liftRClosure2 (+)
>  (-) = liftRClosure2 (-)
>  (*) = liftRClosure2 (*)
>  abs = liftRClosure abs
>  signum = liftRClosure signum
>  negate = liftRClosure negate
>  fromInteger = \i -> RClosure (fromInteger i)

>instance Infinitary (Closure Stream R) where
>   infinite = RClosure infinite

>instance Bounded R where
>   minBound = negate infinite
>   maxBound = infinite

>instance Closed R where
>   accumulationPoint = runRClosure

>lim :: Stream R -> R
>lim = runRClosure . limit

>real :: (Rational -> Rational) -> R
>real = Limit . Endo

>{-# INLINEABLE lim #-}
>{-# INLINEABLE real #-}
>{-# INLINEABLE approximate #-}

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

>rationalApproximationsR :: R -> Stream Rational
>rationalApproximationsR r = fmap (approximate r) $ fmap (1/) $ power 2

>instance Approximations Stream R where
>   floatingApproximations = floatingApproximationsR
>   rationalApproximations = rationalApproximationsR

>floatingApproximationsR :: R -> Stream Double
>floatingApproximationsR r = fmap fromRational $ rationalApproximations r

>displayPrecision :: Rational
>displayPrecision = 1 % 10000000000

>instance Show R where
>   show x = show $! (fromRational (x `approximate` displayPrecision ) :: Double)

>instance PpShow R where
>   pp x = pp $! (fromRational (x `approximate` displayPrecision) :: Double)

>instance CompleteSpace R

>gammaR :: R -> R
>gammaR z = integral (epsilon,1) $ \t -> (log (1/t))**(z-1)

>epsilon :: R
>epsilon = real id

>infiniteR :: R
>infiniteR = liftR (1/) epsilon

>instance Infinitary R where
>   infinite = infiniteR

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
>computableR :: (Rational -> Rational -> Rational) -> R
>computableR f = real $ \eps -> f (-eps/2) (eps/2)

>-- | inverseImage transforms accuracy computation of the real.
>inverseImage :: (Rational -> Rational) -> R -> R
>inverseImage g (Limit (Endo f)) = real $ f . g

>-- | The first argument is the function lifted. The second argument
>-- describes change in accuracy by the function.
>liftWithAccuracy :: (Rational -> Rational) -> (Rational -> Rational) -> R -> R
>liftWithAccuracy f acc = \r -> liftR f (inverseImage acc r)

>-- | approxCompose will use the second real as the level
>--   of approximation to compute the first.
>approximateAs :: R -> R -> R
>approximateAs (Limit f) (Limit g) = Limit (f `mappend` g)

>limitCompose :: (Rational -> R) -> R -> R
>limitCompose  g (Limit f) = real $
> \eps -> approximate_endo (g (f `appEndo` eps)) `appEndo` eps


>inverseImageEndo :: Endo Rational -> Endo R
>inverseImageEndo g = Endo $ \ (Limit f) -> real $ appEndo g . appEndo f
> 

>lubR :: [R] -> Maybe R
>lubR [] = Nothing
>lubR (Limit c:cr) = maybe (Just (Limit c)) f (lubR cr)
>   where f (Limit v) = Just $ real $ \eps ->
>              max (v `appEndo` eps) (c `appEndo` eps)
>
>glbR :: [R] -> Maybe R
>glbR [] = Nothing
>glbR (Limit c:cr) = maybe (Just (Limit c)) f (glbR cr)
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
>  -- fromInteger 0 = real id
>   fromInteger i = real $ const (fromInteger i)

>-- | For division, if a rational approximation of the divisor
>-- is zero, we increase accuracy. Notice this makes
>-- a real number constructed from "real (const 0)" behave
>-- much worse (nontermination) than "epsilon"
>-- (which just increases accuracy)
>-- Using L'Hôpital's rule for 0/0
>-- <https://en.wikipedia.org/wiki/L%27H%C3%B4pital%27s_rule>
>instance Fractional R where
>   (Limit f) / (Limit g) = real checker
>     where divisor eps = g `appEndo` (eps/2)
>           dividend eps = f `appEndo` (eps/2)
>           checker eps
>              | 0 /= divisor eps = dividend eps * recip (divisor eps)
>              | 0 == dividend eps = 
>                  (real (derivateRational (eps/2) $ appEndo f)
>                   / real (derivateRational (eps/2) $ appEndo g)) `approximate` eps
>              | otherwise = checker (eps/2)
>   recip (Limit f) = real checker
>     where divisor eps = (f `appEndo` recip eps)
>           checker eps = if 0 /= divisor eps then recip (divisor eps)
>                               else checker (eps/2)
>  --  fromRational 0  = real id
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

>instance MetricSpace R where
>   type Distance R = R
>   distance x y = abs (x - y)


>-- | This lifts a rational function to a real function.
>-- This computes accuracy using the formula \(dx = df / f'(x)\).
>-- The accuracy behaves badly when \(f'(x) = 0\) due to divide-by-zero.
>differentialLiftR :: (Rational -> Rational) -> R -> R
>differentialLiftR f = \(Limit (Endo g)) -> Limit $ Endo $ \df -> f $ g (1 / (derivateRational df f (g df)))

>-- | derivate for rational functions. The first argument is epsilon.
>{-# INLINEABLE derivateRational #-}
>derivateRational :: Rational -> (Rational -> Rational) -> Rational -> Rational
>derivateRational !eps f = \i -> (f (i + eps) - f i) / eps

>derivateGenericStream :: (Fractional a) => (a -> a) -> Stream a -> Stream a
>derivateGenericStream f (Pre !i z@(Pre !di r)) = let
>    f' !di' !i' = (f (i' + di') - f i') / di'
>  in Pre (f' di i) (derivateGenericStream (\dd -> f' dd i) z)

>-- | computes derivate. \[{{df}\over{dt}} = \lim_{\epsilon\rightarrow 0}{{f(t+\epsilon)-f(t)}\over{\epsilon}}\]
>{-# INLINEABLE derivateR #-}
>derivateR :: (R -> R) -> R -> R
>derivateR f xx@(Limit x) = let !fx = f xx in real $ \ !eps ->
>    ((f (real $ \eps' -> x `appEndo` eps' + eps) - fx)
>     `approximate` eps)/eps
>        -- computing fx separately is an optimization that should allow
>        -- sharing the value of 'fx' to many invocations at different precision.
> 
>instance DifferentiallyClosed R where
>   derivate = derivateR
>   integral = integralR

>-- | newton's method for finding root of function.
>-- \[x_{i+1} = x_i - {{f(x_i)}\over{f'(x_i)}}\]
>newtonsMethodR :: (R -> R) -> R -> R
>newtonsMethodR f = \x -> lim $ iterateStream iteration x 
>  where iteration = \ z' -> z' - f z' / derivate f z'
> 
>instance Numerics R where
>   newtonsMethod = newtonsMethodR

>-- | square root of a real number \(x\), computed with newton's method as root of \(t \mapsto t^2 - x\).
>sqrtR :: R -> R
>sqrtR = \v -> newtonsMethod (\x -> x*x - v) 1

>-- | logarithm of a real number \(x\), computed with newton's method as root of \(t \mapsto e^t - x\).
>logByNewton :: R -> R
>logByNewton = \v -> newtonsMethod (\x -> exp x - v) 1

>inverseOfR :: (R -> R) -> R -> R
>inverseOfR f = \ v -> newtonsMethod (\x -> f x - v) 1

>expR :: R -> R
>expR x = runRClosure $ limit $ do
>   xappr <- approximations $ RClosure x
>   sumStream $ liftA2 (/) (indexPowers (constant xappr)) factorial

>-- | Using Simon Plouffe's BPP digit extraction algorithm for computing pi.
>-- See <https://plouffe.fr/Simon%20Plouffe.htm>
>-- See <https://secure.wikimedia.org/wikipedia/en/wiki/Pi Pi> for details.
>-- \[\pi = \sum_{k=0}^{\infty}{16^{-k}}({{4}\over{8k+1}}-{{2}\over{8k+4}}-{1\over{8k+5}}-{1\over{8k+6}})\]
>piR :: R
>piR = lim $ sumStream $ do
>   k <- naturals
>   let !kr = k % 1
>   return $ fromRational $ (16^^(negate k))*(4/(8*kr+1) - 2/(8*kr+4)
>                              - 1/(8*kr+5) - 1/(8*kr+6))


>-- <http://en.wikipedia.org/wiki/Logarithm Logarithm>
>logR :: R -> R
>logR = \ a -> let !a' = (a-1)/(a+1) in
>  2* (lim $ sumStream $ do   
>     n <- naturals
>     return $! (1 / (2 * fromIntegral n + 1))*a'^(2*n+1))


<https://en.wikipedia.org/wiki/Fourier_series> <https://en.wikipedia.org/wiki/Fourier_transform>
fourierCoefficient :: (Complex R -> Complex R) -> Integer -> Complex R
fourierCoefficient f n = (1/(2*pi)) %* integralR (-pi,pi) (\x -> f (x :+ 0) * exp ((- (0:+1)* fromRational (fromIntegral n)) * (x :+ 0)))


>-- Integrating a rational function.
>-- The function takes 'dx' and 'x' as parameter.
>-- \( \int{x \in [a,b]}{x^2 dx} =def= integrateR (a,b) (\dx x -> x*x*dx) \)
>integrateRationalR :: (R,R) -> (Rational -> Rational -> Rational) -> R
>integrateRationalR (x,y) f = real $ foldableComplexIntegral acc f
>  where acc = integralAccuracySimple x y

>-- Integrating a real function. The function takes dx and x as parameter.
>-- \( integrate\Real\R (a,b) f = \int_a^b f(dx,x) \)
>-- Notice f must include the 'dx' multiplier of the integrand, but more creative uses of dx are possible.
>integrateRealR :: (R,R) -> (R -> R -> R) -> R
>integrateRealR (a,b) f = integrateRationalR (a,b) $ \ !dx x' ->
>    f (fromRational dx) (fromRational x') `approximate` dx


>{-# NOINLINE integralR #-}
>-- integral[a,b] f(x)dx.
>integralR :: (R,R) -> (R -> R) -> R
>integralR (x,y) = \f -> let acc = integralAccuracySimple x y
>                          in foldableIntegral acc (f . fromRational)

>{-# SPECIALIZE foldableIntegral :: (Rational -> [Rational]) -> (Rational -> R) -> R #-}
>foldableIntegral :: (Foldable t, Functor t) => (Rational -> t b) -> (b -> R) -> R
>foldableIntegral border = \f -> real $ \ !eps ->
>   foldableSimpleIntegral border ((`approximate` eps) . f) eps

>foldable_vectorIntegral :: (VectorSpace (v a), Scalar (v a) ~ a, Foldable t, Functor t)
>  => (a -> t b) -> (b -> v a) -> a -> v a
>foldable_vectorIntegral border f = \eps -> eps %* (vsum $ fmap f $ border eps)

>{-# SPECIALIZE foldableSimpleIntegral :: (Rational -> [Rational]) -> (Rational -> Rational) -> Rational -> Rational #-}
>foldableSimpleIntegral :: (Num a, Foldable t, Functor t) =>
>  (a -> t b) -> (b -> a) -> a -> a
>foldableSimpleIntegral border = \f !eps -> sum $
>   fmap ((eps*) . f) $ border eps

>foldableComplexIntegral :: (Num a, Foldable t, Functor t) =>
>  (c -> t b) -> (c -> b -> a) -> c -> a
>foldableComplexIntegral border f !eps = sum $
>   fmap (f eps) $ border eps

>{-# INLINE integralAccuracySimple #-}
>integralAccuracySimple :: R -> R -> Rational -> [Rational]
>integralAccuracySimple x y !eps = [x',x' + eps .. y']
>   where !x' = x `approximate` eps
>         !y' = y `approximate` eps

>{-# SPECIALIZE integralAccuracy :: (R -> R) -> R -> R -> Rational -> [Rational] #-}
>integralAccuracy :: (Fractional t) => (t -> R) -> R -> R -> Rational -> [Rational]
>integralAccuracy f x y = res
>  where res eps = let x'' = x' $ eps ; y'' = y' $ eps in [x'', x''+eps .. y'']
>        accuracy eps z = derivateRational eps (mf eps) z
>        mf eps i = f (fromRational i) `approximate` eps
>        x' eps = x `approximate` (eps / accuracy eps (x `approximate` eps))
>        y' eps = y `approximate` (eps / accuracy eps (y `approximate` eps))

>piByIntegration :: R
>piByIntegration = acos (negate 1)

>-- | <https://proofwiki.org/wiki/Power_Series_Expansion_for_real_Arcsine_Function>
>-- Computed by MacLaurin series expansion of arc sine:
>-- \( \arcsin x = \sum^{\infty}_{n=0}{{(2n)! x^{2n+1}}\over{2^{2n}(n!)^2(2n+1)}} \)
>asinBySeries :: R -> R
>asinBySeries x = ssum ser
>  where two_n_p1_power_of_x = Pre x $ fmap (\a -> x*x*a) two_n_p1_power_of_x
>        two_n_p1 = Pre 1 $ fmap (\a -> 2+a) two_n_p1
>        factorialSquared = fmap (\x -> x*x) factorial
>        two_n_factorial = fmap fromIntegral $ fst (uninterleave factorial)
>        two_powerTwo_n = Pre 1 $ fmap (\a -> 2*2*a) two_powerTwo_n
>        ser = liftA2 (/)
>                (liftA2 (*) two_n_factorial two_n_p1_power_of_x)
>                (fmap fromIntegral $ liftA2 (*) two_powerTwo_n $
>                 liftA2 (*) factorialSquared two_n_p1)

>acosBySeries :: R -> R
>acosBySeries x = (pi/2) - asinBySeries x

>-- | <https://en.wikipedia.org/wiki/Arctangent_series>
>-- Computed using Gregory's series:
>-- \( \arctan x = \sum^{\infty}_{n=0}{{(-1)^kx^{2k+1}}\over{2k+1}} \)
>atanBySeries :: R -> R
>atanBySeries x = ssum $ liftA2 (/) (liftA2 (*) alternatingSigns two_n_p1_power_of_x) (fmap fromIntegral two_n_p1)
>  where two_n_p1_power_of_x = Pre x $ fmap (\a -> x*x*a) two_n_p1_power_of_x
>        two_n_p1 = Pre 1 $ fmap (\a -> 2+a) two_n_p1

>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>asinR :: R -> R
>asinR = \x -> integral (0,x) $ \z -> 1 / sqrt(1 - z*z)
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>acosR :: R -> R
>acosR = \x -> integral (x,1) $ \z -> 1 / sqrt(1 - z*z)
> -- (piR / 2) - asinR x
>
>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions>
>atanR :: R -> R
>atanR = \x -> integral (0,x) $ \z -> (1 / (z*z+1))

>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- Computed using taylor series expansion of sin function.
>-- \[\sin(x) = \sum_{k=0}^{\infty}{{{(-1)^k}\over{(2k+1)!}}{x^{2k+1}}}\]
>sinBySeries :: R -> R
>sinBySeries = \x -> lim $ fst $ uninterleave $ sumStream $ liftA2 (*) filterStream'
>                  $ liftA2 (/) (indexPowers $ constant x)
>                  $ factorial
>   where filterStream = Pre 0 $ Pre 1 $ Pre 0 $ Pre (negate 1) $ filterStream
>         filterStream' = fmap fromIntegral filterStream
>              
>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- Computed using taylor series expansion of cos function.
>-- \[\cos(x) = \sum_{k=0}^{\infty}{{{(-1)^k}\over{(2k)!}}{x^{2k}}}\]
>cosBySeries :: R -> R
>cosBySeries = \x -> lim $ fst $ uninterleave $ sumStream $ liftA2 (*) filterStream'
>                  $ liftA2 (/) (indexPowers $ constant x) factorial
>   where filterStream = Pre 1 $ Pre 0 $ Pre (negate 1) $ Pre 0 $ filterStream
>         filterStream' = fmap fromIntegral filterStream

>-- <https://en.wikipedia.org/wiki/Normal_Distribution>
>errorFunctionR :: R -> R
>errorFunctionR x = (2/(sqrt pi) ) * (integrateRealR (0,x) $ \dt t -> exp (negate $ t*t)*dt)


>-- | <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions inverse trigonometric functions>
>-- <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance Floating R where
>   pi = piR
>   exp = expR
>   log = logR
>   sqrt = sqrtR
>   x ** y = exp (y * log x)
>   sin = sinBySeries
>   cos = cosBySeries
>   tan x = sin x / cos x
>   asin = asinBySeries
>   acos = acosBySeries
>   atan = atanBySeries
>   sinh x = (exp x - exp (negate x)) / 2
>   cosh x = (exp x + exp (negate x)) / 2
>   tanh x = sinh x / cosh x
>   asinh x = log $ x + sqrt (x*x+1)
>   acosh x = log $ x + sqrt (x*x-1)
>   atanh x = log ((1+x)/(1-x)) / 2

>data Limited a where
>   LimitedRationalsAbove :: (Rational -> Rational) -> NonEmpty Rational -> R -> Limited Rational
>   LimitedRationalsBelow :: (Rational -> Rational) -> NonEmpty Rational -> R -> Limited Rational
>   LimitedRationalsAboveBelow :: (Rational -> Rational) -> NonEmpty Rational -> NonEmpty Rational -> R -> R -> Limited Rational

>supremum :: Limited Rational -> Maybe R
>supremum (LimitedRationalsAbove _ _ s) = Just s
>supremum (LimitedRationalsAboveBelow _ _ _ _ s) = Just s
>supremum _ = Nothing
>
>infinum :: Limited Rational -> Maybe R
>infinum (LimitedRationalsBelow _ _ i) = Just i
>infinum (LimitedRationalsAboveBelow _ _ _ i _) = Just i
>infinum _ = Nothing

>boundedApproximations :: Limited Rational -> Rational -> Rational
>boundedApproximations (LimitedRationalsAbove f _ _) = f
>boundedApproximations (LimitedRationalsBelow f _ _) = f
>boundedApproximations (LimitedRationalsAboveBelow f _ _ _ _) = f

>upperBounds :: Limited Rational -> [Rational]
>upperBounds (LimitedRationalsAbove _ ub _ ) = NonEmpty.toList ub
>upperBounds (LimitedRationalsAboveBelow _ _ ub _ _) = NonEmpty.toList ub
>upperBounds _ = []

>lowerBounds :: Limited Rational -> [Rational]
>lowerBounds (LimitedRationalsBelow _ lb _) = NonEmpty.toList lb
>lowerBounds (LimitedRationalsAboveBelow _ lb _ _ _) = NonEmpty.toList lb
>lowerBounds _ = []
