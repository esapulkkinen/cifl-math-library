>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, TypeOperators, TypeFamilies, PatternGuards, UnicodeSyntax #-}
>{-# LANGUAGE DataKinds, UndecidableInstances #-}
>module Math.Number.Real
> (
> R(Limit), Modulus(..), d, liftReal, convergenceRatioTest,
> averageConvergenceRatio, listAverage, cauchy, cseqDifferenceMatrix,
> cauchySequenceEquivalence, cseqEquivalenceMatrix, cseqConvergence,
> cseqEquivalence_list, converges, increaseAccuracyBy,
> partialDerive, partialDerivateClosure, partialDerivate,
> partialDerivateEndo, complexDerivate, average, completenessOfReals,
> liftRClosure, limitReal, liftReal, series, supremumGen, negate_limit,
> infimumGen, infinity, infinityGen, negative_infinityGen,
> limitBelow, limitAbove, limitBoth, negative_infinity, epsilonLinear,
> increaseAccuracy, lessthanPrecision, limitingDistance, distanceMatrix,
> atPrecisionRational, atPrecision, precisionRational,
> realDerivate, realExp, derivateClosed, vectorDerivate, pseudoDerivate,
> derivatesAt, derivateAround, partialDerivate1_2, partialDerivate2_2,
> partialDerivate1_3, partialDerivate2_3, partialDerivate3_3, agm,
> expo, newtonsMethod, eigenvalue, integrate_vector, integrate,
> integralReal, integralRational, integrateCurve, gamma_via_integral,
> approximatelyEqual, approximatelyEq, constructively_less, equality,
> signumReal, showAtPrecisionReal, properFractionReal, enumFromThenToReal,
> fromThenTo, fromTo, enumFromToReal, realEnumFromThenTo, strEnumFromThenTo,
> toReal, logarithm, eulerConstant, napiersConstant, logarithmByNewton,
> invertByNewton, expByPowers, expByPowersReal,
> sumGeneratingFunction, riemann_zeta, riemann_zetaReal, riemann_zetaComplex,
> encodeInBase, fromFloat, goldenRatio, slowGoldenRatio, logistic,
> floatingApproximations, logarithmByPower, tetrate, tetrate2,
> logNewton, expBySeries2, expBySeries, sinBySeries, cosBySeries,
> approximateSumsModulus, withInverseModulus, partialDerivateLinear
> ) where
>import safe Prelude hiding (zipWith,take,zip,zip3, id, (.))
>import safe Control.Monad.Fix (fix)
>import safe Control.Applicative
>import safe Control.Category
>import safe Data.Type.Equality
>import safe Data.Complex
>import safe Data.Ratio
>import safe Data.Monoid
>import safe Math.Tools.Functor
>import safe Math.Tools.Visitor
>import safe Math.Tools.Median
>import safe Math.Tools.Orthogonal
>import safe Math.Tools.Adjunction
>import safe Math.Tools.PrettyP
>import safe Math.Tools.Arrow (BiArrow (..))
>import safe Math.Tools.Isomorphism ((:==:)(..))
>import safe Math.Matrix.Interface
>import safe Math.Matrix.Matrix
>import safe Math.Matrix.Linear
>import safe Math.Matrix.Vector2
>import safe Math.Number.StreamInterface
>import safe Math.Number.Interface
>import safe Math.Number.Stream hiding (logarithm)
>import safe qualified Math.Number.Stream as Stream
>import safe Math.Tools.Prop
>import safe GHC.TypeLits

>import safe Math.Matrix.Instances (characteristicPolynomial)

>-- | Problem with this representation: real number ranges cannot be represented.
>-- given two rational numbers \(r_1 < r_2\), it should be there are infinitely
>-- many real numbers between \(r_1\) and \(r_2\), so support density of irrationals.
>-- <https://en.wikipedia.org/wiki/Dense_order>
>-- 
>-- In constructive mathematics, given two real numbers \(r_1' < r_2'\), there
>-- should exist a rational number r, such that \(r_1' < r\) and \(r < r_2'\).
>-- HOWEVER, these comparisons are a type error for Eq class. In particular this
>-- cannot be used to _define_ how to compare two real numbers.
>-- 
>-- what is meant is if \(r_1' < r_2'\)
>--                  then there exists a rational r
>--                  such that \(r_1' < {\mathbf{fromRational}}(r)\) and \({\mathbf{fromRational}}(r) < r_2'\)
>-- 
>-- In here properties of \({\mathbf{fromRational}}\) are important. Note that comparisons
>-- invoke the real comparison, so at most is a recursive definition
>-- of real comparison in terms of itself. But it doesn't include
>-- termination condition.
>-- 
>-- It should be so that _if_ two real numbers \(r_1\) and \(r_2\) are not the same
>-- real number, then \(r_1 < r_2\) makes sense. But comparing whether two real
>-- numbers are the same is undecidable (algorithm for this comparison
>-- will not halt if they are same). Would this mean that if \(r_1 < r_2\) is
>-- computable, wouldn't \(\neg(r_1 < r_2 \lor r_2 < r_1)\) be computable equality of reals;
>-- This is computable in the limit , but not computable? <https://en.wikipedia.org/wiki/Computation_in_the_limit>
>--
>-- So we are forced to consider only comparison between a rational and a real,
>-- but not between two reals. This is encapsulated in
>-- the "Math.Tools.Interface.DedekindCut" type class.
>-- 
>-- The solution is that when comparing a rational with a real,
>-- the rational's denominator determines the accuracy used.
>-- This additional information makes comparison between a rational and a real
>-- decidable, but doesn't help when comparing two reals.

>newtype R = Limit { approximate :: (Stream Rational) }
>data Modulus = Modulus { modulus_value :: R, modulus :: Rational -> Integer }

>instance DedekindCut Rational R where
>  ~(Limit ~(Pre x xr)) <% q
>     | denominator x > denominator q = x < q
>     | otherwise = (Limit xr) <% q
>  q %< ~(Limit ~(Pre x xr))
>     | denominator x > denominator q = q < x
>     | otherwise = q %< (Limit xr)

>instance DedekindCut Int R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance DedekindCut Integer R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>instance DedekindCut Word R where
>  r %< x = (fromIntegral r :: Rational) %< x
>  x <% r = x <% (fromIntegral r :: Rational)

>-- | d x = partialDerivate x 

>d :: (Infinitesimal Stream a) => (a -> a) -> a -> Closure Stream a
>d f x = derivateClosed f x .>>=. \derx ->
>             epsilonClosure .>>=. \dx ->
>             close $! (derx * dx)

>convergenceRatioTest :: (Fractional a, Ord a, Closed a) => Stream a -> Bool
>convergenceRatioTest str = (accumulationPoint $ limit $ abs <$> streamQuotients str) < 1

>averageConvergenceRatio :: (MetricSpace a, Fractional (Scalar a)) => Stream a -> Stream (Scalar a)
>averageConvergenceRatio = fmap listAverage . cauchy

>listAverage :: (Fractional a) => [a] -> a
>listAverage lst = (1 / fromIntegral (length lst)) * Prelude.sum lst

maxConvergenceRatio :: (MetricSpace a) => Stream a -> Stream R
maxConvergenceRatio = fmap (foldl1 max) . cauchy

>cauchy :: (MetricSpace a) => Stream a -> Stream [Scalar a]
>cauchy s = codiagonals $ matrix distance s s

>cseqDifferenceMatrix :: (Functor m, Functor n, Diagonalizable m a, LinearTransform m n a, Num a)
> => m a -> n a -> (m :*: n) a
>cseqDifferenceMatrix s t = matrix (\x y -> abs (x - y)) s t

>-- | See Suppes: Axiomatic Set Theory.
>-- Note that we would like to check that
>-- \[\forall \epsilon > 0. \exists n. \forall k > n. |s_k - t_k| < \epsilon \].
>-- However, this is undecidable in particular with respect to what to choose for 'n'.
>-- We can nonetheless compute the stream of distances.
>cauchySequenceEquivalence :: (ConjugateSymmetric a, Closed a, Num a) => Stream a -> Stream a -> Stream a
>cauchySequenceEquivalence s t = diagonalImpl $ cseqDifferenceMatrix s t

>-- | cseqEquivalenceMatrix computes equivalence of cauchy sequences.
>-- However, the 'modulus' argument is undecidable. But if you produce a modulus,
>-- this chooses the part of the cauchy sequence differences that should converge
>-- according to the convergence ratio of the @epsilon@ from the infinitesimal instance.
>-- Thus if you can prove that the matrix elements converge to zero when elements
>-- of increasing index are chosen, you would have proven equality of the two cauchy
>-- sequences. This could be possible in special cases, even if it's undecidable
>-- in general. Note that it suffices to consider the convergence of the first column,
>-- because you could move any part to the first column by choosing reasonable values
>-- of the modulus.
>-- The other differences are included so computations and analysis of the situation
>-- can be easier to perform.
>cseqEquivalenceMatrix :: (Num a, ConjugateSymmetric a, Closed a, Infinitesimal Stream eps)
> => (eps -> Integer) -> Stream a -> Stream a -> (Stream :*: Stream) a
>cseqEquivalenceMatrix modulus x y = Matrix $ do
>   eps <- epsilonStream
>   let n = modulus eps
>   return $ Stream.drop n seq
> where seq = cauchySequenceEquivalence x y

>-- | This version of cauchy sequence equality convergence check
>-- produces a stream of lists, where each list contains those elements
>-- which are close to each other in terms of choice of indices, e.g.
>-- \[C(s,t)_i = [ \lvert s_j - t_k \rvert | j \leftarrow naturals, k \leftarrow naturals, i = j + k]\]
>-- thus the complexity of the stream element lists of the result should increase
>-- in the same way than the complexity of the input stream elements.
>-- The lists in the result stream have monotonically increasing length.
>-- thus to compute results for particular level of approximation, it should suffice
>-- to choose a list from the result stream, and perform the equivalence check
>-- using the values in the list. To do this, it would be necessary to be able
>-- to determine how close to zero the elements in the list should be.
>-- the middle element (if any) of the list is probably the most interesting,
>-- because that would mean \(j == k\), therefore the computation result
>-- would be \(\lvert s_j - t_j \rvert\), but due to different rate of convergence of the input
>-- sequences, it can happen that the elementwise differences are not really
>-- representative of the stream convergence behaviour at the diagonal.
>-- however, analyzing these numbers should produce useful results about
>-- convergence of the input stream element differences.
>-- e.g choosing minimum and maximum element from each list determines
>-- best and worst approximation in the same depth of approximations.
>-- for the equality of the cauchy sequences to hold, the best approximation should
>-- stay close to zero and better so when further elements of the stream are obtained.
>cseqConvergence :: (ConjugateSymmetric a, Closed a, Num a) => Stream a -> Stream a -> Stream [a]
>cseqConvergence s t = codiagonals $ cseqDifferenceMatrix s t

>cseqEquivalence_list :: (Closed a, ConjugateSymmetric a, Num a, Infinitesimal Stream eps) => (eps -> Integer) -> Stream a -> Stream a -> Stream [a]
>cseqEquivalence_list modulus x y = codiagonals $ cseqEquivalenceMatrix modulus x y

>converges :: (MetricSpace a) => Stream a -> a -> Stream [Scalar a]
>converges s p = codiagonals $ matrix distance s (constant p)
>
>instance NormedSpace R where
>  norm = abs

>instance InnerProductSpace R where
>   x %. y = x * y

>instance Infinitesimal Stream Rational where
>   epsilonStream = fmap (1 /) $ Stream.power 10

>instance MetricSpace R where
>  distance x y = abs (x-y)

>instance MetricSpace (Stream R) where
>  distance x y = accumulationPoint $ limit $ do 
>                    (xn,yn,n) <- fzip3 x y naturals
>                    let d = abs (xn-yn)
>                    return $! (1 / (2^n)) * d / (1 + d)

>increaseAccuracyBy :: (Limiting Stream a) => Integer -> Closure Stream a -> Closure Stream a
>increaseAccuracyBy i = limit . Stream.drop i . approximations


>partialDerivateClosure :: (Limiting str a, Fractional a)
>   => (a -> b -> b) -> (b -> a) -> b -> Endo (Closure str a)
>partialDerivateClosure delta f v = Endo $ \dx -> limit $ do
>   let fv = f v
>   eps <- approximations dx
>   return $! (f (delta eps v) - fv) / eps


>partialDerivateLinear :: (LinearTransform f f a, VectorSpace (f a), Closed a, Closed (f a),
> Fractional a, Scalar (f a) ~ a, Linearizable LinearMap (:*:) f f a, Diagonalizable f a)
> => (a -> f a :-> f a) -> (f a :-> f a) -> (f a :-> f a)
>partialDerivateLinear delta f = arrLinear $ \v -> accumulationPoint $ limit $ do
>   let fv = f -!< v
>   eps <- epsilonStream
>   return $! (1/eps) %* (f -!< (delta eps -!< v) %- fv)

>partialDerivateEndo :: (Limiting str a, Fractional a)
>  => (a -> Endo b) -> (b -> a) -> b -> Endo (Closure str a)
>partialDerivateEndo delta f v = Endo $ \dx -> limit $ do
>   let fv = f v
>   eps <- approximations dx
>   return $! (f (delta eps `appEndo` v) - fv) / eps

already defined in Math.Number.Stream:

instance (RealFloat a, Infinitesimal str a) => Infinitesimal str (Complex a) where
  epsilonClosure = limit $ liftA2 (:+) (epsilonStream) (epsilonStream)

>average :: R -> R -> R
>average (Limit x) (Limit y) = Limit $ liftA2 (\x y -> (x+y)/2) x y


would require Ord constraint
instance MedianAlgebra R where
   med (Limit x) (Limit y) (Limit z) = Limit $ med x y z

>instance ConjugateSymmetric R where { conj = id }
  
>instance VectorSpace R where
>  type Scalar R = R
>  vzero = 0
>  vnegate x = negate x
>  a %+ b = a + b
>  a %* b = a * b

>-- | The following instance declaration represents the completeness of the
>-- real number system. 
>instance Limiting Stream R where
>  data Closure Stream R = RClosure { runRClosure :: R }
>  limit str = RClosure $ limitReal str
>  approximations = fmap (Limit . return) . approximate . runRClosure

>instance Infinitary (Closure Stream R) where
>   infinite = RClosure infinite

>-- | An isomorphism \(Cl(R) \cong R\).
>completenessOfReals :: Closure Stream R :==: R
>completenessOfReals = runRClosure <-> RClosure

>instance Floating (Closure Stream R) where
>   pi = RClosure pi
>   exp = liftRClosure exp
>   log = liftRClosure log
>   sqrt = liftRClosure sqrt
>   sin = liftRClosure sin
>   cos = liftRClosure cos
>   tan = liftRClosure tan
>   asin = liftRClosure asin
>   acos = liftRClosure acos
>   atan = liftRClosure atan
>   sinh = liftRClosure sinh
>   cosh = liftRClosure cosh
>   tanh = liftRClosure tanh
>   asinh = liftRClosure asinh
>   acosh = liftRClosure acosh
>   atanh = liftRClosure atanh

>liftRClosure :: (R -> R) -> Closure Stream R -> Closure Stream R
>liftRClosure f (RClosure x) = RClosure $! (f x)

>instance Num (Closure Stream R) where
>   (RClosure x) + (RClosure y) = RClosure $ x + y
>   (RClosure x) - (RClosure y) = RClosure $ x - y
>   (RClosure x) * (RClosure y) = RClosure $ x * y
>   negate (RClosure x) = RClosure $ negate x
>   abs (RClosure x) = RClosure $ abs x
>   signum (RClosure x) = RClosure $ signum x
>   fromInteger i = RClosure $ fromInteger i

>limitReal :: Stream R -> R
>limitReal str = Limit $ str >>= approximate

>liftReal :: (Rational -> Rational) -> R -> R
>liftReal f = Limit . fmap f . approximate

>instance Show (Closure Stream R) where
>   show (RClosure (Limit str)) = show (take 10 $ fmap fromRational str)

>instance VectorSpace (Closure Stream R) where
>  type Scalar (Closure Stream R) = R
>  vzero = RClosure vzero
>  vnegate (RClosure x) = RClosure (vnegate x)
>  (RClosure x) %+ (RClosure y) = RClosure $ x %+ y
>  a %* (RClosure x) = RClosure (a %* x)


>instance MetricSpace (Closure Stream R) where
>   distance (RClosure a) (RClosure b) = distance a b

>instance Closed R where
>   accumulationPoint = runRClosure

>series :: (Integer -> R) -> R
>series f = ssum (fromIntegerSeq f)

upperbound :: [R] -> Prop R
upperbound set = Characteristic $ \x -> and $ map (x >=) set

lowerbound :: [R] -> Prop R
lowerbound set = Characteristic $ \x -> and $ map (x <=) set

>-- | <http://en.wikipedia.org/wiki/Least-upper-bound_property Least upper bound property>
>-- | note this is really undecidable.

supremum :: [R] -> R
supremum [x] = x
supremum (Limit ~(Pre c cr):lst) = Limit $ Pre c $ rest
   where rest = liftA2 max cr $ approximate $ supremum lst
supremum [] = negative_infinity

>supremumGen :: (Fractional a, Ord a, Limiting Stream a)
>             => [Closure Stream a] -> Closure Stream a
>supremumGen [] = negative_infinityGen
>supremumGen [x] = x
>supremumGen (a:lst) = limit $ Pre c $ rest
>   where rest = liftA2 max cr $ approximations $ supremumGen lst
>         ~(Pre c cr) = approximations a

>negate_limit :: (Limiting str a, Num a) => Closure str a -> Closure str a
>negate_limit = cmap negate

infimum :: [Closure R] -> Closure R
infimum = negate_limit . supremumGen . map negate_limit

>infimumGen :: (Fractional a, Ord a, Limiting Stream a) => [Closure Stream a] -> Closure Stream a
>infimumGen = negate_limit . supremumGen . map negate_limit

>instance Infinitary R where
>   infinite = infinity

>infinity :: R
>infinity = Limit $ Stream.power 10

>instance Bounded R where
>   minBound = negate infinity
>   maxBound = infinity

>infinityGen :: (Fractional a, Limiting Stream a) => Closure Stream a
>infinityGen = limit $ Stream.power 10

>negative_infinityGen :: (Fractional a, Limiting Stream a) => Closure Stream a
>negative_infinityGen = limit $ fmap negate $ Stream.power 10

>-- | epsilon is a real that converges to zero.
>instance Infinitesimal Stream R where
>  epsilonClosure = close $ 1 / infinity

>-- | <http://en.wikipedia.org/wiki/Squeeze_theorem Squeeze theorem>

>limitBelow :: R -> (R -> R) -> Closure Stream R
>limitBelow c f = limit $ approximations (close $ c - accumulationPoint epsilonClosure) >>= (return . f)

>-- | <http://en.wikipedia.org/wiki/Squeeze_theorem Squeeze_theorem>

>limitAbove :: R -> (R -> R) -> Closure Stream R
>limitAbove c f = limit $ approximations (close $ c + accumulationPoint epsilonClosure) >>= (return . f)

>-- | <http://en.wikipedia.org/wiki/Squeeze_theorem Squeeze_theorem>
>limitBoth :: R -> (R -> R) -> Closure Stream R
>limitBoth c f = limit $ approximations (limitBelow c f)
>            `interleave` approximations (limitAbove c f)


>negative_infinity :: R
>negative_infinity = negate infinity

>epsilonLinear :: R
>epsilonLinear = Limit $ (1 %) <$> nonzeroNaturals

>increaseAccuracy :: R -> R
>increaseAccuracy (Limit ~(Pre _ xr)) = Limit xr

>lessthanPrecision :: R -> R -> Integer -> Bool
>lessthanPrecision _ _ 0 = False 
>lessthanPrecision (Limit ~(Pre x xs)) (Limit ~(Pre y ys)) i
>           | x < y = True
>           | x > y = False
>           | otherwise = lessthanPrecision (Limit xs) (Limit ys) (pred i)

>limitingDistance :: (Num a, Limiting str a) => Closure str a -> Closure str a -> Closure str a
>limitingDistance x y = limit $ do 
>                      (x',y') <- approximations x <&> approximations y
>                      return $! abs (x' - y')

>distanceMatrix :: R -> R -> (Stream :*: Stream) Rational
>distanceMatrix (Limit x) (Limit y) = matrix (\a b -> abs $ a - b) x y

>-- | compute rational approximation more precise than the given rational
>-- the precision is expressed as rational number close to zero,
>-- the result will be within range \([r-p,r+p]\), in the sense
>-- that in the sequence of rational approximations \(r = (r_i | i \in {\mathbf N})\)
>-- we have \(\lvert r_{i+1} - r_i \rvert \leq p\). Note that we do _not_ attempt to prove
>-- that all successive approximations have this same property,
>-- we need the differences to be monotonically decreasing for this
>-- to represent correct precision.
>atPrecisionRational :: R -> Rational -> Rational
>atPrecisionRational (Limit s) p = checkPrecision s
> where checkPrecision ~z@(Pre x ~z'@(Pre y _))
>         | abs (y - x) <= p = x
>         | otherwise = checkPrecision z'

>-- | here precision is expresses as integer power of 10, e.g.
>--   \(1 == 10^{-1}\), \(2 == 10^{-2}\), \(3 == 10^{-3}\) and so on.
>--   so how many digits after the decimal point are required.
>atPrecision :: R -> Integer -> R
>atPrecision (Limit s) p = Limit $ checkPrecision s
>  where lm = 10^^(negate p)
>        checkPrecision   ~z@(Pre x ~z'@(Pre y _))
>             | abs (y - x) <= lm = z
>             | otherwise = checkPrecision z'

>-- | rational precision expressed as integer power of 10
>precisionRational :: R -> Integer -> Rational
>precisionRational (Limit s) p = checkPrecision s
>   where checkPrecision ~z@(Pre x ~z'@(Pre y yr))
>           | abs (y - x) <= 10^^(negate p) = x
>           | otherwise = checkPrecision z'

>realDerivate :: (R -> R) -> R -> R
>realDerivate f x = accumulationPoint $ derivateClosed f x

>instance DifferentiallyClosed R where
>  derivate = realDerivate
>  integral = integralReal

>instance DifferentiallyClosed (Closure Stream R) where
>  derivate f (RClosure x) = RClosure $ derivate (runRClosure . f . RClosure) x
>  integral (a,b) f = RClosure $ integral (runRClosure a, runRClosure b) (runRClosure . f . RClosure)

>realExp :: R -> R
>realExp = fix realDerivate

>-- | i'th element of derivatesAt(f,s) is \(D^i[f](s_i)\)
>derivatesAt :: (DifferentiallyClosed a) => (a -> a) -> Stream a -> Stream a
>derivatesAt f x = derivates f <*> x

>-- | <http://en.wikipedia.org/wiki/Arithmetic%E2%80%93geometric_mean Arithmetic-geometric mean>

>agm :: (Floating a, Limiting str a) => a -> a -> Closure str a
>agm x y = limit $ fmap fst $ iterateStream (\(x,y) -> ((x+y)/2, sqrt(x*y))) (x,y)

>-- | <http://en.wikipedia.org/wiki/Exponential_function Exponential function>
>-- using limit definition \(\exp(x) = \lim_{n\rightarrow\infty}(1+{{x}\over{n}})^n\).

>expo :: R -> R
>expo x = Limit $ do
>           (xr,n) <- approximate x <&> powerIntegral 10
>           return $! (1 + (xr/fromIntegral n))^^n

>instance Numerics R where
>   newtonsMethod f x = accumulationPoint $ newtonsMethodRealClosure f x

>newtonsMethodRealClosure :: (Closed a) => (a -> a) -> a -> Closure Stream a
>newtonsMethodRealClosure f x = limit $ iterateStream iteration x
>   where iteration z' = z' - f z' / accumulationPoint (derivateClosed f z')

>eigenvalue :: (Closed a,
>  LinearTraceable LinearMap m a,
>  Linearizable LinearMap (:*:) m m a, 
>               Applicative m, a ~ Scalar (m a),
>               VectorSpace (m a), LinearTransform m m a
>               )
>           => m a :-> m a -> Closure Stream a
>eigenvalue m = newtonsMethodRealClosure (characteristicPolynomial m) 0

>integrate_vector :: (Enum (Scalar a), VectorSpace a, Limiting str a,
>                     Limiting str (Scalar a))
>   => (Scalar a -> a) -> Closure str (Scalar a) -> (Scalar a,Scalar a) -> Closure str a
>integrate_vector f dx ~(xa,ya) = limit $ do
>   eps <- approximations dx
>   return $ vsum $ map ((eps %*) . f) [xa,xa + eps..ya]

>-- | <https://en.wikipedia.org/wiki/Methods_of_contour_integration>
>integrate :: (Enum a, Num a, Limiting str a)
>          => (a -> a) -> Closure str a -> (a,a) -> Closure str a
>integrate f dx ~(xa,ya) = limit $ do
>   eps <- approximations dx
>   return $! (eps *) $! Prelude.sum $! map f [xa,xa+eps..ya]

>integralReal :: (R,R) -> (R -> R) -> R
>integralReal ~(x,y) f = runRClosure $ limit $ do
>   (eps, xa, ya) <- fzip3 (approximate $ runRClosure epsilonClosure)
>                          (approximate x)
>                          (approximate y)
>   return $! (fromRational eps %*) $! vsum $!
>      map (f . fromRational) [xa,xa+eps..ya]

>integralRational :: (R,R) -> (Rational -> Rational) -> R
>integralRational ~(x,y) f = Limit $ do
>       (eps,xa,ya) <- fzip3 (approximate $ runRClosure epsilonClosure)
>                            (approximate x)
>                            (approximate y)
>       return $! (eps *) $! Prelude.sum $! map f [xa,xa+eps..ya]


>-- | for integrateCurve (xa,ya) curve f dx, Requires: \(\lim_{x \rightarrow 0}curve(x) = 0 \land \lim_{i\rightarrow \infty} dx_i = 0\)
>integrateCurve :: (Enum a, Num a, Num r, Limiting str a, Limiting str r)
>                => (a,a) -> (a -> r) -> (r -> r) -> Closure str a -> Closure str r
>integrateCurve (xa,ya) curve f dx  = limit $ do
>   eps <- approximations dx
>   return $! (curve eps *) $! Prelude.sum $! map (f . curve) [xa,xa+eps..ya]

>-- | <http://en.wikipedia.org/wiki/Integral Integral>
>-- This doesn't converge well.

>gamma_via_integral :: R -> Closure Stream R  
>gamma_via_integral zz = integral (close 0,infinityGen) $ \t -> exp (negate t) * (t ** RClosure (zz-1))
    
>instance (TypeError (Text "Equality of constructive reals is undecidable." :$$:
>                     Text "Please use (<%) and (%<) from Math.Number.Interface.DedekindCut class")) => Eq R where
>  xs == ys = error "equality of constructive reals is undecidable"

>approximatelyEqual :: R -> R -> Bool
>approximatelyEqual x y = showAtPrecision x 30 == showAtPrecision y 30

>approximatelyEq :: R -> R -> Bool
>approximatelyEq x y = a == b
>   where a = showAtPrecision x 7
>         b = showAtPrecision y 7


>constructively_less :: Rational -> R -> R -> Closure Stream Bool
>constructively_less between (Limit ~(Pre a ar)) (Limit ~(Pre b br)) = limit $
>   (a < between || between < b)
>   `Pre` (approximations $ constructively_less between (Limit ar) (Limit br))


>equality :: Closure Stream R -> Closure Stream R -> Closure Stream Bool
>equality str str'
>   = limit $ liftA3 (\a b eps -> abs (b - a) < eps) astr astr' aeps
>  where astr = fmap approximate $ approximations str
>        astr' = fmap approximate $ approximations str'
>        aeps  = fmap approximate $ epsilonStream


instance Show (Complex R) where
  showsPrec x (a :+ b) = showAtPrecisionReal a p
    . showString ":+" . showAtPrecisionReal b p
   where p = fromIntegral x


>signumReal :: R -> Int -> Int
>signumReal _ 0 = 0
>signumReal r i
>   | ~(a,b) <- properFraction (shead $ approximate r) =
>       if signum a == 0
>        then signumReal (r*10) (i-1)
>         else signum a

>showAtPrecisionReal :: R -> Integer -> String
>showAtPrecisionReal r i = (signcorr r . shows a . showString "." . decs) []
>   where signcorr r = if signum a == 0 && signumReal b 30 == -1 then ('-':) else id
>         decs = if a == 0 then decimalsStr2 (abs b) i
>                          else decimals (abs (b `atPrecision` i)) i
>         (a,b) = properFractionReal r
>         decimals x 0 = id
>         decimals x i
>             | ~(a',b') <- properFractionReal (x*10), r <- decimals b' (pred i) = shows a' . r
>         decimalsStr2 x i = case decimals2 x i of
>           (0,res) -> decimals (res `atPrecision` i) i 
>           (j,res) -> decimals (res `atPrecision` i) i 
>                        . showString "e" . shows j
>         decimals2 x 0 = (0,x)
>         decimals2 x i = let ~(a',b') = properFractionReal (x*10) 
>                           in case a' of
>                             0 -> let (res,res2) = decimals2 b' (pred i) in (pred res,res2)
>                             _ -> (0,x)

>instance Show R where
>    show s = showAtPrecision s 20

>instance ShowPrecision R where
>    showAtPrecision = showAtPrecisionReal

>instance PpShow R where
>    pp s = pp (showAtPrecision s 10)


instance Real R where -- needs Ord instance
    toRational = shead . approximate . (`atPrecision` 5)

instance RealFrac R where
    -- ^ not really computable for reals, since 'floor' is undecidable
    -- according to <http://www.win.tue.nl/~hzantema/hg2.pdf Geuvers: Stream representations of real numbers>
    -- we implement this using approximations nonetheless because
    -- this is used for printing the real number.
    -- Note that two distinct representations of the _same_ real number
    -- may produce different result, e.g.
    -- 0.9999999.. == 1.0000000.. , but:
    -- fst (properFraction 0.9999...) == 0 != 1 == fst (properFraction 1.00..)
    -- however, since comparing constructive reals is noncomputable,
    -- we couldn't possibly declare that 0.9999.. == 1.000000..
    floor = fst . properFraction
    round = floor . (+ 0.5)

>properFractionReal :: (Num a) => R -> (a, R)
>properFractionReal r = (fromIntegral wholepart,liftReal (snd . properFraction) r)
>      where rat = shead $ approximate $ r `atPrecision` 2
>            wholepart = sign * ((abs $ numerator rat)
>                                `div` (abs $ denominator rat))
>            sign = signum (numerator rat) * signum (denominator rat)

>-- | Enum instance for reals is undecidable with respect to end of
>-- real intervals, since comparison of two real numbers is undecidable.
>-- Thus enumFromTo and enumFromThenTo produce errors indicating
>-- density problem.
>instance Enum R where
>    succ = liftReal succ
>    pred = liftReal pred
>    toEnum i = Limit $ constant (toEnum i)
>    fromEnum (Limit xr) = fromEnum (shead $ Stream.drop 8 xr)
>    enumFrom x  = x : map succ (enumFrom x)
>    enumFromThen x x2 = x : enumFromThen x2 (2*x2 - x)
>    enumFromTo _ _ = error "density problem: cannot compute end of real interval"
>    enumFromThenTo x x2 y = error "density problem: cannot compute end of real interval"

    enumFromTo x y | x <= y = x : enumFromTo (succ x) y
                   | otherwise = []
    enumFromThenTo x x2 y | x <= y = x : enumFromThenTo x2 (2*x2-x) y
                          | otherwise = []

>-- | This attempts to work around the density problem by computing range
>-- | in some type which does have enum instance,
>-- | then using the given function to map to reals.
>enumFromThenToReal :: (Enum e) => (e -> R) -> e -> e -> e -> [R]
>enumFromThenToReal f a s b = [f x | x <- [a,s..b]]

>fromThenTo :: Rational -> Rational -> Rational -> [R]
>fromThenTo = enumFromThenToReal fromRational

>fromTo :: Rational -> Rational -> [R]
>fromTo = enumFromToReal fromRational

>-- | This attempts to work around the density problem by computing range
>-- | in some type which does have enum instance,
>-- | then using the given function to map to reals.
>enumFromToReal :: (Enum e) => (e -> R) -> e -> e -> [R]
>enumFromToReal f a b = [f x | x <- [a..b]]

>realEnumFromThenTo :: R -> R -> R -> Stream [Rational]
>realEnumFromThenTo (Limit str) (Limit str2) (Limit str3) = strEnumFromThenTo str str2 str3
> 
>strEnumFromThenTo :: Stream Rational -> Stream Rational -> Stream Rational -> Stream [Rational]
>strEnumFromThenTo ~(Pre x xr) ~(Pre x2 x2r) ~(Pre y yr) = Pre (enumFromThenTo x x2 y) (strEnumFromThenTo xr x2r yr)

>instance Num R where
>    (Limit s) + (Limit t) = Limit $ liftA2 (+) s t
>    (Limit s) - (Limit t) = Limit $ liftA2 (-) s t
>    (Limit s) * (Limit t) = Limit $ liftA2 (*) s t
>    negate = liftReal negate
>    abs = liftReal abs
>    signum = liftReal signum
>    fromInteger i = Limit $ constant $ fromInteger $! i

>instance Fractional R where
>    (Limit s) / (Limit t) = Limit $ liftA2 (/) s t
>    recip = liftReal recip
>    fromRational r = Limit $ constant $! r

>toReal :: Rational -> R
>toReal = fromRational

>instance Fractional (Closure Stream R) where
>   (RClosure x) / (RClosure y) = RClosure $ x / y
>   recip (RClosure x) = RClosure $ recip x
>   fromRational r = RClosure $ fromRational r

>-- | <https://en.wikipedia.org/wiki/Natural_logarithm Natural logarithm>
>-- @ log(1+z) = z - z^2/2 + z^3/3 - ... @

>logarithm :: R -> R
>logarithm a = integral (1,a) (\x -> 1 / x)

>eulerConstant :: R
>eulerConstant = exp 1

>napiersConstant :: R
>napiersConstant = exp 1.0

>logarithmByNewton :: R -> Closure Stream R
>logarithmByNewton e = newtonsMethodRealClosure (\x -> exp x - e) 1

>invertByNewton :: (Closed a) => (a -> a) -> a -> Closure Stream a
>invertByNewton f e = newtonsMethodRealClosure (\x -> f x - e) 1

>-- computes x from equation @(Df)(x) = y@ by newton's method
>differentialEquation1 :: (Closed a) => (a -> a) -> a -> Closure Stream a
>differentialEquation1 f y = newtonsMethodRealClosure (\x -> accumulationPoint (derivateClosed f x) - y) 1


>expByPowers :: (Fractional r, Limiting Stream r) => r -> Closure Stream r
>expByPowers x = limit $ do
>     n <- naturals
>     let n2 = 10^n
>     return $! (1 + x/fromIntegral n2)^^n2

>expByPowersReal :: R -> R
>expByPowersReal (Limit x) = Limit $ liftA2 (\n x' -> (1 + (x'/fromIntegral n))^^n) (stail $ stail $ powerIntegral 10) x

>sumGeneratingFunction :: Stream Rational -> R -> R
>sumGeneratingFunction f (Limit x) = Limit $ uninterleaveIndex 1
>                                            $ approximateSums f x

>-- | <https://en.wikipedia.org/wiki/Riemann_zeta_function>
>riemann_zeta :: (Floating a) => a -> Stream a
>riemann_zeta s = sumStream $ fmap term nonzeroNaturals
>   where term n = 1 / (n ** s)

>-- | <https://en.wikipedia.org/wiki/Riemann_zeta_function>
>riemann_zetaReal :: (Integral a) => a -> R
>riemann_zetaReal s = Limit $ sumStream $ fmap term nonzeroNaturals
>   where term n = 1 / (n ^ s)

>-- | <https://en.wikipedia.org/wiki/Riemann_zeta_function>
>riemann_zetaComplex :: (RealFloat a) => Complex a -> Stream (Complex a)
>riemann_zetaComplex s = sumStream $ fmap term nonzeroNaturals
>   where term n = 1 / (n ** s)

instance (Fractional a, Limiting a, Closed a) => Infinitesimal Stream (Stream a) where
  epsilonStream = cellsLinear streamEpsilon

instance RealFloat R where
   floatRadix  x = 10
   -- WARNING: floatDigits would want to use real comparison.
   floatDigits x = if shead (approximate x) < 0 then 30 else negate 30
   floatRange  x = (-30000,30000)
   decodeFloat x = decodeInBase 10 30 x  
   encodeFloat m n = (fromInteger m :: R) * 10^^n
   isNegativeZero _ = False
   isInfinite _ = False
   isNaN x = False
   isDenormalized _ = False
   isIEEE _ = False
   atan2 y x = 2 * atan ((sqrt (x*x+y*y) - x) / y)

decodeInBase :: Integer -> Int -> R -> (Integer,Int)
decodeInBase base p x = iter start
     where start = floor ((x*fromIntegral base^p) `atPrecision` 1)
           iter x | abs x >= base^(p-1) && abs x < base^p = (x,negate p)
                  | x >= base^p,
                       (a,b) <- iter (x `div` base) = (a,b+1)
                  | x <= negate (base^p),
                      (a,b) <- iter (x `div` base) = (a,b+1)
                  | x < base^(p-1)  && x > 0,
                      (a,b) <- iter (x*base) = (a,b-1)
                  | x > negate (base^(p-1)) && x < 0
                       , (a,b) <- iter (x*base) = (a,b-1)
                  | x == 0 = (0,0)
                  | otherwise = error $ "iter base for:" ++ show x

>encodeInBase :: Integer -> Integer -> Int -> R
>encodeInBase base m n = (fromInteger m :: R) * (fromIntegral base)^^n

>fromFloat :: (RealFloat a) => a -> R
>fromFloat x = let (a,b) = decodeFloat x in encodeInBase (floatRadix x) a b

>-- | https://en.wikipedia.org/wiki/Golden_ratio

>goldenRatio :: R
>goldenRatio = Limit $ Stream.drop 4 
>                     $ streamQuotients (fmap fromIntegral fib)

>slowGoldenRatio :: R
>slowGoldenRatio = (1 + sqrt 5) / 2

Doesn't work since triggers problems with equality comparison:

primitiveRoot_of_unity :: Integer -> Closure (Complex R)
primitiveRoot_of_unity i = newtonsMethod (\x -> x ^^ i - (1 :+ 0)) (0 :+ 1)

>-- | https://en.wikipedia.org/wiki/Logistic_function

>logistic :: (Floating a) => a -> a
>logistic x = exp x / (exp x + 1)

>-- | this converts a real to a sequence of doubles, each item
>-- attempting to approximate the real to higher accuracy.
>floatingApproximationsReal :: R -> Stream Double
>floatingApproximationsReal = fmap fromRational . approximate

>instance Approximations Stream R where
>   floatingApproximations = floatingApproximationsReal
>   rationalApproximations = approximate


>logarithmByPower :: (Floating a, Limiting Stream a) => a -> Closure Stream a
>logarithmByPower x = limit $ do
>                n <- naturals
>                return $! n * (x ** (1/n) - 1)


tetrate2 x == x^x

>tetrate2_ :: (Floating a) => a -> a
>tetrate2_ x = x ** x

>-- | <https://en.wikipedia.org/wiki/Tetration Tetration>

>tetrate :: (Eq b, Num b, Floating a) => b -> a -> a
>tetrate 1 x = x
>tetrate i x = x ** ((i - 1) `tetrate` x)

ssrt is Inverse function for tetration. Don't have implementation.

>tetrate2 :: (Floating a) => a -> a
>tetrate2 x = sinh xln + cosh xln
>   where xln = x*log(x)

>logNewton :: (Floating a, Closed a) => a -> a
>logNewton a = accumulationPoint $ newtonsMethodRealClosure (\x -> exp x - a) 1

>cosGeneratingFunction_ = Pre 1 $ Pre 0 $ liftA2 (*) signs fact
> where signs = stail $ stail $ alternatingPossiblyNegativeBits
>       fact  = fmap (1/) (stail $ stail $ factorial)

>expBySeries2 :: R -> R
>expBySeries2 (Limit x) = Limit $ do
>   xappr <- x
>   sumStream $ liftA2 (/) (indexPowers (constant xappr)) factorial

expBySeries2 :: R -> R
expBySeries2 (Limit x) = Limit $ sumStream $
            matrix (*) (fmap (1/) factorial) (indexPowers x)

>expBySeries :: R -> R
>expBySeries (Limit x) = Limit $ (Pre 1 $ fmap (1 /) factorial) `approximateSums` x

>sinBySeries :: R -> R
>sinBySeries (Limit x) = (Limit $ x >>= \xappr ->
>        Stream.filter (\a -> a >= -1 && a <= 1) $ fst $ uninterleave $ sumStream 
>        $ liftA2 (*) filterStream
>        $ liftA2 (/) (indexPowers $ constant xappr)
>        $ factorial) `atPrecision` 2
>  where filterStream = Pre 0 $ Pre 1 $ Pre 0 $ Pre (negate 1) $ filterStream

uninterleave stuff is needed to ensure atPrecision works correctly,
since the sum of the generating function has zero at every second element,
it means there are always two adjacent equal elements.

>cosBySeries :: R -> R
>cosBySeries (Limit x) = (Limit $ x >>= \xappr ->
>        Stream.filter (\a -> a >= -1 && a <= 1) $
>        fst $ uninterleave $ sumStream
>        $ liftA2 (*) filterStream
>        $ liftA2 (/) (indexPowers (constant xappr)) factorial) `atPrecision` 2
>  where filterStream = Pre 1 $ Pre 0 $ Pre (negate 1) $ Pre 0 $ filterStream

>-- | Using Simon Plouffe's BPP digit extraction algorithm for computing pi.
>-- See <https://secure.wikimedia.org/wikipedia/en/wiki/Pi> for details.
>-- exp:  <http://en.wikipedia.org/wiki/Exponential_function Exponential function>
>-- log:  <http://en.wikipedia.org/wiki/Logarithm Logarithm>
>-- <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions inverse trigonometric functions>
>-- <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance Floating R where
>    pi = Limit $ Stream.sumStream $ do 
>            k <- Stream.naturals
>            let kr = k % 1                
>            return $! (16^^(negate k))*(4/(8*kr+1) - 2/(8*kr+4)
>                                       - 1/(8*kr+5) - 1/(8*kr+6))
>    exp (Limit x) = Limit $ Stream.drop 3 $ x >>= \xappr -> sumStream $
>         liftA2 (/) (indexPowers (constant xappr)) factorial
>    sqrt v = newtonsMethod (\x -> x*x - v) 1
>    log a = 2* (Limit $ Stream.sumStream $ do
>                  (n,aaprx) <- naturals <&> approximate a'
>                  return $! (1 / (2*fromIntegral n+1))*aaprx^(2*n+1))
>         where a' = (a-1)/(a+1)
>    x ** y = exp (y * log x)
>    sin = sinBySeries
>    cos = cosBySeries
>    tan x = sin x / cos x
>    asin x = integralReal (0,x) $ \a -> 1 / (sqrt $! 1 - a*a)
>    acos x = integralReal (x,1) $ \a -> 1 / (sqrt $! 1 - a*a)
>    atan x = integral (0,x) $ \a -> 1 / (a*a+1)
>    sinh x = (exp x - exp (negate x)) / 2
>    cosh x = (exp x + exp (negate x)) / 2
>    tanh x = sinh x / cosh x
>    asinh x = log $! x + sqrt (x*x+1)
>    acosh x = log $! x + sqrt (x*x-1)
>    atanh x = log ((1 + x)/(1-x)) / 2

mandelbrotPolynomial :: Complex R -> Complex R -> Complex R
mandelbrotPolynomial c z = z*z + c

mandelbrotStream :: Complex R -> Stream (Complex R)
mandelbrotStream c = iterateStream (mandelbrotPolynomial c) 0

mandelbrot :: Complex R -> Closure (Complex R)
mandelbrot c = limit (mandelbrotStream c)

>instance MetricSpace (Ratio Integer) where
>   distance x y = abs (x - y)
>instance MetricSpace Integer where { distance x y = abs (x - y) }
>instance MetricSpace Int where { distance x y = abs (x - y) }

>approximateSumsModulus :: Modulus -> Modulus -> Modulus
>approximateSumsModulus ((Limit x) `Modulus` f) ((Limit y) `Modulus` g)
>       = (Limit $ approximateSums x y) `Modulus` \r -> max (f r) (g r)

>instance Show Modulus where
>   show (Modulus x m) = show x ++ "[ used " ++ show (m (10^^(negate 30))) ++ " terms ]"

>instance Num Modulus where
>  (v `Modulus` f) + (w `Modulus` g) = (v + w) `Modulus` \r -> max (f r) (g r)
>  (v `Modulus` f) - (w `Modulus` g) = (v - w) `Modulus` \r -> max (f r) (g r)
>  (v `Modulus` f) * (w `Modulus` g) = (v * w) `Modulus` \r -> max (f r) (g r)
>  negate (v `Modulus` f) = (negate v) `Modulus` f
>  abs (v `Modulus` f) = (abs v) `Modulus` f
>  signum (v `Modulus` f) = (signum v) `Modulus` f
>  fromInteger i = (fromInteger i) `Modulus` (const 0)

>instance Fractional Modulus where
>  (v `Modulus` f) / (w `Modulus` g) = (v / w) `Modulus` \r -> max (f r) (g r)
>  recip (v `Modulus` f) = recip v `Modulus` \r -> f (recip r)
>  fromRational r = fromRational r `Modulus` const 0

>withInverseModulus :: (Fractional r, Real u) => (R -> R) -> (r -> u) -> Modulus -> Modulus
>withInverseModulus f finv (Modulus x m) = f x `Modulus` (\eps -> m (toRational $ finv $ fromRational eps))

>instance Floating Modulus where
>  pi = pi `Modulus` \r -> ceiling $ negate $ log (fromRational r) / log 16
>  exp = exp `withInverseModulus` log
>  sqrt = sqrt `withInverseModulus` (\x -> x*x)
>  sin = sin `withInverseModulus` asin
>  cos = cos `withInverseModulus` acos
>  log = log `withInverseModulus` exp
>  asin = asin `withInverseModulus` sin
>  acos = acos `withInverseModulus` cos
>  atan = atan `withInverseModulus` tan
>  tan x = sin x / cos x
>  x ** y = exp (y * log x)
>  sinh x = (exp x - exp (negate x)) / 2
>  cosh x = (exp x + exp (negate x)) / 2
>  tanh x = sinh x / cosh x
>  asinh x = log $ x + sqrt (x*x+1)
>  acosh x = log $ x + sqrt (x*x-1)
>  atanh x = log ((1 + x)/(1-x)) / 2

>-- | this computes partial derivates of the scalar-value 2D vector field
>-- along both variables simultaneously.
>del_vector2 :: (Infinitesimal str a) => (Vector2 a -> a) -> Vector2 a -> Vector2 (Closure str a)
>del_vector2 f (Vector2 x y) = Vector2 (partialDerivate1_2 ff x y)
>                                      (partialDerivate2_2 ff x y)
>  where ff a b = f (Vector2 a b)
