>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, TypeOperators, TypeFamilies, PatternGuards, UnicodeSyntax #-}
>{-# LANGUAGE DataKinds, UndecidableInstances #-}
>module Math.Number.Real
> (
> R(Limit), Modulus(..), d, liftReal, convergence_ratio_test,
> average_convergence_ratio, list_average, cauchy, cseq_difference_matrix,
> cauchy_sequence_equivalence, cseq_equivalence_matrix, cseq_convergence,
> cseq_equivalence_list, converges, increase_accuracy_by,
> partial_derive, partial_derivate_closure, partial_derivate,
> partial_derivate_endo, complex_derivate, average, completenessOfReals,
> liftRClosure, limit_real, lift_real, series, supremum_gen, negate_limit,
> infimum_gen, infinity, infinity_gen, negative_infinity_gen,
> limit_below, limit_above, limit_both, negative_infinity, epsilon_linear,
> increase_accuracy, lessthan_precision, limiting_distance, distance_matrix,
> at_precision_rational, at_precision, precision_rational,
> real_derivate, real_exp, derivate_closed, vector_derivate, pseudo_derivate,
> derivates_at, derivate_around, partial_derivate1_2, partial_derivate2_2,
> partial_derivate1_3, partial_derivate2_3, partial_derivate3_3, agm,
> expo, newtons_method, eigenvalue, integrate_vector, integrate,
> integral_real, integral_rational, integrate_curve, gamma_via_integral,
> approximately_equal, approximately_eq, constructively_less, equality,
> signum_real, show_at_precision_real, properFraction_real, enumFromThenToReal,
> fromThenTo, fromTo, enumFromToReal, realEnumFromThenTo, strEnumFromThenTo,
> toReal, logarithm, euler_constant, napiers_constant, logarithm_by_newton,
> invert_by_newton, exp_by_powers, exp_by_powers_real,
> sum_generating_function, riemann_zeta, riemann_zeta_real, riemann_zeta_complex,
> encodeInBase, fromFloat, golden_ratio, slow_golden_ratio, logistic,
> floating_approximations, logarithm_by_power, tetrate, tetrate2,
> log_newton, exp_by_series2, exp_by_series, sin_by_series, cos_by_series,
> approximate_sums_modulus, withInverseModulus, partial_derivate_linear
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

>-- | d x = partial_derivate x 

>d :: (Infinitesimal Stream a) => (a -> a) -> a -> Closure Stream a
>d f x = derivate_closed f x .>>=. \derx ->
>             epsilon_closure .>>=. \dx ->
>             close $! (derx * dx)

>liftReal :: (Rational -> Rational) -> R -> R
>liftReal f (Limit str) = Limit $ fmap f str

>convergence_ratio_test :: (Fractional a, Ord a, Closed a) => Stream a -> Bool
>convergence_ratio_test str = (accumulation_point $ limit $ abs <$> stream_quotients str) < 1

>average_convergence_ratio :: (MetricSpace a, Fractional (Scalar a)) => Stream a -> Stream (Scalar a)
>average_convergence_ratio = fmap list_average . cauchy

>list_average :: (Fractional a) => [a] -> a
>list_average lst = (1 / fromIntegral (length lst)) * Prelude.sum lst

max_convergence_ratio :: (MetricSpace a) => Stream a -> Stream R
max_convergence_ratio = fmap (foldl1 max) . cauchy

>cauchy :: (MetricSpace a) => Stream a -> Stream [Scalar a]
>cauchy s = codiagonals $ matrix distance s s

>cseq_difference_matrix :: (Functor m, Functor n, Diagonalizable m a, LinearTransform m n a, Num a)
> => m a -> n a -> (m :*: n) a
>cseq_difference_matrix s t = matrix (\x y -> abs (x - y)) s t

>-- | See Suppes: Axiomatic Set Theory.
>-- Note that we would like to check that
>-- \[\forall \epsilon > 0. \exists n. \forall k > n. |s_k - t_k| < \epsilon \].
>-- However, this is undecidable in particular with respect to what to choose for 'n'.
>-- We can nonetheless compute the stream of distances.
>cauchy_sequence_equivalence :: (Closed a, Num a) => Stream a -> Stream a -> Stream a
>cauchy_sequence_equivalence s t = diagonal_impl $ cseq_difference_matrix s t

>-- | cseq_equivalence_matrix computes equivalence of cauchy sequences.
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
>cseq_equivalence_matrix :: (Num a, Closed a, Infinitesimal Stream eps)
> => (eps -> Integer) -> Stream a -> Stream a -> (Stream :*: Stream) a
>cseq_equivalence_matrix modulus x y = Matrix $ do
>   eps <- epsilon_stream
>   let n = modulus eps
>   return $ Stream.drop n seq
> where seq = cauchy_sequence_equivalence x y

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
>cseq_convergence :: (Closed a, Num a) => Stream a -> Stream a -> Stream [a]
>cseq_convergence s t = codiagonals $ cseq_difference_matrix s t

>cseq_equivalence_list :: (Closed a, Num a, Infinitesimal Stream eps) => (eps -> Integer) -> Stream a -> Stream a -> Stream [a]
>cseq_equivalence_list modulus x y = codiagonals $ cseq_equivalence_matrix modulus x y

>converges :: (MetricSpace a) => Stream a -> a -> Stream [Scalar a]
>converges s p = codiagonals $ matrix distance s (constant p)
>
>instance NormedSpace R where
>  norm = abs

>instance InnerProductSpace R where
>   x %. y = x * y

>instance Infinitesimal Stream Rational where
>   epsilon_stream = fmap (1 /) $ Stream.power 10

>instance MetricSpace R where
>  distance x y = abs (x-y)

>instance MetricSpace (Stream R) where
>  distance x y = accumulation_point $ limit $ do 
>                    (xn,yn,n) <- fzip3 x y naturals
>                    let d = abs (xn-yn)
>                    return $! (1 / (2^n)) * d / (1 + d)

>increase_accuracy_by :: (Limiting Stream a) => Integer -> Closure Stream a -> Closure Stream a
>increase_accuracy_by i = limit . Stream.drop i . approximations


>partial_derivate_closure :: (Limiting str a, Fractional a)
>   => (a -> b -> b) -> (b -> a) -> b -> Endo (Closure str a)
>partial_derivate_closure delta f v = Endo $ \dx -> limit $ do
>   let fv = f v
>   eps <- approximations dx
>   return $! (f (delta eps v) - fv) / eps


>partial_derivate_linear :: (LinearTransform f f a, VectorSpace (f a), Closed a, Closed (f a),
> Fractional a, Scalar (f a) ~ a, Linearizable LinearMap (:*:) f f a, Diagonalizable f a)
> => (a -> f a :-> f a) -> (f a :-> f a) -> (f a :-> f a)
>partial_derivate_linear delta f = arr_linear $ \v -> accumulation_point $ limit $ do
>   let fv = f -!< v
>   eps <- epsilon_stream
>   return $! (1/eps) %* (f -!< (delta eps -!< v) %- fv)

>partial_derivate_endo :: (Limiting str a, Fractional a)
>  => (a -> Endo b) -> (b -> a) -> b -> Endo (Closure str a)
>partial_derivate_endo delta f v = Endo $ \dx -> limit $ do
>   let fv = f v
>   eps <- approximations dx
>   return $! (f (delta eps `appEndo` v) - fv) / eps


>-- | <https://en.wikipedia.org/wiki/Wirtinger_derivatives>

>complex_derivate :: (RealFloat r, Closed r)
>                 => (Complex r -> Complex r) -> Complex r -> Complex r
>complex_derivate f z =
>                   (partial_derivate (\eps z' -> z' + (eps :+ 0))
>                                     (realPart . f) z)/2
>                :+ (negate $ (partial_derivate
>                                     (\eps z' -> z' + (0 :+ eps))
>                                     (imagPart . f) z) / 2)

already defined in Math.Number.Stream:

instance (RealFloat a, Infinitesimal str a) => Infinitesimal str (Complex a) where
  epsilon_closure = limit $ liftA2 (:+) (epsilon_stream) (epsilon_stream)

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

instance VectorDerivative R Math.Matrix.Linear.Dual LinearMap where
  divergence f = covector $ real_derivate ((-!<) f)
  grad (Covector f) = arr_linear $ \x -> real_derivate (f -!!<) x

instance VectorCrossProduct R LinearMap where
  curl f = arr_linear $ \x -> real_derivate ((-!<) f) x

>-- | The following instance declaration represents the completeness of the
>-- real number system. 
>instance Limiting Stream R where
>  data Closure Stream R = RClosure { runRClosure :: R }
>  limit str = RClosure $ limit_real str
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

>limit_real :: Stream R -> R
>limit_real str = Limit $ str >>= approximate

>lift_real :: (Rational -> Rational) -> R -> R
>lift_real f = Limit . fmap f . approximate

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
>   accumulation_point = runRClosure

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

>supremum_gen :: (Fractional a, Ord a, Limiting Stream a)
>             => [Closure Stream a] -> Closure Stream a
>supremum_gen [] = negative_infinity_gen
>supremum_gen [x] = x
>supremum_gen (a:lst) = limit $ Pre c $ rest
>   where rest = liftA2 max cr $ approximations $ supremum_gen lst
>         ~(Pre c cr) = approximations a

>negate_limit :: (Limiting str a, Num a) => Closure str a -> Closure str a
>negate_limit = cmap negate

infimum :: [Closure R] -> Closure R
infimum = negate_limit . supremum_gen . map negate_limit

>infimum_gen :: (Fractional a, Ord a, Limiting Stream a) => [Closure Stream a] -> Closure Stream a
>infimum_gen = negate_limit . supremum_gen . map negate_limit

>instance Infinitary R where
>   infinite = infinity

>infinity :: R
>infinity = Limit $ Stream.power 10

>infinity_gen :: (Fractional a, Limiting Stream a) => Closure Stream a
>infinity_gen = limit $ Stream.power 10

>negative_infinity_gen :: (Fractional a, Limiting Stream a) => Closure Stream a
>negative_infinity_gen = limit $ fmap negate $ Stream.power 10

>-- | epsilon is a real that converges to zero.
>instance Infinitesimal Stream R where
>  epsilon_closure = close $ 1 / infinity

>-- | <http://en.wikipedia.org/wiki/Squeeze_theorem Squeeze theorem>

>limit_below :: R -> (R -> R) -> Closure Stream R
>limit_below c f = limit $ approximations (close $ c - accumulation_point epsilon_closure) >>= (return . f)

>-- | <http://en.wikipedia.org/wiki/Squeeze_theorem Squeeze_theorem>

>limit_above :: R -> (R -> R) -> Closure Stream R
>limit_above c f = limit $ approximations (close $ c + accumulation_point epsilon_closure) >>= (return . f)

>-- | <http://en.wikipedia.org/wiki/Squeeze_theorem Squeeze_theorem>
>limit_both :: R -> (R -> R) -> Closure Stream R
>limit_both c f = limit $ approximations (limit_below c f)
>            `interleave` approximations (limit_above c f)


>negative_infinity :: R
>negative_infinity = negate infinity

>epsilon_linear :: R
>epsilon_linear = Limit $ (1 %) <$> nonzero_naturals

>increase_accuracy :: R -> R
>increase_accuracy (Limit ~(Pre _ xr)) = Limit xr

>lessthan_precision :: R -> R -> Integer -> Bool
>lessthan_precision _ _ 0 = False 
>lessthan_precision (Limit ~(Pre x xs)) (Limit ~(Pre y ys)) i
>           | x < y = True
>           | x > y = False
>           | otherwise = lessthan_precision (Limit xs) (Limit ys) (pred i)

>limiting_distance :: (Num a, Limiting str a) => Closure str a -> Closure str a -> Closure str a
>limiting_distance x y = limit $ do 
>                      (x',y') <- approximations x <&> approximations y
>                      return $! abs (x' - y')

>distance_matrix :: R -> R -> (Stream :*: Stream) Rational
>distance_matrix (Limit x) (Limit y) = matrix (\a b -> abs $ a - b) x y

>-- | compute rational approximation more precise than the given rational
>-- the precision is expressed as rational number close to zero,
>-- the result will be within range \([r-p,r+p]\), in the sense
>-- that in the sequence of rational approximations \(r = (r_i | i \in {\mathbf N})\)
>-- we have \(\lvert r_{i+1} - r_i \rvert \leq p\). Note that we do _not_ attempt to prove
>-- that all successive approximations have this same property,
>-- we need the differences to be monotonically decreasing for this
>-- to represent correct precision.
>at_precision_rational :: R -> Rational -> Rational
>at_precision_rational (Limit s) p = check_precision s
> where check_precision ~z@(Pre x ~z'@(Pre y _))
>         | abs (y - x) <= p = x
>         | otherwise = check_precision z'

>-- | here precision is expresses as integer power of 10, e.g.
>--   \(1 == 10^{-1}\), \(2 == 10^{-2}\), \(3 == 10^{-3}\) and so on.
>--   so how many digits after the decimal point are required.
>at_precision :: R -> Integer -> R
>at_precision (Limit s) p = Limit $ check_precision s
>  where lm = 10^^(negate p)
>        check_precision   ~z@(Pre x ~z'@(Pre y _))
>             | abs (y - x) <= lm = z
>             | otherwise = check_precision z'

>-- | rational precision expressed as integer power of 10
>precision_rational :: R -> Integer -> Rational
>precision_rational (Limit s) p = check_precision s
>   where check_precision ~z@(Pre x ~z'@(Pre y yr))
>           | abs (y - x) <= 10^^(negate p) = x
>           | otherwise = check_precision z'

>real_derivate :: (R -> R) -> R -> R
>real_derivate f x = accumulation_point $ derivate_closed f x

>instance DifferentiallyClosed R where
>  derivate = real_derivate
>  integral = integral_real

>instance DifferentiallyClosed (Closure Stream R) where
>  derivate f (RClosure x) = RClosure $ derivate (runRClosure . f . RClosure) x
>  integral (a,b) f = RClosure $ integral (runRClosure a, runRClosure b) (runRClosure . f . RClosure)

>real_exp :: R -> R
>real_exp = fix real_derivate

>-- | i'th element of derivates_at(f,s) is \(D^i[f](s_i)\)
>derivates_at :: (DifferentiallyClosed a) => (a -> a) -> Stream a -> Stream a
>derivates_at f x = derivates f <*> x

>-- | <http://en.wikipedia.org/wiki/Arithmetic%E2%80%93geometric_mean Arithmetic-geometric mean>

>agm :: (Floating a, Limiting str a) => a -> a -> Closure str a
>agm x y = limit $ fmap fst $ iterate_stream (\(x,y) -> ((x+y)/2, sqrt(x*y))) (x,y)

>-- | <http://en.wikipedia.org/wiki/Exponential_function Exponential function>
>-- using limit definition \(\exp(x) = \lim_{n\rightarrow\infty}(1+{{x}\over{n}})^n\).

>expo :: R -> R
>expo x = Limit $ do
>           (xr,n) <- approximate x <&> power_integral 10
>           return $! (1 + (xr/fromIntegral n))^^n

>instance Numerics R where
>   newtons_method f x = accumulation_point $ newtons_method_real_closure f x

>newtons_method_real_closure :: (Closed a) => (a -> a) -> a -> Closure Stream a
>newtons_method_real_closure f x = limit $ iterate_stream iteration x
>   where iteration z' = z' - f z' / accumulation_point (derivate_closed f z')

>eigenvalue :: (Closed a,
>  LinearTraceable LinearMap m a,
>  Linearizable LinearMap (:*:) m m a, 
>               Applicative m, a ~ Scalar (m a),
>               VectorSpace (m a), LinearTransform m m a
>               )
>           => m a :-> m a -> Closure Stream a
>eigenvalue m = newtons_method_real_closure (characteristicPolynomial m) 0

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

>integral_real :: (R,R) -> (R -> R) -> R
>integral_real ~(x,y) f = runRClosure $ limit $ do
>   (eps, xa, ya) <- fzip3 (approximate $ runRClosure epsilon_closure)
>                          (approximate x)
>                          (approximate y)
>   return $! (fromRational eps *) $! Prelude.sum $!
>      map (f . fromRational) [xa,xa+eps..ya]

>integral_rational :: (R,R) -> (Rational -> Rational) -> R
>integral_rational ~(x,y) f = Limit $ do
>       (eps,xa,ya) <- fzip3 (approximate $ runRClosure epsilon_closure)
>                            (approximate x)
>                            (approximate y)
>       return $! (eps *) $! Prelude.sum $! map f [xa,xa+eps..ya]


>-- | for integrate_curve (xa,ya) curve f dx, Requires: \(\lim_{x \rightarrow 0}curve(x) = 0 \land \lim_{i\rightarrow \infty} dx_i = 0\)
>integrate_curve :: (Enum a, Num a, Num r, Limiting str a, Limiting str r)
>                => (a,a) -> (a -> r) -> (r -> r) -> Closure str a -> Closure str r
>integrate_curve (xa,ya) curve f dx  = limit $ do
>   eps <- approximations dx
>   return $! (curve eps *) $! Prelude.sum $! map (f . curve) [xa,xa+eps..ya]

>-- | <http://en.wikipedia.org/wiki/Integral Integral>
>-- This doesn't converge well.

>gamma_via_integral :: R -> Closure Stream R  
>gamma_via_integral zz = integral (close 0,infinity_gen) $ \t -> exp (negate t) * (t ** RClosure (zz-1))
    
>instance (TypeError (Text "Equality of constructive reals is undecidable." :$$:
>                     Text "Please use (<%) and (%<) from Math.Number.Interface.DedekindCut class")) => Eq R where
>  xs == ys = error "equality of constructive reals is undecidable"

>instance (Monad str, StreamObserver str, StreamBuilder str) => Limiting str Bool where
>   data Closure str Bool = BoolClosure { runBoolClosure :: str Bool }
>   limit s = BoolClosure s
>   approximations (BoolClosure s) = s
> 
>approximately_equal :: R -> R -> Bool
>approximately_equal x y = show_at_precision x 30 == show_at_precision y 30

>approximately_eq :: R -> R -> Bool
>approximately_eq x y = a == b
>   where a = show_at_precision x 7
>         b = show_at_precision y 7


>constructively_less :: Rational -> R -> R -> Closure Stream Bool
>constructively_less between (Limit ~(Pre a ar)) (Limit ~(Pre b br)) = limit $
>   (a < between || between < b)
>   `Pre` (approximations $ constructively_less between (Limit ar) (Limit br))


>equality :: (Infinitesimal str R) => Closure str R -> Closure str R -> Closure str Bool
>equality str str'
>   = limit $ liftA3 (\a b eps -> abs (b - a) < eps) astr astr' aeps
>  where astr = fmap approximate $ approximations str
>        astr' = fmap approximate $ approximations str'
>        aeps  = fmap approximate $ epsilon_stream


instance Show (Complex R) where
  showsPrec x (a :+ b) = show_at_precision_real a p
    . showString ":+" . show_at_precision_real b p
   where p = fromIntegral x


>signum_real :: R -> Int -> Int
>signum_real _ 0 = 0
>signum_real r i
>   | ~(a,b) <- properFraction (shead $ approximate r) =
>       if signum a == 0
>        then signum_real (r*10) (i-1)
>         else signum a

>show_at_precision_real :: R -> Integer -> String
>show_at_precision_real r i = (signcorr r . shows a . showString "." . decs) []
>   where signcorr r = if signum a == 0 && signum_real b 30 == -1 then ('-':) else id
>         decs = if a == 0 then decimals_str2 (abs b) i
>                          else decimals (abs (b `at_precision` i)) i
>         (a,b) = properFraction_real r
>         decimals x 0 = id
>         decimals x i
>             | ~(a',b') <- properFraction_real (x*10), r <- decimals b' (pred i) = shows a' . r
>         decimals_str2 x i = case decimals2 x i of
>           (0,res) -> decimals (res `at_precision` i) i 
>           (j,res) -> decimals (res `at_precision` i) i 
>                        . showString "e" . shows j
>         decimals2 x 0 = (0,x)
>         decimals2 x i = let ~(a',b') = properFraction_real (x*10) 
>                           in case a' of
>                             0 -> let (res,res2) = decimals2 b' (pred i) in (pred res,res2)
>                             _ -> (0,x)

>instance Show R where
>    show s = show_at_precision s 20

>instance ShowPrecision R where
>    show_at_precision = show_at_precision_real

>instance PpShow R where
>    pp s = pp (show_at_precision s 10)


instance Real R where -- needs Ord instance
    toRational = shead . approximate . (`at_precision` 5)

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

>properFraction_real :: (Num a) => R -> (a, R)
>properFraction_real r = (fromIntegral wholepart,lift_real (snd . properFraction) r)
>      where rat = shead $ approximate $ r `at_precision` 2
>            wholepart = sign * ((abs $ numerator rat)
>                                `div` (abs $ denominator rat))
>            sign = signum (numerator rat) * signum (denominator rat)

>-- | Enum instance for reals is undecidable with respect to end of
>-- real intervals, since comparison of two real numbers is undecidable.
>-- Thus enumFromTo and enumFromThenTo produce errors indicating
>-- density problem.
>instance Enum R where
>    succ = lift_real succ
>    pred = lift_real pred
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
>    negate = lift_real negate
>    abs = lift_real abs
>    signum = lift_real signum
>    fromInteger i = Limit $ constant $ fromInteger $! i

>instance Fractional R where
>    (Limit s) / (Limit t) = Limit $ liftA2 (/) s t
>    recip = lift_real recip
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

>euler_constant :: R
>euler_constant = exp 1

>napiers_constant :: R
>napiers_constant = exp 1.0

>logarithm_by_newton :: R -> Closure Stream R
>logarithm_by_newton e = newtons_method_real_closure (\x -> exp x - e) 1

>invert_by_newton :: (Closed a) => (a -> a) -> a -> Closure Stream a
>invert_by_newton f e = newtons_method_real_closure (\x -> f x - e) 1

>-- computes x from equation @(Df)(x) = y@ by newton's method
>differential_equation1 :: (Closed a) => (a -> a) -> a -> Closure Stream a
>differential_equation1 f y = newtons_method_real_closure (\x -> accumulation_point (derivate_closed f x) - y) 1


>exp_by_powers :: (Fractional r, Limiting Stream r) => r -> Closure Stream r
>exp_by_powers x = limit $ do
>     n <- naturals
>     let n2 = 10^n
>     return $! (1 + x/fromIntegral n2)^^n2

>exp_by_powers_real :: R -> R
>exp_by_powers_real (Limit x) = Limit $ liftA2 (\n x' -> (1 + (x'/fromIntegral n))^^n) (stail $ stail $ power_integral 10) x

>sum_generating_function :: Stream Rational -> R -> R
>sum_generating_function f (Limit x) = Limit $ uninterleave_index 1
>                                            $ approximate_sums f x

>-- | <https://en.wikipedia.org/wiki/Riemann_zeta_function>
>riemann_zeta :: (Floating a) => a -> Stream a
>riemann_zeta s = sum_stream $ fmap term nonzero_naturals
>   where term n = 1 / (n ** s)

>-- | <https://en.wikipedia.org/wiki/Riemann_zeta_function>
>riemann_zeta_real :: (Integral a) => a -> R
>riemann_zeta_real s = Limit $ sum_stream $ fmap term nonzero_naturals
>   where term n = 1 / (n ^ s)

>-- | <https://en.wikipedia.org/wiki/Riemann_zeta_function>
>riemann_zeta_complex :: (RealFloat a) => Complex a -> Stream (Complex a)
>riemann_zeta_complex s = sum_stream $ fmap term nonzero_naturals
>   where term n = 1 / (n ** s)

instance (Fractional a, Limiting a, Closed a) => Infinitesimal Stream (Stream a) where
  epsilon_stream = cells_linear stream_epsilon

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
     where start = floor ((x*fromIntegral base^p) `at_precision` 1)
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

>golden_ratio :: R
>golden_ratio = Limit $ Stream.drop 4 
>                     $ stream_quotients (fmap fromIntegral fib)

>slow_golden_ratio :: R
>slow_golden_ratio = (1 + sqrt 5) / 2

Doesn't work since triggers problems with equality comparison:

primitive_root_of_unity :: Integer -> Closure (Complex R)
primitive_root_of_unity i = newtons_method (\x -> x ^^ i - (1 :+ 0)) (0 :+ 1)

>-- | https://en.wikipedia.org/wiki/Logistic_function

>logistic :: (Floating a) => a -> a
>logistic x = exp x / (exp x + 1)

>-- | this converts a real to a sequence of doubles, each item
>-- attempting to approximate the real to higher accuracy.
>floating_approximations_real :: R -> Stream Double
>floating_approximations_real = fmap fromRational . approximate

>instance Approximations Stream R where
>   floating_approximations = floating_approximations_real
>   rational_approximations = approximate


>logarithm_by_power :: (Floating a, Limiting Stream a) => a -> Closure Stream a
>logarithm_by_power x = limit $ do
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

>log_newton :: (Floating a, Closed a) => a -> a
>log_newton a = accumulation_point $ newtons_method_real_closure (\x -> exp x - a) 1

>cos_generating_function_ = Pre 1 $ Pre 0 $ liftA2 (*) signs fact
> where signs = stail $ stail $ alternating_possibly_negative_bits
>       fact  = fmap (1/) (stail $ stail $ factorial)

>exp_by_series2 :: R -> R
>exp_by_series2 (Limit x) = Limit $ do
>   xappr <- x
>   sum_stream $ liftA2 (/) (index_powers (constant xappr)) factorial

exp_by_series2 :: R -> R
exp_by_series2 (Limit x) = Limit $ sum_stream $
            matrix (*) (fmap (1/) factorial) (index_powers x)

>exp_by_series :: R -> R
>exp_by_series (Limit x) = Limit $ (Pre 1 $ fmap (1 /) factorial) `approximate_sums` x

>sin_by_series :: R -> R
>sin_by_series (Limit x) = (Limit $ x >>= \xappr ->
>        Stream.filter (\a -> a >= -1 && a <= 1) $ fst $ uninterleave $ sum_stream 
>        $ liftA2 (*) filterStream
>        $ liftA2 (/) (index_powers $ constant xappr)
>        $ factorial) `at_precision` 2
>  where filterStream = Pre 0 $ Pre 1 $ Pre 0 $ Pre (negate 1) $ filterStream

uninterleave stuff is needed to ensure at_precision works correctly,
since the sum of the generating function has zero at every second element,
it means there are always two adjacent equal elements.

>cos_by_series :: R -> R
>cos_by_series (Limit x) = (Limit $ x >>= \xappr ->
>        Stream.filter (\a -> a >= -1 && a <= 1) $
>        fst $ uninterleave $ sum_stream
>        $ liftA2 (*) filterStream
>        $ liftA2 (/) (index_powers (constant xappr)) factorial) `at_precision` 2
>  where filterStream = Pre 1 $ Pre 0 $ Pre (negate 1) $ Pre 0 $ filterStream

>-- | Using Simon Plouffe's BPP digit extraction algorithm for computing pi.
>-- See <https://secure.wikimedia.org/wikipedia/en/wiki/Pi> for details.
>-- exp:  <http://en.wikipedia.org/wiki/Exponential_function Exponential function>
>-- log:  <http://en.wikipedia.org/wiki/Logarithm Logarithm>
>-- <http://en.wikipedia.org/wiki/Trigonometric_functions trigonometric functions>
>-- <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions inverse trigonometric functions>
>-- <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance Floating R where
>    pi = Limit $ Stream.sum_stream $ do 
>            k <- Stream.naturals
>            let kr = k % 1                
>            return $! (16^^(negate k))*(4/(8*kr+1) - 2/(8*kr+4)
>                                       - 1/(8*kr+5) - 1/(8*kr+6))
>    exp (Limit x) = Limit $ Stream.drop 3 $ x >>= \xappr -> sum_stream $
>         liftA2 (/) (index_powers (constant xappr)) factorial
>    sqrt v = newtons_method (\x -> x*x - v) 1
>    log a = 2* (Limit $ Stream.sum_stream $ do
>                  (n,aaprx) <- naturals <&> approximate a'
>                  return $! (1 / (2*fromIntegral n+1))*aaprx^(2*n+1))
>         where a' = (a-1)/(a+1)
>    x ** y = exp (y * log x)
>    sin = sin_by_series
>    cos = cos_by_series
>    tan x = sin x / cos x
>    asin x = integral_real (0,x) $ \a -> 1 / (sqrt $! 1 - a*a)
>    acos x = integral_real (x,1) $ \a -> 1 / (sqrt $! 1 - a*a)
>    atan x = integral (0,x) $ \a -> 1 / (a*a+1)
>    sinh x = (exp x - exp (negate x)) / 2
>    cosh x = (exp x + exp (negate x)) / 2
>    tanh x = sinh x / cosh x
>    asinh x = log $! x + sqrt (x*x+1)
>    acosh x = log $! x + sqrt (x*x-1)
>    atanh x = log ((1 + x)/(1-x)) / 2

mandelbrot_polynomial :: Complex R -> Complex R -> Complex R
mandelbrot_polynomial c z = z*z + c

mandelbrot_stream :: Complex R -> Stream (Complex R)
mandelbrot_stream c = iterate_stream (mandelbrot_polynomial c) 0

mandelbrot :: Complex R -> Closure (Complex R)
mandelbrot c = limit (mandelbrot_stream c)

>instance MetricSpace (Ratio Integer) where
>   distance x y = abs (x - y)
>instance MetricSpace Integer where { distance x y = abs (x - y) }
>instance MetricSpace Int where { distance x y = abs (x - y) }

>approximate_sums_modulus :: Modulus -> Modulus -> Modulus
>approximate_sums_modulus ((Limit x) `Modulus` f) ((Limit y) `Modulus` g)
>       = (Limit $ approximate_sums x y) `Modulus` \r -> max (f r) (g r)

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
>del_vector2 f (Vector2 x y) = Vector2 (partial_derivate1_2 ff x y)
>                                      (partial_derivate2_2 ff x y)
>  where ff a b = f (Vector2 a b)
