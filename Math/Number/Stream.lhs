>{-# LANGUAGE Safe,UndecidableInstances, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, MagicHash, TypeOperators, FlexibleContexts, Arrows, TypeFamilies, ScopedTypeVariables, DeriveGeneric, DeriveDataTypeable, BangPatterns, Rank2Types, StandaloneDeriving, TypeApplications #-}
>{-# OPTIONS_GHC -Wall #-}
>-- |
>--  Module: Math.Number.Stream
>--  Description: Stream implementation
>--  Copyright: (c) Esa Pulkkinen, 2018
>--  License: LGPL
>--  Maintainer: esa.pulkkinen@iki.fi
>--  Stability: experimental
>-- 
>-- This module implements lazy infinite streams,
>-- stream generating functions and lots of operations on the lazy streams.
>-- some examples for use of this module:
>-- 
>-- @fib = 1 \`div\` (1 - s_z - s_z*s_z)@
>-- 
>-- @pascal_triangle = Matrix $ 1 \`div\` (1 - s_z - s_z2*s_z)@
>-- 
>-- @take 4 fib == [1,1,2,3]@
>-- 
>-- @alternating_bits = cycle [0,1]@
>
>module Math.Number.Stream where
>import safe Data.Monoid
>import safe Data.Complex
>import safe qualified Data.Foldable
>import safe qualified Control.Category as C
>import safe Control.Applicative
>import safe qualified Prelude as P
>import safe Prelude hiding (zip,unzip, zipWith,cycle,take,filter,drop,zipWith3,sin,cos,either,takeWhile,dropWhile,span,splitAt)
>import safe Math.Tools.Functor
>import safe qualified Math.Tools.Arrow as TArrow
>import safe Math.Tools.Visitor
>import safe Math.Tools.CoMonad hiding (copy)
>import safe Math.Tools.PrettyP
>import safe Math.Tools.FixedPoint (Rec(..))
>import safe Math.Tools.Adjunction hiding (swap)
>import safe Math.Tools.Isomorphism
>import safe Math.Tools.Integer
>import safe qualified Math.Tools.Queue as Q
>import safe qualified Data.Sequence as Seq
>import safe qualified Text.PrettyPrint as Pretty
>import safe Math.Tools.I
>import safe Math.Tools.Queue (Queue)
>import safe Math.Matrix.Linear
>import safe Control.Arrow
>import safe Data.Ratio
>import safe GHC.Generics hiding ((:+:),(:*:))
>import safe Data.Typeable
>import safe Data.Data
>import safe Control.Monad.Fix (fix)

import qualified Model.Nondeterminism as Nondet

>import safe qualified Math.Tools.Nondeterministic as Nondeterministic
>import safe qualified Data.List as List
>import safe qualified Math.Matrix.Interface as Matrix
>import safe Math.Matrix.Interface hiding ((|>))
>import safe Math.Number.StreamInterface
>import safe Math.Number.Interface

>infixl 4 !

>default ()

>deriving instance Typeable (Stream a)
>deriving instance (Data a) => Data (Stream a)
>deriving instance Generic (Stream a)

>instance StreamBuilder Stream where
>   pre = Pre

instance ProjectionDual Stream Math.Matrix.Linear.Dual a where
  projection_dual = Pre (covector shead) $
     fmap (\e -> Covector (bracketMap e C.. stail_lazy)) projection_dual

>(!) :: Stream a -> Integer -> a
>(!) s i = shead (drop i s)

>stail_stream :: Stream a -> Stream a
>stail_stream ~(Pre _ xr) = xr

>shead_strict :: Stream a -> a
>shead_strict (Pre !x _) = x

>matrix_powers :: (Diagonalizable g a, LinearTransform g g a,Transposable g g a,InnerProductSpace (g a),
>                 Scalar a ~ a, Scalar (g a) ~ a, Scalar (g (g a)) ~ Scalar (g a))
> => (g :*: g) a -> Stream ((g :*: g) a)
>matrix_powers x = Pre identity $ fmap (x %*%) $ matrix_powers x

>power_series_powers :: (Num a) => Stream a -> (Stream :*: Stream) a
>power_series_powers s = Matrix $ Pre 1 $ fmap (s *) $ cells $ power_series_powers s

>compose_power_series :: (Closed a, Num a) => Stream a -> Stream a -> Stream a
>compose_power_series a b = ssum $ fmap (liftA2 (*) a) $ cells $ power_series_powers b

>power_series_exp :: (Fractional a, Closed a) => Stream a -> Stream a
>power_series_exp = compose_power_series exponential_stream

>matrix_exponential_stream ::
> (Fractional (Scalar ((g :*: g) a)),
>   Diagonalizable g a, LinearTransform g g a,
>   InnerProductSpace (g a), VectorSpace ((g :*: g) a),
>   Num ((g :*: g) a), Scalar (g (g a)) ~ Scalar (g a),
>   Scalar (g a) ~ a, Scalar a ~ a) =>
>    (g :*: g) a -> Stream ((g :*: g) a)
>matrix_exponential_stream x = sum_stream str
>  where str = liftA2 (\x' kf -> (1 / kf) %* x') (matrix_powers x) factorial

>-- | <https://en.wikipedia.org/wiki/Matrix_exponential>
>matrix_exponential :: (Fractional (Scalar ((g :*: g) a)),
>                             Diagonalizable g a, LinearTransform g g a, InnerProductSpace (g a),
>                             VectorSpace ((g :*: g) a), Num ((g :*: g) a),
>                             Scalar (g (g a)) ~ Scalar (g a), Scalar (g a) ~ a, Scalar a ~ a) =>
>                            (g :*: g) a -> (g :*: g) a
>matrix_exponential x = shead $ drop 10 (matrix_exponential_stream x)

>-- | <https://en.wikipedia.org/wiki/Matrix_exponential>

instance (Num a) => FiniteDimensional a Math.Matrix.Linear.Dual Stream LinearMap where
   finite = linear $ \ (Matrix (Covector f)) ->
     Pre (f -!!< (covector shead))
             (fmap (\x -> f -!!< covector (shead . x)) tls)
    where tls = stail `Pre` (fmap (\x -> x . stail) tls)


>instance Limiting Stream Rational where
>   data Closure Stream Rational = FiniteRational (Stream Rational)
>                   | MinusInfiniteRational
>                   | InfiniteRational
>   limit = FiniteRational
>   approximations (FiniteRational s) = s
>   approximations MinusInfiniteRational = fmap negate $ power 10
>   approximations InfiniteRational = power 10

>instance Infinitary (Closure Stream Rational) where
>   infinite = InfiniteRational

>instance Infinitary (Closure Stream Integer) where
>   infinite = InfiniteInteger

>instance Fractional (Closure Stream Rational) where
>   (FiniteRational x) / (FiniteRational y) = FiniteRational $ liftA2 (/) x y
>   (FiniteRational _) / InfiniteRational = 0
>   (FiniteRational _) / MinusInfiniteRational = 0
>   MinusInfiniteRational / (FiniteRational _) = error "can't compute sign of rational closure in -INF/lim{i->INF}(x_i)"
>   InfiniteRational / (FiniteRational _) = error "can't compute sign of rational closure in INF/lim{i->INF}(x_i)"
>   _ / _ = error "undefined: INF/INF"
>   recip (FiniteRational x) = FiniteRational $ fmap recip x
>   recip InfiniteRational = 0
>   recip MinusInfiniteRational = -0
>   fromRational = FiniteRational . constant

>instance Num (Closure Stream Rational) where
>   InfiniteRational + InfiniteRational = InfiniteRational
>   MinusInfiniteRational + InfiniteRational = error "undefined INF-INF"
>   InfiniteRational + MinusInfiniteRational = error "undefined INF-INF"
>   MinusInfiniteRational + MinusInfiniteRational = MinusInfiniteRational
>   InfiniteRational + _ = InfiniteRational
>   MinusInfiniteRational + _ = MinusInfiniteRational
>   _ + InfiniteRational = InfiniteRational
>   _ + MinusInfiniteRational = MinusInfiniteRational
>   (FiniteRational x) + (FiniteRational y) = FiniteRational (liftA2 (+) x y)
>   a - b = a + (negate b)
>   (FiniteRational x) * (FiniteRational y) = FiniteRational $ liftA2 (*) x y
>   InfiniteRational * InfiniteRational = InfiniteRational
>   InfiniteRational * MinusInfiniteRational = MinusInfiniteRational
>   MinusInfiniteRational * InfiniteRational = MinusInfiniteRational
>   MinusInfiniteRational * MinusInfiniteRational = InfiniteRational
>   _ * InfiniteRational = error "(*): can't compute sign of rational closure in lim{i->INF}(x_i)*INF"
>   InfiniteRational * _ = error "(*): can't compute sign of rational closure in INF*lim{i->INF}(x_i)"
>   _ * MinusInfiniteRational = error "(*): can't compute sign of rational closure in lim{i->INF}(x_i)* -INF"
>   MinusInfiniteRational * _ = error "(*): can't compute sign of rational closure in -INF*lim{i->INF}(x_i)"
>   negate InfiniteRational = MinusInfiniteRational
>   negate MinusInfiniteRational = InfiniteRational
>   negate (FiniteRational x) = FiniteRational (fmap negate x)
>   abs (FiniteRational x) = FiniteRational (fmap abs x)
>   abs MinusInfiniteRational = InfiniteRational
>   abs InfiniteRational = InfiniteRational
>   signum InfiniteRational = FiniteRational (constant 1)
>   signum MinusInfiniteRational = FiniteRational (constant $ negate 1)
>   signum (FiniteRational s) = FiniteRational (fmap signum s)
>   fromInteger = FiniteRational . fromInteger

>instance Limiting Stream Integer where
>   data Closure Stream Integer = FiniteInteger { integer_closure_stream :: Stream Integer }
>     | MinusInfiniteInteger
>     | InfiniteInteger
>   limit s = FiniteInteger s
>   approximations (FiniteInteger s) = s
>   approximations MinusInfiniteInteger = fmap negate $ power_integral 10
>   approximations InfiniteInteger = power_integral 10

>instance (Num (Closure Stream Integer)) where
>   InfiniteInteger + InfiniteInteger = InfiniteInteger
>   MinusInfiniteInteger + InfiniteInteger = error "undefined inf - inf"
>   InfiniteInteger + MinusInfiniteInteger = error "undefined inf - inf"
>   MinusInfiniteInteger + MinusInfiniteInteger = MinusInfiniteInteger
>   InfiniteInteger + _ = InfiniteInteger
>   MinusInfiniteInteger + _ = MinusInfiniteInteger
>   _ + InfiniteInteger = InfiniteInteger
>   _ + MinusInfiniteInteger = MinusInfiniteInteger
>   (FiniteInteger s) + (FiniteInteger s')
>     = FiniteInteger (s + s') 
>   a - b = a + (negate b)
>   negate InfiniteInteger = MinusInfiniteInteger
>   negate MinusInfiniteInteger = InfiniteInteger
>   negate (FiniteInteger s) = FiniteInteger (fmap negate s)
>   InfiniteInteger * InfiniteInteger = InfiniteInteger
>   MinusInfiniteInteger * InfiniteInteger = MinusInfiniteInteger
>   InfiniteInteger * MinusInfiniteInteger = MinusInfiniteInteger
>   MinusInfiniteInteger * MinusInfiniteInteger = InfiniteInteger
>   InfiniteInteger * _ = error "cannot compute sign of integer closure in INF * lim{i->INF}(x_i)"
>   MinusInfiniteInteger * _ = error "cannot compute sign of integer closure in -INF*lim{i->INF}(x_i)"
>   _ * InfiniteInteger = error "cannot compute sign of integer closure in lim{i->INF}(x_i)*INF"
>   _ * MinusInfiniteInteger = error "cannot compute sign of integer closure in -INF*lim{i->INF}(x_i)"
>   (FiniteInteger s) * (FiniteInteger s')
>     = FiniteInteger (liftA2 (*) s s')
>   abs MinusInfiniteInteger = InfiniteInteger
>   abs InfiniteInteger = InfiniteInteger
>   abs (FiniteInteger s) = FiniteInteger (fmap abs s)
>   signum InfiniteInteger = constantIntegerClosure 1
>   signum MinusInfiniteInteger = constantIntegerClosure (negate 1)
>   signum (FiniteInteger s) = FiniteInteger (fmap signum s)
>   fromInteger = constantIntegerClosure

>constantIntegerClosure :: Integer -> Closure Stream Integer
>constantIntegerClosure i = FiniteInteger (constant i)
>
>infiniteInteger :: Closure Stream Integer
>infiniteInteger = InfiniteInteger
>negativeInfiniteInteger :: Closure Stream Integer
>negativeInfiniteInteger = MinusInfiniteInteger

>instance Show (Closure Stream Integer) where
>   show (FiniteInteger s) = " ... " ++ concat (List.intersperse "," (map show $ take 10 s)) ++ " ... "
>   show InfiniteInteger = "INF"
>   show MinusInfiniteInteger = "-INF"

>instance Show (Closure Stream Rational) where
>   show InfiniteRational = "INF"
>   show MinusInfiniteRational = "-INF"
>   show (FiniteRational s) = " ... " ++ concat (List.intersperse "," (map show $ take 10 s)) ++ " ... "

>instance Limiting Stream Double where
>   data Closure Stream Double = DoubleClosure (Stream Double)
>   limit zz = DoubleClosure zz
>   approximations ~(DoubleClosure x) = x

>instance Limiting Stream Float where
>   data Closure Stream Float = FloatClosure (Stream Float)
>   limit zz = FloatClosure zz
>   approximations ~(FloatClosure x) = x

>-- | Notice that after double precision is not sufficient, the infinitesimals are zero.
>instance Infinitesimal Stream Double where
>   epsilon_stream = Pre 1.0 $ fmap (/10.0) epsilon_stream

>-- | Notice that after float precision is not sufficient, the infinitesimals are zero.
>instance Infinitesimal Stream Float where
>   epsilon_stream = Pre 1.0 $ fmap (/10.0) epsilon_stream

>instance Closed Double where
>   accumulation_point (DoubleClosure ~(Pre x ~(Pre y yr)))
>     | abs (y - x) <= 1 / (fromInteger $ (floatRadix (0 :: Double)) ^ (floatDigits (0 :: Double) - 1)) = x
>     | otherwise = accumulation_point (DoubleClosure yr)

>instance Closed Float where
>   accumulation_point (FloatClosure ~(Pre x ~(Pre y yr)))
>     | abs (y - x) <= 1 / (fromInteger $ (floatRadix (0 :: Float)) ^ (floatDigits (0 :: Float) - 1)) = x
>     | otherwise = accumulation_point (FloatClosure yr)

>limiting_iso :: (TArrow.BiArrow arr, Limiting Stream a) => arr (Stream a) (Closure Stream a)
>limiting_iso = limit TArrow.<-> approximations

>vec_stream :: Stream a -> Integer -> a
>vec_stream ~(Pre x xr) i = if i == 0 then x else vec_stream xr (pred i)

>-- | mapping operation for closures.
>-- Note we cannot make this a Functor instance due to constraint in the type.
>cmap :: (Limiting s a, Limiting s b) => (a -> b) -> Closure s a -> Closure s b
>cmap f = limit . fmap f . approximations
>
>cliftA2 :: (Limiting s a, Limiting s b, Limiting s c)
>  => (a -> b -> c) -> Closure s a -> Closure s b -> Closure s c
>cliftA2 f a b = limit $ liftA2 f (approximations a) (approximations b)
>
> 
>-- | operation for producing a closure out of single element.
>close :: (Limiting Stream a) => a -> Closure Stream a
>close x = limit $ constant x

>closure_limit :: (Num a, Closed a, Limiting Stream a) => Stream (Closure Stream a) -> Closure Stream a
>closure_limit = stream_limit . limit . fmap approximations

>-- | Monadic bind operation for closures,
>-- again cannot be made instance of Monad.
>(.>>=.) :: (Limiting s a, Limiting s b) => Closure s a -> (a -> Closure s b) -> Closure s b
>x .>>=. f = limit $ approximations x >>= (approximations . f)

>instance Approximations Stream Float where
>  floating_approximations = constant . realToFrac
>  rational_approximations = constant . toRational

>instance Approximations Stream Double where
>  floating_approximations = constant
>  rational_approximations = constant . toRational

>instance Foldable Stream where
>   foldMap f ~(Pre x xr) = f x <> foldMap f xr

>instance Traversable Stream where
>   traverse f ~(Pre x xr) = Pre <$> f x <*> traverse f xr

>instance (Num a, Limiting Stream a) => Limiting Stream (Stream a) where
>   data Closure Stream (Stream a) = SClosure {
>     runSClosure :: Stream (Closure Stream a),
>     stream_limit :: Closure Stream a }
>   limit x = SClosure (fmap limit $ sequenceA x)
>                      (limit $ stream_diagonal_impl $ Matrix $ x)
>   approximations = sequenceA . fmap approximations . runSClosure

>instance (Show (Closure Stream a)) => Show (Closure Stream (Stream a)) where
>   show (SClosure str l) = "lim[" ++ show (take 10 str) ++ "...] = " ++ show l

>instance (Closed a, Num a) => Closed (Stream a) where
>   accumulation_point = approximations . stream_limit

>instance (Applicative f, Traversable f,
> Traversable str,
> Applicative g, Traversable g,
> Limiting str a) => Limiting str ((f :*: g) a) where
>   data Closure str ((f :*: g) a) = MatrixClosure {
>      runMatrixClosure :: (f :*: g) (Closure str a) }
>   limit zz = MatrixClosure $ fmap limit $ sequenceA zz
>   approximations = sequenceA . fmap approximations . runMatrixClosure

instance (Closed a, Applicative f, Applicative g,
 Infinitesimal Stream (f a), Infinitesimal Stream (g a),
 Traversable f, Traversable g)
 => Closed ((f :*: g) a) where
   accumulation_point (MatrixClosure m) = fmap accumulation_point m

>-- | https://en.wikipedia.org/wiki/Power_iteration
>eigenvector_by_power_iteration ::
>   (Fractional (Matrix.Scalar (m a)),
>    Matrix.NormedSpace (m a),
>    Matrix.LinearTransform m m a)
>   => (m :*: m) a -> m a -> Stream (m a)
>eigenvector_by_power_iteration a b = iterate_stream f b
>   where f b' | m <- a Matrix.<<*> b' = (1 / Matrix.norm m) Matrix.%* m

>stream_distance_count :: (Eq a) => Stream a -> Stream a -> Integer
>stream_distance_count ~(Pre x xr) ~(Pre y yr) 
>         | not (x == y) = 0
>         | otherwise = succ $ stream_distance_count xr yr

>stream_distance :: (Eq a) => Stream a -> Stream a -> Rational
>stream_distance x y = 1 % (2 ^ stream_distance_count x y)

>-- | <https://en.wikipedia.org/wiki/Taylor_series>
>taylor :: (Fractional a, DifferentiallyClosed a)
>  => (a -> a) -> a -> (Stream :*: Stream) a
>taylor f a = Matrix $ sum_stream $ mapper <$> sub_powers
>   where mapper p = liftA3 (\a b c -> a*b*c) der divider p
>         divider = fmap (1/) factorial
>         der = derivates f <*> constant a
>         sub_powers = cells $ stream_powers (s_z-fromNum a)


>instance Limiting Stream (IO ()) where
>  data Closure Stream (IO ()) = IOClosure { runIOClosure :: IO () }
>  limit ~(Pre x xr) = IOClosure $ x >> runIOClosure (limit xr)
>  approximations x = Pre (runIOClosure x) $ fmap (runIOClosure x >>) $ approximations x

>instance (Monad m) => Limiting Stream (Kleisli m a a) where
>  data Closure Stream (Kleisli m a a) = KleisliClosure { runKleisliClosure :: Kleisli m a a }
>  limit ~(Pre x xr) = KleisliClosure $ (runKleisliClosure $ limit xr) C.. x
>  approximations x = Pre (runKleisliClosure x) $ fmap (C.. runKleisliClosure x)$ approximations x

>instance Nondeterministic.Nondeterministic Stream where
>   guess = cycle

>-- | Show instance displays 15 elements from beginning of stream
>-- To display more elements, use 'Math.Number.Stream.drop' operation.
>instance (Show x) => Show (Stream x) where
>  showsPrec _ str = showList (take 15 str)

  show str = show (fmap show $ take 15 str)

>-- | pretty printing displays 15 element prefix of the stream.
>instance (PpShow x) => PpShow (Stream x) where
>   pp zz = pp_list (take 15 zz)

>instance PpShowF Stream where
>   ppf zz = pp_list (take 15 zz)

>instance PpShowVerticalF Stream where
>   ppf_vertical zz = Pretty.vcat $ take 15 $ fmap pp zz


>-- | monoid instance for streams generated by monoid instance of the elements.
>instance (Monoid a) => Monoid (Stream a) where
>  mempty = pure mempty
>  mappend = liftA2 mappend

instance (Num a, SupportsMatrixMultiplication Stream Stream Stream a, Closed a, ConjugateSymmetric a) => Semigroup ((Stream :*: Stream) a) where
   (<>) = (%**%)

instance (Num a, Closed a, ConjugateSymmetric a) => Monoid ((Stream :*: Stream) a) where
   mempty = identity naturals
   mappend = (%**%)

>instance (Semigroup a) => Semigroup (Stream a) where
>   (<>) = liftA2 (<>)

instance (Num a) => Matrix.VectorSpace ((Stream :*: Stream) a) where
  type Scalar ((Stream :*: Stream) a) = a
  vzero = Matrix $ constant (constant 0)
  vnegate (Matrix x) = Matrix $ fmap (fmap negate) x
  v %* (Matrix x) = Matrix $ fmap (fmap (v *)) x
  (Matrix x) %+ (Matrix y) = Matrix $ liftA2 (liftA2 (+)) x y 

>-- | square matrix implementation for streams.
>instance (Num a) => Matrix.Diagonalizable Stream a where
>  vector_dimension s = liftA2 seq s $ naturals
>  identity = stream_matrix_impl (constant 1) $ codiag
>     where codiag = (zero,zero) `Pre` codiag
>  identity_impl dim = identity
>  diagonal_impl = stream_diagonal_impl
>  diagonal_matrix_impl = stream_diagonal_matrix

>-- | This implementation of infinite identity matrix requires
>-- Integral constraint.
>-- it is not as efficient as 'identity', but is implemented using
>-- generating functions.
>stream_identity_matrix :: (Integral a) => (Stream :*: Stream) a
>stream_identity_matrix = Matrix $ 1 `div` (1 - s_z*s_z2)

>-- | stream_diagonal is the "depth-first" traversal over two-dimensional streams,
>-- where the choice always goes to diagonal elements.
>--
>-- stream_diagonal (matrix f x y) == liftA2 f x y
>stream_diagonal :: (Closed a, ConjugateSymmetric a, Num a) => Stream a :-> Stream a -> Stream a
>stream_diagonal xx | ~(Matrix ~(Pre ~(Pre x _) dr)) <- fromLinear xx
>   = Pre x $ stream_diagonal_impl $ Matrix $ fmap stail dr
>

>instance (Matrix.ConjugateSymmetric a, Num a, Closed a)
>     => Matrix.InnerProductSpace (Stream a) where
>  x %. y = ssum $ do { (xa,ya) <- x <&> y ; return (xa* (conj ya)) }

>instance (Closed a, Matrix.ConjugateSymmetric a, Floating a)
>    => Matrix.NormedSpace (Stream a) where
>  norm x = sqrt (x Matrix.%. x)

>instance (Closed a, Num a) => Matrix.LinearTransform Stream Stream a where
>  s <*>> m = fmap (ssum . liftA2 (*) s) $ cells m
>  m <<*> s = fmap (ssum . liftA2 (*) s) $ cells $ transpose_impl $ m



>instance (Matrix.ConjugateSymmetric a) => Matrix.ConjugateSymmetric (Stream a) where
>   conj (Pre x xr) = Pre (Matrix.conj x) (Matrix.conj xr)

>-- | According to <http://patternsinfp.wordpress.com/2010/12/31/stream-monad/>, the diagonal
>-- is the join of the stream monad.
>instance Monad Stream where
>   return = constant
>   m >>= f = stream_diagonal_impl $ Matrix (fmap f m)
>   fail str = Pre (error ("Stream.fail:" ++ str)) (fail str)

>-- | sum of a stream elements.
>ssum :: (Closed a, Num a) => Stream a -> a
>ssum = accumulation_point . limit . sum_stream

>svsum :: (Closed a, VectorSpace a) => Stream a -> a
>svsum = accumulation_point . limit . vsum_stream

>increasing_substream :: (Ord a) => Stream a -> Stream a
>increasing_substream ~(Pre x xr) = Pre x res
>  where res = increasing_substream (dropWhile (< x) xr)

>sub_member :: (Ord a) => Stream a -> a -> Bool
>sub_member s x = prim_member (increasing_substream s) x
>   where prim_member ~(Pre x' xr) y = case compare x' y of
>              EQ -> True
>              GT -> False
>              LT -> prim_member xr y            

>-- | bind diagonal element and horizontal and vertical strip
>bind_matrix :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a -> (a -> Stream a -> Stream a -> Stream b)
>            -> Stream b 
>bind_matrix f h = do 
>                     d <- stream_diagonal f
>                     (x,y) <- stream_codiagonal f                       
>                     h d x y

>-- | stream matrix with a specified diagonal, other elements are zero.
>stream_as_diagonal :: (ConjugateSymmetric a, Num a, Closed a) => Stream a -> Stream a :-> Stream a
>stream_as_diagonal x = stream_matrix x (return (zero,zero))

>-- | adding a row to a matrix.
>add_row :: f a -> (Stream :*: f) a -> (Stream :*: f) a
>add_row x = Matrix . Pre x . cells

>first_row :: (Stream :*: g) a -> g a
>first_row = shead . cells
>
>first_column :: (Functor f) => (f :*: Stream) a -> f a
>first_column = fmap shead . cells

>remove_row :: (Stream :*: f) a -> (Stream :*: f) a
>remove_row = Matrix . stail . cells

>remove_row_endo :: Endo ((Stream :*: f) a)
>remove_row_endo = Endo remove_row

>-- | adding a column to a matrix.
>add_column :: (Applicative f) => f a -> (f :*: Stream) a -> (f :*: Stream) a
>add_column x = Matrix . liftA2 Pre x . cells

>add_column_endo :: (Applicative f) => f a -> Endo ((f :*: Stream) a)
>add_column_endo = Endo . add_column

>remove_column :: (Functor f) => (f :*: Stream) a -> (f :*: Stream) a
>remove_column = Matrix . fmap stail . cells

>remove_column_endo :: (Functor f) => Endo ((f :*: Stream) a)
>remove_column_endo = Endo remove_column

>join3 :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a -> Stream (a,a,a)
>join3 m = m `bind_matrix` \x y zz -> do
>              (y',z') <- y <&> zz
>              return (x,y',z')

>instance Comonad Stream where
>   extract = shead
>   duplicate = cells . tails
>   extend f ~z'@(Pre _ xr) = Pre (f z') (extend f xr)

>instance CircularComonad Stream where
>   rotate = stail

>instance InfiniteComonad Stream where
>   comonad_pre = Pre

>tails :: Stream a -> (Stream :*: Stream) a
>tails xs = Matrix $ Pre xs (cells $ tails (stail xs))

>skippers :: Stream a -> (Stream :*: Stream) a
>skippers xs = Matrix $ Pre xs (cells $ skippers (skip_even xs))

>-- | folds over streams
>instance Visitor (Stream a) where
>   data Fold (Stream a) b = forall c. StreamFold (a -> c -> b) (Fold (Stream a) c)
>   visit (StreamFold f y) ~(Pre x xr) = f x (visit y xr)

>-- | unfold for streams
>instance (Builder a) => Builder (Stream a) where
>   data Unfold (Stream a) b = StreamUnfold (Unfold a b) (Unfold (Stream a) b)
>   build (StreamUnfold e f) x = Pre (build e x) (build f x)

>-- | The instance of Eq is kind of bogus, because equality for streams is
>-- only semidecidable. It's still possible to use this if you know
>-- that the operation on your specific streams terminate.
>-- worst case occurs if the streams contain exactly equal elements.
>instance (Eq a) => Eq (Stream a) where
> ~(Pre x xr) == ~(Pre y yr) = x == y && xr == yr
> ~(Pre x xr) /= ~(Pre y yr) = x /= y || xr /= yr

>-- | Interpretation of a stream as a polynomial (its ordinary generating function)
>-- \[OGF_{s}(z) = \sum_{i=0}^{\infty}s_iz^i\]
>-- Good exposition exists in http://en.wikipedia.org/wiki/Formal_power_series
>--
>-- the (*) operation is specific to ordinary generating function interpretation, i.e.
>-- discrete convolution/Cauchy product
>--
>-- \[(xy)_k = \sum_{i+j=k}x_iy_j\]
>-- \[OGF_{xy}(z) = \sum_{k=0}^{\infty}\sum_{i+j=k}x_iy_jz^k\]
>-- 
>instance (Num a) => Num (Stream a) where
>   (+) = liftA2 (+)
>   (-) = liftA2 (-)
>   negate = liftA negate
>   abs = liftA abs
>   signum = liftA signum
>   x * y = fmap sum_seq $ codiagonals_seq $ matrix (*) x y
>   fromInteger i = Pre (fromInteger i) zero

>fromComplex :: (RealFloat a) => Complex a -> Stream (Complex a)
>fromComplex c = Pre c zero

>adjusted_sequence_product :: (Fractional a) => Stream a -> Stream a -> Stream a
>adjusted_sequence_product x y = fmap average_seq $ codiagonals_seq $ matrix (*) x y

>average_seq :: (Fractional a) => Seq.Seq a -> a
>average_seq s = sum_seq s / fromIntegral (Seq.length s)

>-- <https://en.wikipedia.org/wiki/Cauchy_product>
>convolution_product_matrix :: (Functor m, Functor n, Num a)
>  => (m :*: Stream) a -> (n :*: Stream) a -> (m :*: n) (Stream a)
>convolution_product_matrix (Matrix a) (Matrix b)
> = matrix ((*) :: (Num a) => Stream a -> Stream a -> Stream a) a b

>sum_seq :: (Num a) => Seq.Seq a -> a
>sum_seq = foldr (+) 0

>stream_powers :: (Num a) => Stream a -> (Stream :*: Stream) a
>stream_powers f = Matrix $ Pre 1 $ fmap (f *) $ cells $ stream_powers f

>-- | 
>-- For streams \(b = (b_i)_{i\in{\mathbf{N}}}\) and \(a = (a_i)_{i\in{\mathbf{N}}}\)
>-- representing generating functions \(g(z) = \sum_{i=0}^{\infty}{b_iz^i}\) and \(f(z) = \sum_{i=1}^{\infty}{a_iz^i}\)
>-- where \(a_0 = 0\), this computes:
>-- \[(g \circ f)(z) = g(f(z)) = \sum_{i=0}^{\infty}{b_i(f(z))^i} = \sum_{i=0}^{\infty}(b_i(\sum_{j=1}^{\infty}{a_jz^j})^i)\]
>-- 
>-- Note that identity of composition on the right is 's_z'.
>-- That is, \(s \circ s_z = s\).
>-- <https://en.wikipedia.org/wiki/Formal_power_series>
>compose :: (ConjugateSymmetric a, Num a, Eq a) => Stream a -> Stream a -> Stream a
>compose g f@(Pre 0 _) = fmap sum $ liftA2 take nonzero_naturals
>                                 $ cells $ Matrix.transpose_impl $ Matrix
>                                 $ fmap (liftA2 (*) g) (cells $ stream_powers f)
>compose _ _ = error "Stream must begin with zero to be pre-composed"

>matrix_convolution :: (InnerProductSpace (g a), Transposable g h a, Functor f,
>  VectorSpace ((f :*: h) a), Scalar (g a) ~ a)
> => Stream ((f :*: g) a) -> Stream ((g :*: h) a) -> Stream ((f :*: h) a)
>matrix_convolution x y = fmap Matrix.vsum $ codiagonals_seq $ matrix (Matrix.%*%) x y

>matrix_convolution_linear ::
> (InnerProductSpace (g a), Transposable g h a, Functor f,
> Linearizable LinearMap (:*:) g h a, Linearizable LinearMap (:*:) f g a,
> Linearizable LinearMap (:*:) f h a,
>  VectorSpace ((f :*: h) a), Scalar (g a) ~ a)
> => Stream (f a :-> g a) -> Stream (g a :-> h a) -> Stream (f a :-> h a)
>matrix_convolution_linear x y = fmap linear $
>   matrix_convolution (fmap fromLinear x) (fmap fromLinear y)

>matrix_convolution_product_linear :: (Closed a, Fractional a, ConjugateSymmetric a) => (Stream a :-> Stream a) -> (Stream a :-> Stream a) -> (Stream a :-> Stream a)
>matrix_convolution_product_linear x y =
>   linear $ matrix_convolution_product (fromLinear x) (fromLinear y)

>-- | computes average to normalize the matrix
>matrix_convolution_product :: (ConjugateSymmetric a, Fractional a) => (Stream :*: Stream) a -> (Stream :*: Stream) a -> (Stream :*: Stream) a
>matrix_convolution_product x y =
> Matrix $ fmap average_seq $ codiagonals_seq $
>    matrix (*) (cells x) (cells $ conj y)

>-- | @distance_product a b@ computes \[l_k = \sqrt{\sum_{i+j=k}a_ib_j^{*}}\].
>distance_product :: (Floating a, ConjugateSymmetric a) => Stream a -> Stream a -> Stream a
>distance_product a b = fmap (sqrt . sum) $ codiagonals_seq $ matrix (*) a (conj b)

>nonzero_permutation :: (Num a, Eq a) => Stream a -> (Stream a, Stream Integer)
>nonzero_permutation (Pre 0 r) = (Pre x (Pre 0 r'),
>                                 Pre (xp+1) (Pre 0 $ fmap (+1) rp))
>   where ~(Pre x r', Pre xp rp) = nonzero_permutation r
>nonzero_permutation z = (z, naturals)

>prefix_matrix :: a -> Stream a -> Stream a -> (Stream :*: Stream) a -> (Stream :*: Stream) a
>prefix_matrix x row col rest =
>   stream_matrix_impl (Pre x $ stream_diagonal_impl rest) $
>                      (Pre (row,col) $ stream_codiagonal_impl rest)

>compose_permutation :: Stream Integer -> Stream Integer -> Stream Integer
>compose_permutation (Pre i r) s = Pre (i `streamindex` s) $ compose_permutation r s

>permutation_matrix_from_indices :: (Num a) => Stream Integer -> (Stream :*: Stream) a
>permutation_matrix_from_indices indices = Matrix $ fmap (\i ->
>    prefix (take i (constant 0)) $ Pre 1 $ constant 0) indices

>-- | <https://en.wikipedia.org/wiki/LU_decomposition>
>-- This has some bug still, l_matrix computation gets stuck.
>lu_decomposition_stream_matrix :: (Floating a, Eq a)
> => (Stream :*: Stream) a -> ((Stream :*: Stream) a, (Stream :*: Stream) a, Stream Integer)
>lu_decomposition_stream_matrix a = (l_matrix,u_matrix, perm `compose_permutation` rest_p_perm)
>  where (dd,(xx,y),m) = dematrix_impl a
>        ((Pre d x), perm) = nonzero_permutation (Pre dd y)
>        dinv = 1/d
>        diag1 = Pre 1 $ diagonal_impl rest_l_matrix
>        l_matrix = stream_matrix_impl diag1 $
>                Pre (constant 0, dinv %* x) $
>                 stream_codiagonal_impl rest_l_matrix
>        diag2 = Pre d $ diagonal_impl rest_u_matrix
>        u_matrix = stream_matrix_impl diag2 $
>                    Pre (xx,constant 0) $
>                     stream_codiagonal_impl rest_u_matrix
>        rest = liftA2 (-) m ((dinv %* x) `tensor_product` xx)
>        (rest_l_matrix,rest_u_matrix, rest_p_perm) = lu_decomposition_stream_matrix rest

>instance (ConjugateSymmetric a) => ConjugateSymmetric ((Stream :*: Stream) a) where
>   conj = fmap conj

>instance (Closed a, Num a,ConjugateSymmetric a) => ConjugateSymmetric (Stream a :-> Stream a) where
>   conj x = linear $ fmap conj $ fromLinear x

>instance (Closed a, Fractional a, ConjugateSymmetric a) => Num (Stream a :-> Stream a) where
>   x + y = linear $ liftA2 (+) (fromLinear x) (fromLinear y)
>   x - y = linear $ liftA2 (-) (fromLinear x) (fromLinear y)
>   (*) = matrix_convolution_product_linear
>   negate x = linear $ fmap negate $ fromLinear x
>   abs x = linear $ fmap abs $ fromLinear x
>   signum x = linear $ fmap signum $ fromLinear x
>   fromInteger i = diagonal_matrix (fromInteger i)

and_convolution :: Stream Bool -> Stream Bool -> Stream Bool
and_convolution x y = fmap or $ codiagonals_seq $ linmatrix (bilinear (&&)) (x, y)

>min_convolution :: (Num a, Ord a) => Stream a -> Stream a -> Stream a
>min_convolution x y = fmap (foldr max 0) $ codiagonals_seq $ matrix min x y

>and_stream :: Stream Bool -> Stream Bool -> Stream Bool
>and_stream x y = do { b <- x ; c <- y ; return (b && c) }

>or_stream :: Stream Bool -> Stream Bool -> Stream Bool
>or_stream x y = do { b <- x ; c <- y ; return (b || c) }

>fromNum :: (Num a) => a -> Stream a
>fromNum i = Pre i zero

>cycleQueueBy :: (a -> b -> b) -> Queue a -> b
>cycleQueueBy f q = case Q.dequeue q of
>                     (Just (v,q')) -> f v (cycleQueueBy f $ Q.enqueue v q')
>                     Nothing -> error "Cannot cycle empty queue"

>cycleQueue :: Queue a -> Stream a
>cycleQueue = cycleQueueBy Pre

>convolve_with :: (Num a) => ([a] -> b) -> (c -> d -> a) -> Stream c -> Stream d -> Stream b
>convolve_with f g x y = fmap f $ codiagonals $ matrix g x y

>convolve_with_seq :: (Num a) => (Seq.Seq a -> b) -> (c -> d -> a) -> Stream c -> Stream d -> Stream b
>convolve_with_seq f g x y = fmap f $ codiagonals_seq $ matrix g x y

>data StreamIndex a = StreamIndex { sindex_position :: Integer, sindex_element :: a }

>instance Comonad StreamIndex where
>   extract (StreamIndex _ x) = x
>   duplicate (StreamIndex i x) = StreamIndex i (StreamIndex i x)
>   extend f z'@(StreamIndex i _) = StreamIndex i (f z')

>instance Functor StreamIndex where
>   fmap f (StreamIndex i x) = StreamIndex i (f x)

>instance Adjunction StreamIndex Stream where
>   unit t = liftA2 StreamIndex naturals (constant t)
>   counit (StreamIndex i s) = i `streamindex` s

>instance (Show a) => Show (StreamIndex a) where
>   show (StreamIndex i x) = show i ++ ":" ++ show x

>-- | this instance of Ord goes to infinite loop if the compared streams are
>-- equal.
>instance (Ord a) => Ord (Stream a) where
>   compare ~(Pre x xr) ~(Pre y yr) = case compare x y of
>                                     LT -> LT
>                                     GT -> GT
>                                     EQ -> compare xr yr

>instance (Enum a, Num a) => Enum (Stream a) where
>   succ ~(Pre x xr) = Pre (succ x) xr
>   pred ~(Pre x xr) = Pre (pred x) xr
>   toEnum i = Pre (toEnum i) zero
>   fromEnum ~(Pre x _) = fromEnum x
>   enumFrom p = p : fmap succ (enumFrom p)
>   enumFromThen p q = p : fmap (+ diff) (enumFromThen p q)
>     where diff = q - p
>   enumFromTo p q = unzipList $ liftA2 enumFromTo p q

> --   enumFromThen p q = p : fmap (+ diff) (enumFromThen p q)
> --     where diff = fmap P.subtract q p
>   -- TODO: missing functions

>instance (Num a, Ord a, Real a) => Real (Stream a) where
>   toRational ~(Pre x _) = toRational x

>-- | Integral instance is based on interpretation of
>-- a stream as generating function.
>instance (Integral a) => Integral (Stream a) where
>   quot x y = x * quotient_invert y
>   div x y = x * inversion y
>   rem x y = x - ((x * inversion y)*y)
>   quotRem x y = (x*quotient_invert y, x - ((x*quotient_invert y)*y))
>   toInteger = toInteger . shead

>-- | Fractional instance is based on interpretation of
>-- a stream as generating function.
>instance (Fractional a) => Fractional (Stream a) where
>   x / y = x * reciprocal y
>   recip = reciprocal
>   fromRational r = fromInteger (numerator r) * reciprocal (fromInteger (denominator r))


>-- | fold over streams
>streamfold :: (a -> b -> b) -> Fold (Stream a) b
>streamfold f = let x = StreamFold f x in x

>fold_codiagonals :: (Num a) => ([a] -> b -> b) -> (Stream :*: Stream) a -> b
>fold_codiagonals f = visit (streamfold f) . codiagonals

>streamfold2 :: (a -> c -> b) -> (a -> b -> c) -> Fold (Stream a) b
>streamfold2 f g = let x = StreamFold f (StreamFold g x) in x

>streamfold_lst :: [a -> b -> b] -> Fold (Stream a) b
>streamfold_lst lst = let x = foldr StreamFold x lst in x

>streamfold_stream :: Stream (a -> b -> b) -> Fold (Stream a) b
>streamfold_stream ~(Pre x xr) = StreamFold x (streamfold_stream xr)

>rotate_prefix :: Integer -> Stream a -> Stream a
>rotate_prefix k ~(Pre c cr) = (take k cr ++ [c]) `prefix` drop k cr

>-- | the function studied in the Collatz conjecture.
>threeNplusOne :: Integer -> Integer
>threeNplusOne n | n == 1 = 1
>                | odd n  = 3 * n + 1
>                | otherwise = n `div` 2

>collatz :: Integer -> [Integer]
>collatz s = takeWhile (/= 1) $ iterate_stream threeNplusOne s

>toNatSeq :: Stream a -> Rec Maybe -> a
>toNatSeq ~(Pre x _) (In Nothing) = x
>toNatSeq ~(Pre _ xr) (In (Just y)) = toNatSeq xr y

>fromIntegerSeq :: (Integer -> a) -> Stream a
>fromIntegerSeq f = fmap f naturals

>-- | \[[sumSeq(f)(n)]_j = \sum_{i=n}^{n+j+1}f(i)\]
>sumSeq :: (Num b, Num a, Ord a) => (a -> b) -> a -> Stream b
>sumSeq f i = sum_stream $ fmap f $ naturals_starting_from i

>boolValue :: (Num a) => Bool -> a
>boolValue b = if b then 1 else 0

>fromBoolSeq :: (Num a) => (Integer -> Bool) -> Stream a
>fromBoolSeq f = fromIntegerSeq (boolValue . f)

>streamIndexFold :: Fold (Stream a) (Integer -> a)
>streamIndexFold = streamfold (\ x r i -> if i == 0 then x else r (i-1))

>streamindex :: Integer -> Stream a -> a
>streamindex = flip (visit streamIndexFold)

>streamindex2 :: (Integer,Integer) -> (Stream :*: Stream) a -> a
>streamindex2 (a,b) m = m <!> (streamindex a, streamindex b)
  
>map_indices :: (Integer -> Integer) -> Stream a -> Stream a
>map_indices f s = fromIntegerSeq ((`streamindex` s) . f)

>limit_fold :: (a -> a -> a) -> Fold (Stream a) (Integer -> a)
>limit_fold f = streamfold $
>   \ x r i -> if i > 0 then f x (r (i-1)) else x

>limit_fold_gen :: (a -> b -> b) -> (a -> b) -> Fold (Stream a) (Integer -> b)
>limit_fold_gen f g = streamfold $
>   \ x r i -> if i > 0 then f x (r (i-1)) else g x


>stream_differences :: (Num a) => Stream a -> Stream a
>stream_differences ~(Pre x ~z'@(Pre y _)) = 
>        Pre (y - x) $ stream_differences z'

>stream_quotients :: (Fractional a) => Stream a -> Stream a
>stream_quotients ~(Pre x ~z'@(Pre y _)) = Pre (y/x) $ stream_quotients z'

>fixpoint :: (Limiting str a) => (a -> a) -> a -> Closure str a
>fixpoint f x = limit $ iterate_stream f x

matrix_inverse :: (InnerProductSpace (f a), InnerProductSpace (h a),
                  Transposable f h a, Transposable h f a,
                  VectorSpace ((h :*: f) a), Num (f a),
                   SupportsMatrixMultiplication f h f a,
                   SupportsMatrixMultiplication h f h a,
                  VectorSpace (h a), VectorSpace (f a),
                  Scalar (h (h a)) ~ a,
                  Num a)
 => f a :-> h a -> h a :-> f a -> Stream (h a :-> f a)

>matrix_inverse :: (
>                         LinearTransform (f2 :*: g1) (f2 :*: g1) (g2 a),
>                         TArrow.FunctorArrow (f2 :*: g1) LinearMap,
>                         InnerProductSpace (f2 (g2 a)), InnerProductSpace (g1 (g2 a)),
>                         Transposable f2 g1 (g2 a), Transposable g1 f2 (g2 a),
>                         Linearizable LinearMap (:*:) g1 f2 (g2 a),
>                         Linearizable LinearMap (:*:) f2 g1 (g2 a),
>                         Linearizable LinearMap (:*:) f2 g1 (g2 a),
>                         VectorSpace ((:*:) f2 g1 (g2 a)), Scalar (f2 (g2 a)) ~ g2 a,
>                         Scalar (g1 (g2 a)) ~ g2 a, Linearizable LinearMap (:*:) g2 g2 a,
>                         VectorSpace ((:*:) g2 g2 a), Diagonalizable g2 a
> ) =>
>    (g1 (g2 a)) :-> (f2 (g2 a))
>  -> (f2 (g2 a)) :-> (g1 (g2 a))
>  -> Stream ((f2 (g2 a)) :-> (g1 (g2 a)))
>matrix_inverse a b = fmap linear $ iterate_stream (\xk -> (TArrow.amap timestwo -!< xk) %- xk %*% a' %*% xk) (fromLinear b)
>   where timestwo = linear (2 %* identity)
>         a' = fromLinear a
> 
>stream_min :: (Ord a) => Fold (Stream a) (Integer -> a)
>stream_min = limit_fold min
>
>stream_max :: (Ord a) => Fold (Stream a) (Integer -> a)
>stream_max = limit_fold max
>
>liftStream2 :: (a -> a -> a) -> Stream a -> Stream a
>liftStream2 f ~(Pre x xr) = Pre x $ fmap (f x) (liftStream2 f xr)

>min_stream :: (Ord a) => Stream a -> Stream a
>min_stream = liftStream2 min

>max_stream :: (Ord a) => Stream a -> Stream a
>max_stream = liftStream2 max

>stream_product :: (Num a) => Fold (Stream a) (Integer -> a)
>stream_product = limit_fold (*)

>stream_sum :: (Num a) => Fold (Stream a) (Integer -> a)
>stream_sum = limit_fold (+)

>-- | product_stream produce from a stream \([a_0,a_1,a_2,...]\) a stream
>-- \([a_0,a_0a_1,a_0a_1a_2,a_0a_1a_2a_3,...]\)
>product_stream :: (Num a) => Stream a -> Stream a
>product_stream = liftStream2 (*)

>-- | sum_stream will produce from a stream \([a_0,a_1,a_2,...]\) a stream
>-- \([a_0,a_0+a_1,a_0+a_1+a_2,...]\).

>sum_stream :: (Num a) => Stream a -> Stream a
>sum_stream = liftStream2 (+)

>vsum_stream :: (VectorSpace a) => Stream a -> Stream a
>vsum_stream = liftStream2 (%+)

>sum_stream_integral :: (Integral a) => Stream a -> Stream a
>sum_stream_integral s = s `div` (1 - s_z)

>sum_stream_fractional :: (Fractional a) => Stream a -> Stream a
>sum_stream_fractional s = s / (1 - s_z)

>-- | if 'f' in cobind is a fixed-point free function, then this will find a
>-- stream that is not an element of any of the streams.

>cobind :: (ConjugateSymmetric a, Num a, Closed a) => (a -> b) -> Stream a :-> Stream a -> Stream b
>cobind f = fmap f . stream_diagonal

>indexed_lookup :: Stream Integer -> Stream a -> Stream a
>indexed_lookup ~(Pre i x) s = Pre y (indexed_lookup x yr)
>   where ~(Pre y yr) = drop i s 

universal :: Stream Bool :-> Stream Bool -> Stream Bool
universal = fmap and . codiagonals_seq

existential :: Stream Bool :-> Stream Bool -> Stream Bool
existential = fmap or . codiagonals_seq

>topleft :: (ConjugateSymmetric a,Num a, Closed a) => Stream a :-> Stream a -> a
>topleft = shead . stream_diagonal

>-- | Everything but a diagonal

>stream_codiagonal :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a -> Stream (Stream a, Stream a)
>stream_codiagonal x | z <- fromLinear x = stream_codiagonal_impl z


>zero_codiagonal :: (Num a) => Codiagonal Stream a
>zero_codiagonal = CodiagonalStream $ constant (constant 0, constant 0)

>stream_diagonal_matrix :: (Num a) => Stream a -> (Stream :*: Stream) a
>stream_diagonal_matrix m = m |\| zero_codiagonal

>-- | stream_matrix creates matrix from a diagonal and a codiagonal

>stream_matrix :: (ConjugateSymmetric a, Num a, Closed a) => Stream a -> Stream (Stream a, Stream a) -> Stream a :-> Stream a
>stream_matrix x y = linear $ stream_matrix_impl x y

>-- | this surprising function builds a two-dimensional infinite matrix
>-- from a diagonal, an upper triangular matrix and a lower triangular matrix.
>-- Notice the input matrices have no zero entries, rather indices for
>-- the triangular matrices are bijectively
>-- mapped to indices of the result matrix.
>-- This is only possible for infinite matrices.
>-- It's even more surprising once it's realised that two-dimensional
>-- infinite matrices can be bijectively described as an infinite binary tree
>-- of one-dimensional streams.
>from_triangular_matrices :: (Num a) => Stream a -> (Stream :*: Stream) a -> (Stream :*: Stream) a -> (Stream :*: Stream) a
>from_triangular_matrices diag upper lower =
>  stream_matrix_impl diag (liftA2 (,) (cells upper) (cells lower))

>-- | isomorphism of infinite matrices that splits along diagonal.
>triangular_matrix_iso :: (Num a) => (Stream :*: Stream) a :==: ((Stream :*: Stream) a, Stream a, (Stream :*: Stream) a)
>triangular_matrix_iso = fwd TArrow.<-> bck
>  where -- fwd :: (Stream :*: Stream) a -> ((Stream :*: Stream) a, Stream a, (Stream :*: Stream) a)
>        fwd m = (lower_triangle_impl m, diagonal_impl m, upper_triangle_impl m)
>        bck (l,d,u) = from_triangular_matrices d u l

>first_row_iso :: (Stream :*: Stream) a :==: (Stream a, (Stream :*: Stream) a)
>first_row_iso = fwd TArrow.<-> bck
>  where fwd (Matrix m) = (fmap shead m, Matrix $ fmap stail m)
>        bck (s,Matrix m) = Matrix $ liftA2 Pre s m

>first_col_iso :: (Stream :*: Stream) a :==: (Stream a, (Stream :*: Stream) a)
>first_col_iso = fwd TArrow.<-> bck
>   where fwd (Matrix m) = (shead m, Matrix $ stail m)
>         bck (s,Matrix m) = Matrix $ Pre s m

>-- | this matrix contains 0 on diagonal, 1 on diagonal of upper and lower
>-- triangular matrices and so on. Every element of the two-dimensional matrix
>-- gets an index.
>triangular_order_matrix :: (Num a) => (Stream :*: Stream) a 
>triangular_order_matrix = from_triangular_matrices (constant 0) submatrix submatrix
>   where submatrix = fmap (+1) triangular_order_matrix

triangular_inverted_matrix :: (Num a) => Stream a :-> Stream a
triangular_inverted_matrix = from_triangular_matrices (constant 0) submatrix (TArrow.amap negate submatrix)
   where submatrix = TArrow.amap (+1) triangular_inverted_matrix

triangular_pair_matrix :: (Num a) => Stream (a,a) :-> Stream (a,a)
triangular_pair_matrix = from_triangular_matrices (constant (0,0))
   (linear $ fmap (first (+1)) $ fromLinear triangular_pair_matrix)
   (linear $ fmap (second (+1)) $ fromLinear triangular_pair_matrix)

>prefix_stream_matrix :: (ConjugateSymmetric a, Num a, Closed a) => a -> (Stream a, Stream a) -> Stream a :-> Stream a -> Stream a :-> Stream a
>prefix_stream_matrix x (row,col) m = linear $ Matrix $ 
>       Pre (Pre x row) $ liftA2 Pre col (cells $ fromLinear m)

>pseudo_codiagonal :: (Stream :*: Stream) a -> Codiagonal Stream a
>pseudo_codiagonal (Matrix ~(Pre ~(Pre _ x)
>                       ~(Pre y yr))) = CodiagonalStream $
>                             Pre (x,y) (codiagonal_substreams $ pseudo_codiagonal $ Matrix yr)

>pseudo_matrix :: Stream a -> Stream (Stream a, Stream a) -> (Stream :*: Stream) a
>pseudo_matrix ~(Pre a ar) ~(Pre ~(x,y) yr) = Matrix $ Pre (Pre a x) (Pre y (cells $ pseudo_matrix ar yr))

>map_matrix :: (Num a, Num b) => (a -> b) 
>           -> ((Stream a,Stream a) -> (Stream b, Stream b))
>           -> (Stream :*: Stream)a -> (Stream :*: Stream) b
>map_matrix f g s = stream_matrix_impl (fmap f y) (fmap g x)
>   where x = stream_codiagonal_impl s
>         y = stream_diagonal_impl s

>map_diagonal :: (Num a) => (a -> a) -> (Stream :*: Stream) a -> (Stream :*: Stream) a
>map_diagonal f = map_matrix f id

>joinWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
>joinWith f x y = cojoin $ matrix f x y

>shead_index :: Stream a :==: I a
>shead_index = (I . shead) TArrow.<-> (constant . unI)

>stail_index :: (Num a) => Stream a :==: I a -> Stream a :==: I a
>stail_index ind = (I . index_project ind . stail) TArrow.<-> (\ (I a) -> Pre a (isomorphism_section ind (I a)))

>instance (Num a) => Indexable Stream a where
>   diagonal_projections = Pre shead_index $ fmap stail_index diagonal_projections
>   indexable_indices = fmap fromIntegral (naturals :: Stream Integer)

>zipList :: ([a] -> b) -> [Stream a] -> Stream b
>zipList f lst = Pre (f $ map shead lst) $ zipList f $ map stail lst

>sequence :: [Stream a] -> Stream [a]
>sequence = zipList id

>unzipList :: Stream [a] -> [Stream a]
>unzipList z' = fmaphead z' : unzipList (fmaptail z')
>  where fmaphead (Pre (c:_) xr) = Pre c (fmaphead xr)
>        fmaphead (Pre []     xr) = fmaphead xr
>        fmaptail (Pre (_:cr) xr) = Pre cr (fmaptail xr)
>        fmaptail (Pre []     xr) = fmaptail xr
  
>zipStream :: (Functor f) => (f b -> a) -> f (Stream b) -> Stream a
>zipStream f inp = Pre (f (fmap shead inp)) (zipStream f (fmap stail inp))

>sequenceStream :: (Functor f) => f (Stream a) -> Stream (f a)
>sequenceStream = zipStream id

>apply3 :: Stream (a -> b -> c) -> Stream a -> Stream b -> Stream c
>apply3 = liftA3 ($)

>applyA :: (Arrow arr) => Stream (arr a b) -> arr (Stream a) (Stream b)
>applyA ~(Pre f fr) = proc (Pre y yr) -> do
>                      y' <- f -< y
>                      yr' <- applyA fr -< yr
>                      returnA -< Pre y' yr'

>dematrix :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a
>               -> (a, (Stream a, Stream a), Stream a :-> Stream a)
>dematrix y = let (d,zz,res) = dematrix_impl (fromLinear y)
>              in (d,zz,linear res)

>dematrix_impl :: (Stream :*: Stream) a -> (a, (Stream a, Stream a), (Stream :*: Stream) a)
>dematrix_impl y = (d,zz,stream_matrix_impl dr cr)
>  where ~(Pre d  dr) = stream_diagonal_impl y
>        ~(Pre zz cr) = stream_codiagonal_impl y

>codiagonals_linear :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a -> Stream [a]
>codiagonals_linear s = fmap Data.Foldable.toList $ codiagonals_seq $ fromLinear s

>-- | codiagonals operation implemented natively on lists.
>-- This is not efficient due to use of list append (++) operation,
>-- which is linear-time operation. Use 'codiagonals' function instead.
>codiagonals_list :: (Num a) => (Stream :*: Stream) a -> Stream [a]
>codiagonals_list q = Pre [d] $ Pre [x,y] $ liftA3 f xr yr $ codiagonals_list m
>    where f pr suf r = pr : (r ++ [suf])
>          ~(d,~(~(Pre x xr), ~(Pre y yr)),m) = dematrix_impl q
>          

>-- | This is faster than the version for lists, because
>-- the suffix computation on Seq is constant time rather than linear.
>-- However, strictness seems to behave differently.
>codiagonals_seq :: (Stream :*: Stream) a -> Stream (Seq.Seq a)
>codiagonals_seq q = Pre (Seq.singleton d) $ Pre (Seq.singleton x Seq.|> y) $
>                      liftA3 f xr yr $ codiagonals_seq m
>       where f pr suf r = (pr Seq.<| r) Seq.|> suf
>             ~(d,~(~(Pre x xr), ~(Pre y yr)),m) = dematrix_impl q

codiagonals_seq_linear :: (Num a, Closed a) => Stream a :-> Stream a -> Stream a :-> Seq.Seq a
codiagonals_seq_linear = linear . Matrix . codiagonals_seq . fromLinear

>-- | The 'codiagonals' function will divide a two-dimensional
>-- stream of streams (where elements are \(e_{(i,j)}\) into a stream of
>-- lists
>-- 
>--   \(l_k\) where \(l_k = [e_{(i,j)} | i \leftarrow naturals, j \leftarrow naturals, i+j = k]\)
>-- 
>-- The indices in the streams are closely related to "Cantor pairing
>-- function" (which is a bijection)
>--
>-- @
>-- f :: (N,N) -> N
>-- f(x,y) = (x+y)(x+y+1)/2 + x
>-- @
>--  
>-- Note that the list at each element of the resulting stream is of
>-- finite (but increasing) size. The order in each list is such that the
>-- diagonal elements are at the center of (the every second) list.
>-- 
>-- It is possible to think of this as "breadth-first" traversal
>-- over two-dimensional streams.
>--
>-- @
>--  d   x xr0 xr1  xr2 ..
>--  y   d' x' xr0' xr1' ..
>--  yr0 y'
>--  yr1 yr0'
>--  yr2 yr1'
>--  ..
>-- @
>--
>-- There is symmetry between primed and unprimed (e.g. d and d') towards
>-- the diagonal.
>-- 
>-- The resulting lists would be: 
>--  @[d], [x,y], [xr0,d',yr0], [xr1,x',y',yr1],...@
>codiagonals :: (Stream :*: Stream) a -> Stream [a]
>codiagonals x = fmap Data.Foldable.toList $ codiagonals_seq x

>-- | for uncodiagonals, the i'th list element of the input stream must have length 'i'.
>-- 
>-- @uncodiagonals ([a] \`Pre\` [b,c] \`Pre\` [d,e,f] ...)@
>--  == @Pre (a \`Pre\` c \`Pre\` f ...) $
>--     Pre (b \`Pre\` e \`Pre\` ...) $
>--     Pre (d \`Pre\` ...  ) $
>--     ...@
>--
>-- <https://en.wikipedia.org/wiki/Formal_power_series>                        
>uncodiagonals :: (ConjugateSymmetric a, Closed a, Num a) => Stream [a] -> Stream a :-> Stream a
>uncodiagonals (Pre [d] (Pre [x,y] re)) = stream_matrix diag codiag
> where codiaghead = (x `Pre` fmap head re,y `Pre` fmap last re)
>       codiag = codiaghead `Pre` stream_codiagonal next
>       diag = d `Pre` stream_diagonal next
>       next = uncodiagonals $ fmap (\lst -> P.take (length lst - 2) (tail lst)) re
>uncodiagonals _ = error "form requirements for input to uncodiagonals not satisfied."

>uncodiagonals_impl :: (Num a) => Stream [a] -> (Stream :*: Stream) a
>uncodiagonals_impl (Pre [d] (Pre [x,y] re)) = stream_matrix_impl diag codiag
> where codiaghead = (x `Pre` fmap head re,y `Pre` fmap last re)
>       codiag = codiaghead `Pre` stream_codiagonal_impl next
>       diag = d `Pre` stream_diagonal_impl next
>       next = uncodiagonals_impl $ fmap (\lst -> P.take (length lst - 2) (tail lst)) re
>uncodiagonals_impl _ = error "form requirements for input to uncodiagonals not satisfied."

>takes :: Stream Integer -> Stream a -> Stream [a]
>takes ~(Pre x xr) s = Pre (take x s) $ takes xr (drop x s)

>split_dimension :: (ConjugateSymmetric a, Num a, Closed a) => Stream a -> Stream a :-> Stream a
>split_dimension = uncodiagonals . takes nonzero_naturals

>split_planar :: (ConjugateSymmetric a, Num a, Closed a) => Stream a -> Stream a :-> Stream a
>split_planar = uncodiagonals . fmap (\lst -> if odd (length lst) then lst else reverse lst) . takes nonzero_naturals


>codiagonalsIso :: (ConjugateSymmetric a, Num a, Closed a, TArrow.BiArrow arr) => arr (Stream a :-> Stream a) (Stream [a])
>codiagonalsIso = codiagonals_linear TArrow.<-> uncodiagonals

codiagonalsIsoMatrix :: (Closed a, Num a, TArrow.BiArrow arr) => arr (Stream a :-> Stream a) (Stream a :-> [a])
codiagonalsIsoMatrix = codiagonalsIso >>> ((linear . Matrix) TArrow.<-> cells_linear)

>-- | lower_triangle takes lower half of a two-dimensional stream split at diagonal.
>lower_triangle :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a -> Stream a :-> Stream a
>lower_triangle = uncodiagonals . liftA2 take nonzero_naturals . cells . fromLinear

>lower_triangle_impl :: (Num a) => (Stream :*: Stream) a -> (Stream :*: Stream) a
>lower_triangle_impl = uncodiagonals_impl . liftA2 take nonzero_naturals . cells

>-- | upper_triangle takes upper half of a two-dimensional stream split at diagonal.
>upper_triangle :: (ConjugateSymmetric a, Num a, Closed a) => Stream a :-> Stream a -> Stream a :-> Stream a
>upper_triangle = lower_triangle . transpose

>upper_triangle_impl :: (Num a) => (Stream :*: Stream) a -> (Stream :*: Stream) a
>upper_triangle_impl = lower_triangle_impl . transpose_impl

>pairing_matrix :: (Fractional a) => (Stream :*: Stream) a
>pairing_matrix = Matrix $ (s_z+s_z2)*(s_z+s_z2+1)/2+s_z

problem: Num [a]
codiagonals3 :: (Num a, Closed a) => Stream (Stream a) :-> (Stream a :-> Stream a) -> Stream [[a]]
codiagonals3 = codiagonals . linear . Matrix . fmap codiagonals . cells_linear

>-- | concatenate a stream of lists to a stream of elements
>concatenate :: Stream [a] -> Stream a
>concatenate (Pre [] xr) = concatenate xr
>concatenate (Pre (c:cr) xr) = Pre c (concatenate (Pre cr xr))

>diagonalize :: Stream [a] -> Stream a
>diagonalize (Pre [] xr) = diagonalize (fmap nice_tail xr)
>diagonalize (Pre (c:_) xr) = Pre c (diagonalize (fmap nice_tail xr))

>skip_even :: Stream a -> Stream a
>skip_even ~(Pre x ~(Pre _ yr)) = Pre x (skip_even yr)

>nice_tail :: [a] -> [a]
>nice_tail [] = []
>nice_tail ~(_:cr) = cr

>diagonal_differences :: (ConjugateSymmetric a, Num a, Closed a) => (a -> b) -> Stream a :-> Stream a -> Stream b
>diagonal_differences neg = fmap neg . stream_diagonal

>find_equal :: (Eq a) => Stream a -> Stream a -> a
>find_equal ~(Pre x xr) ~(Pre y yr) | x == y = x
>                                   | otherwise = find_equal xr yr

>fixedpoint :: (ConjugateSymmetric a, Num a, Closed a, Eq a) => (a -> a) -> Stream a :-> Stream a -> a
>fixedpoint f s = find_equal diag (fmap f diag)
>   where diag = stream_diagonal s

>sum_join :: (Num a) => (Stream :*: Stream) a -> Stream a
>sum_join = fmap sum . codiagonals_seq
>
>sum_bind :: (Num b) => Stream a -> (a -> Stream b) -> Stream b
>sum_bind x f = sum_join $ Matrix $ fmap f x

>-- | cojoin is "better" version of join for streams. cojoin does not
>-- satisfy monad laws (Right identity and associativity do not hold.).
>-- It eventually produces every element of the original two-dimensional
>-- stream. It first produces all elements \(e_{(i,j)}\) where \(i+j=k\) and \(k\)
>-- increasing.

>cojoin :: (Stream :*: Stream) a -> Stream a
>cojoin = concatenate . codiagonals

>-- |
>-- > naturals |*| naturals == (0,0),   [note sum=0]
>-- >                          (0,1),(1,0),  [note sum=1]
>-- >                          (0,2),(2,0),(1,1),  [note sum=2]
>-- >                          (0,3),(3,0),(1,2),(2,1),  [note sum=3]
>-- >                          (0,4),(4,0),(1,3),(3,1),(2,2),  [note sum=4]
>-- >                           ...  
>(|*|) :: (Closed a) => Stream a -> Stream a -> Stream (Vector2 a)
>f |*| g = cojoin $ matrix Vector2 f g

>-- | For a better "bind" operation, there exists (>>!=), which produces
>-- the following result (note duplicates):
>--
>-- >naturals >>!= \x -> naturals >>!= \y -> return (x,y)
>-- >== (0,0),
>-- >  (0,0),(1,0),
>-- >     (0,1),(1,0),(2,0),
>-- >  (0,0),(1,1),(2,0),(3,0),
>-- >    (0,1),(1,0),(2,1),(3,0),(4,0),
>-- >      (0,2),(1,1),(2,0),(3,1),(4,0),(5,0),
>-- >  (0,0),(1,2),(2,1),(3,0),(4,1),(5,0),(6,0),
>-- >    (0,1),(1,0),(2,2),(3,1),(4,0),(5,1),(6,0),(7,0),
>-- >      (0,2),(1,1),(2,0),(3,2),(4,1),(5,0),(6,1),(7,0),(8,0),
>-- >       (0,3),(1,2),(2,1),(3,0),(4,2),(5,1),(6,0),(7,1),(8,0),(9,0),
>-- >  ...

>(>>!=) :: (Num b) => Stream a -> (a -> Stream b) -> Stream b
>m >>!= f = cojoin $ Matrix $ fmap f m

>-- | The (>!=) is like binding, but the results are packaged
>-- to a finite list. Again note duplicates.
>--
>-- 
>-- >naturals >!= \x -> naturals >!= \y -> return (x,y)
>-- >== [[(0,0)]
>-- >  ],
>-- >  [[(0,0),(0,1)],
>-- >   [(1,0)]
>-- >  ],
>-- >  [[(0,0),(0,1),(0,2)],
>-- >   [(1,0),(1,1)],
>-- >   [(2,0)],
>-- >  ],
>-- >  [[(0,0),(0,1),(0,2),(0,3)],
>-- > [(1,0),(1,1),(1,2)],
>-- > [(2,0),(2,1)],
>-- > [(3,0)]
>-- >],
>-- >...
>-- 

>(>!=) :: (Num b) => Stream a -> (a -> Stream b) -> Stream [b]
>m >!= f = codiagonals $ Matrix $ fmap f m

>integers :: (Num a) => Stream a
>integers = interleave_stream naturals (fmap negate nonzero_naturals)

>-- | A stream of integers. This is specialized version of naturals for integers
>integers_stream :: Stream Integer
>integers_stream = naturals

>-- | A stream of increasing numbers starting from 0
>naturals :: (Num a) => Stream a
>naturals = Pre 0 nonzero_naturals

>-- | A stream of increasing numbers starting from a given number.
>naturals_starting_from :: (Num a, Ord a) => a -> Stream a
>naturals_starting_from i = dropWhile (< i) naturals

>-- | A stream of increasing numbers starting from 1
>nonzero_naturals :: (Num a) => Stream a
>nonzero_naturals = fmap (+ 1) naturals

>integral_nonzero_naturals :: (Integral a) => Stream a
>integral_nonzero_naturals = 1 `div` (1 - 2*s_z + s_z*s_z)

>integral_naturals :: (Integral a) => Stream a
>integral_naturals = Pre 0 integral_nonzero_naturals

>-- | This is the variable used in generating functions.  Note that in
>-- multidimensional streams, this is the variable associated with the
>-- largest dimension.

>s_z :: (Num a) => Stream a
>s_z = Pre 0 (Pre 1 0)

>-- | The variable associated with second largest dimension:

>s_z2 :: (Num a) => Stream (Stream a)
>s_z2 = Pre s_z 0

>z2_matrix :: (Num a) => (Stream :*: Stream) a
>z2_matrix = Matrix s_z2

>-- | variable associated with third largest dimension

>s_z3 :: (Num a) => Stream (Stream (Stream a))
>s_z3 = Pre s_z2 0

>z3_matrix :: (Num a) => ((Stream :*: Stream) :*: Stream) a
>z3_matrix = Matrix (Matrix s_z3)

>log_stream :: (Num a) => (Stream :*: Stream) a
>log_stream = stream_powers (s_z-1)

>stream_logarithm :: (Fractional a) => Stream a -> Stream a
>stream_logarithm s = liftA2 (/) s factorial

>stream_log :: (Integral a) => Stream a -> Stream a
>stream_log s = liftA2 div s factorial

>-- | For a stream of terms \(a=[a_0,a_1,a_2,...]\), and \(x=[x_0,x_1,x_2,...]\),
>-- 'approximate_sums a x' computes
>-- \[\sum_{i=0}^n{a_ix_i^i}\], where n is the index to the result stream.

>approximate_sums :: (Fractional a) => Stream a -> Stream a -> Stream a
>approximate_sums gen x = diagonal_impl $ Matrix $ fmap sum_stream $ cells $ matrix (*) gen (index_powers x)

>-- | exponential_stream computes \(s_{i} = {{1}\over{i!}}\).
>-- notice that euler's constant \(e = \sum_{i=0}^{\infty}{1\over{i!}}\).
>exponential_stream :: (Fractional a) => Stream a
>exponential_stream = Pre 1 $ fmap (1/) factorial

>exponential :: (ConjugateSymmetric a, Closed a, Eq a, Fractional a) => Stream a -> Stream a
>exponential = compose exponential_stream

>log_generating_function :: (Fractional a) => Stream a
>log_generating_function = negate $ fmap (1/) nonzero_naturals

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>

>cos_stream :: (Closed a, Fractional a) => Stream a -> Stream a
>cos_stream x = uninterleave_index 1 $ stail $ approximate_sums cos_generating_function x

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>

>sin_stream :: (Closed a, Fractional a) => Stream a -> Stream a
>sin_stream x = uninterleave_index 1 $ approximate_sums sin_generating_function x

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>sin_generating_function :: (Fractional a) => Stream a
>sin_generating_function = stail $ liftA2 (*) (-1 / (1+s_z*s_z)) exponential_stream

>-- | <https://en.wikipedia.org/wiki/Trigonometric_functions>
>cos_generating_function :: (Fractional a) => Stream a 
>cos_generating_function = liftA2 (*) (1 / (1+s_z*s_z)) (stail $ exponential_stream)

>factorial :: (Num a) => Stream a
>factorial = Pre 1 $ Pre 1 $ liftA2 (*) (stail $ nonzero_naturals) (stail factorial)

>-- | both reciprocal and inversion are the inverse of a Cauchy product.
>-- see <http://en.wikipedia.org/wiki/Formal_power_series>
>--
>-- @reciprocal s * s == unit_product@
>reciprocal :: (Fractional a) => Stream a -> Stream a
>reciprocal ~(Pre c cr) = self
>    where self = fmap (/c) $ Pre 1 (negate (cr * self))

>-- | see <http://en.wikipedia.org/wiki/Formal_power_series>
>-- 
>-- @inversion s * s == unit_product@.
>inversion :: (Integral a) => Stream a -> Stream a
>inversion ~(Pre c cr)
>   | c /= 0 = let self = Pre (1 `div` c) (fmap (`div` c) $ negate (cr * self)) in self
>   | otherwise = error "can't invert series that begins with zero"
           
>-- | see <http://en.wikipedia.org/wiki/Formal_power_series>
>quotient_invert :: (Integral a) => Stream a -> Stream a
>quotient_invert ~(Pre c cr)
>   | c /= 0 = let self = (fmap (`quot` c) $ Pre 1 (negate (cr * self))) in self
>   | otherwise = error "can't invert series that begins with zero"

>-- | unit_product is the unit element of (*) for streams.
>unit_product :: (Num a) => Stream a
>unit_product = fromInteger 1

>-- | zero is the unit element of (+) for streams
>zero :: (Num a) => Stream a
>zero = return 0

>stream_if :: Stream Bool -> Stream a -> Stream a -> Stream a
>stream_if = liftA3 (\c x y -> if c then x else y)

>either :: (a -> c) -> (b -> c) -> Stream (Either a b) -> Stream c
>either f g z' = fmap (P.either f g) z'

>split_either :: Stream (Either a b) -> (Stream a, Stream b)
>split_either (Pre (Left x) xr) = let ~(a,b) = split_either xr in (Pre x a,b)
>split_either (Pre (Right y) yr) = let ~(a,b) = split_either yr in (a,Pre y b)

>split_either_bool :: Stream (Either a b) -> (Stream Bool, Stream a,Stream b)
>split_either_bool (Pre (Left x) xr) = let ~(c,a,b) = split_either_bool xr
>                                  in (Pre False c,Pre x a,b)
>split_either_bool (Pre (Right y) xr) = let ~(c,a,b) = split_either_bool xr
>                                   in (Pre True c,a,Pre y b)

>join_either :: Stream Bool -> Stream a -> Stream b -> Stream (Either a b)
>join_either (Pre False r) (Pre x xr) s = let r' = join_either r xr s
>                                          in Pre (Left x) r'
>join_either (Pre True r) s (Pre y yr) = let r' = join_either r s yr
>                                         in Pre (Right y) r'

>subtract :: (Num a) => Stream a -> Stream a -> Stream a
>subtract = liftA2 Prelude.subtract

>-- | add a list as a prefix to a stream
>prefix :: [a] -> Stream a -> Stream a
>prefix lst s = foldr Pre s lst

>-- | The @cycle@ operation is better than @fromList@, if the list is infinite,
>-- because it will ensure the result is infinite (and no Num constraint is needed).
>fromList :: (Num a) => [a] -> Stream a
>fromList lst = prefix lst zero

>toList :: Stream a -> [a]
>toList ~(Pre x xr) = (x:toList xr)

>-- | @filter f s@ for streams does not terminate, if the input stream never
>-- contains any elements @e@ for which @f e == True@.
>filter :: (a -> Bool) -> Stream a -> Stream a
>filter f ~(Pre x xr) | f x = Pre x (filter f xr)
>                     | otherwise = filter f xr

>-- remove removes those elements from the stream that are part of the list
>remove :: (Eq a) => [a] -> Stream a -> Stream a
>remove lst = filter (not . (`elem` lst))

>-- | @interleave_count@ is like @interleave@ except that 'i' elements of each
>-- stream are taken from each stream at each stream.
>interleave_count :: Integer -> Stream a -> Stream a -> Stream a
>interleave_count i x y = prefix lst (interleave_count i y rest)
>    where (lst,rest) = splitAt i x
                                  

>uninterleave_index :: Integer -> Stream a -> Stream a
>uninterleave_index i s = Pre x (uninterleave_index i xr)
>   where ~(Pre x xr) = drop i s

>-- | interleave two streams such that even indexed elements of result are
>-- from first input stream and odd indexed elements of result are from
>-- second stream.
>interleave_stream :: Stream a -> Stream a -> Stream a
>interleave_stream ~(Pre x xr) y = Pre x (interleave_stream y xr)

>instance InterleaveFunctor Stream where
>   interleave = interleave_stream

>-- | version of interleave that produces either instances
>interleave_either :: Stream a -> Stream b -> Stream (Either a b)
>interleave_either ~(Pre x xr) yr = Pre (Left x) (interleave_either_reverse yr xr)
>  where interleave_either_reverse ~(Pre y yr') xr' = Pre (Right y) (interleave_either xr' yr')

>-- | split a stream, elements with even index go to first result stream,
>-- rest to second.
>uninterleave :: Stream a -> (Stream a, Stream a)
>uninterleave ~(Pre x ~(Pre y r)) = (Pre x xr, Pre y yr)
>   where ~(xr,yr) = uninterleave r

>-- | three stream interleave
>interleave3 :: Stream a -> Stream a -> Stream a -> Stream a
>interleave3 ~(Pre x xr) y z' = Pre x (interleave3 y z' xr)

>-- | three stream uninterleave
>uninterleave3 :: Stream a -> (Stream a, Stream a, Stream a)
>uninterleave3 ~(Pre x ~(Pre y ~(Pre z' r))) = (Pre x xr, Pre y yr, Pre z' zr)
>   where ~(xr,yr,zr) = uninterleave3 r

>uninterleave_indices :: Integer -> Integer -> Stream a -> Stream a
>uninterleave_indices i s = map_indices (\x -> i * x + s)

>-- | interleave a non-empty list of streams.
>interleave_lst :: [Stream a] -> Stream a
>interleave_lst (~(Pre x xr):sr) = Pre x (interleave_lst (sr ++ [xr]))
>interleave_lst [] = undefined

>-- | uninterleave to an indicated number of streams.
>uninterleave_lst :: Integer -> Stream a -> [Stream a]
>uninterleave_lst i s = liftA2 Pre lst (uninterleave_lst i rst)
>   where ~(lst,rst) = splitAt i s

>-- | interleave a queue of streams.
>interleave_queue :: (Queue :*: Stream) a -> Stream a
>interleave_queue ~(Matrix q) = case Q.dequeue q of
>     (Just (~(Pre x xr),r)) -> Pre x (interleave_queue (Matrix $ Q.enqueue xr r))
>     Nothing                -> error "interleave_queue: empty queue"

>-- | uninterleave to a queue of streams
>uninterleave_queue :: Integer -> Stream a -> (Queue :*: Stream) a
>uninterleave_queue i s = Matrix $ liftA2 Pre (Q.fromList lst) (cells $ uninterleave_queue i rst)
>       where ~(lst,rst) = splitAt i s

>square :: (Num a) => [[a]] -> (Stream :*: Stream) a
>square = Matrix . fromList . map fromList

>-- | drop a specified number of elements from beginning of a stream.
>drop :: Integer -> Stream a -> Stream a
>drop 0 x = x
>drop n ~(Pre _ xr) = drop (n-1) xr

>-- | take a specified number of elements from the beginning of the stream.
>take :: Integer -> Stream a -> [a]
>take 0 _ = []
>take n ~(Pre x xr) = x : take (n-1) xr

>-- | split a stream from index.
>splitAt :: Integer -> Stream a -> ([a],Stream a)
>splitAt i x = (take i x, drop i x)

>take2d :: (Integer,Integer) -> (Stream :*: Stream) a -> ([] :*: []) a
>take2d (x,y) m = Matrix $ m <!> (take x, take y)

>drop2d :: (Integer,Integer) -> (Stream :*: Stream) a -> (Stream :*: Stream) a
>drop2d (x,y) m = Matrix $ m <!> (drop x, drop y)

>-- | count how many elements from a beginning of a stream satisfy a predicate.
>countWhile :: (a -> Bool) -> Stream a -> Integer
>countWhile f ~(Pre x xr) | f x = succ (countWhile f xr)
>                         | otherwise = 0

>-- | take elements from a stream while predicate is true.
>takeWhile :: (a -> Bool) -> Stream a -> [a]
>takeWhile f ~(Pre x xr) | f x = (x:takeWhile f xr)
>                         | otherwise = []

>-- | drop elements from a stream while predicate is true.
>dropWhile :: (a -> Bool) -> Stream a -> Stream a
>dropWhile f ~z'@(Pre x xr)
> | f x = dropWhile f xr
> | otherwise = z'

>span :: (a -> Bool) -> Stream a -> ([a],Stream a)
>span f ~z'@(Pre x xr) = if f x then (x:c,d) else ([],z')
>    where ~(c,d) = span f xr

>-- | The 'cycle' operation is better than fromList for converting a list to stream,
>-- if the list is infinite, because it will ensure the result is infinite.
>cycle :: [a] -> Stream a
>cycle lst = prefix lst (cycle lst)

>-- | use elements of a queue to produce an infinite stream.
>cycle_queue :: Queue a -> Stream a
>cycle_queue q = maybe (error "empty queue") cycler (Q.dequeue q)
>   where cycler (e,r) = Pre e (cycle_queue (Q.enqueue e r))

>-- | stirling numbers are the numbers obtained from
>-- \(z (z+1) ... (z+n)\)
>stirling_numbers :: (Num a) => (Stream :*: Stream) a
>stirling_numbers = Matrix $ fmap (product . fmap (s_z +)) natural_prefixes

>negative_stirling_numbers :: (Num a) => (Stream :*: Stream) a
>negative_stirling_numbers = Matrix $ fmap (product . fmap (s_z -)) natural_prefixes

>natural_prefixes :: (Num a) => Stream [a]
>natural_prefixes = fmap (`take` naturals) naturals

>toSquare :: (Stream :*: Stream) a -> [[a]]
>toSquare = take 10 . fmap (take 10) . cells

>instance (PpShow a) => Show ((Stream :*: Stream) a) where
>  show x = render $ print_square x

>print_square :: (PpShow a) => (Stream :*: Stream) a -> Pretty.Doc
>print_square (Matrix s) = pp_list $ take 15 $ fmap (pp_list . fmap pp . take 15) s

>-- | stream of powers of a given integer for fractional items
>power :: (Fractional a) => Integer -> Stream a
>power i = 1 / (1 - (fromInteger i * s_z)) 

>-- | stream of powers of a given integer for integral items.
>power_integral :: (Integral a) => Integer -> Stream a
>power_integral i = 1 `div` (1 - fromIntegral i * s_z)

>index_powers :: (Num a) => Stream a -> Stream a
>index_powers a = liftA2 (^) a (naturals :: Stream Integer)

>substitute :: (Num a) => Stream a -> Stream a -> Stream a
>substitute a b = liftA2 (*) a (index_powers b)

>fourier_ :: (RealFloat a) 
>        => Complex a -> Stream (Complex a) -> Stream (Complex a)
>fourier_ w s = substitute s $ constant $ exp (negate (0:+1) * w)

>exp_generating_function :: (ConjugateSymmetric a, Closed a, Floating a, Eq a) => Stream a
>exp_generating_function = s_z*(s_z+1)*exp s_z

>-- | <https://en.wikipedia.org/wiki/Generating_function>.
>-- \[EGF(z \mapsto \sum_{k=0}^{\infty}{s_kz^k}) = z \mapsto \sum_{k=0}^{\infty}{{1\over{k!}}{s_kz^k}}\].
>exponential_generating_function :: (Fractional a) => Stream a -> Stream a
>exponential_generating_function a = liftA2 (/) a factorial

>-- | <https://en.wikipedia.org/wiki/Generating_function>
>poisson_generating_function :: (ConjugateSymmetric a, Closed a, Eq a, Floating a) => Stream a -> Stream a
>poisson_generating_function a = exponential_generating_function $
>   liftA2 (*) (exp (negate s_z)) a

>pre_subst :: (Num a) => Stream a -> Stream a -> Stream [a]
>pre_subst a b = codiagonals $ Matrix $ liftA2 multiply a $ cells $ powers b

>subst :: (Closed a, Num a) => Stream a -> Stream a -> Stream a
>subst a b = fmap sum $ pre_subst a b

>multiply :: (Num a) => a -> Stream a -> Stream a
>multiply x = fmap (*x)

>-- | input stream elements are raised to n'th power.
>powers :: (Num a) => Stream a -> (Stream :*: Stream) a
>powers s = Matrix $ Pre s $ fmap (liftA2 (*) s) $ cells $ powers s

>-- | input element is raised to successive increasing powers
>-- (first element of the stream is \(n^1\))
>cauchy_powers :: (Num a) => a -> Stream a
>cauchy_powers s = Pre s (fmap (s *) $ cauchy_powers s)

>-- | The elements of pascal triangle are binomial coefficients.
>--
>-- <http://en.wikipedia.org/wiki/Binomial_coefficient>
>binomial_coefficient :: (Integral a) => Integer -> Integer -> a
>binomial_coefficient i j = pascal_triangle <!> (streamindex i,streamindex j)

-- Problem: (Closed Rational) =>
generating_sqrt :: Stream Rational -> Stream Rational
generating_sqrt xplus1 = (`subst` x) $ fmap (binomial (1%2)) naturals
   where x = xplus1 - 1


>-- | <http://en.wikipedia.org/wiki/Catalan_number>
>catalan_numbers :: (Integral a) => Stream a
>catalan_numbers = fmap coeff naturals
>   where coeff n = binomial_coefficient (2*n) n 
>                    `div` (fromInteger n + 1)

>-- | <http://en.wikipedia.org/wiki/Catalan_number> of floating elements.
>catalan_numbers_floating :: (ConjugateSymmetric a, Closed a, Eq a,Floating a) => Stream a
>catalan_numbers_floating = 2 / (1 + sqrt(1 - 4*s_z))

>-- | pascal triangle, elements are binomial coefficients.
>-- <https://en.wikipedia.org/wiki/Pascal%27s_triangle>
>-- this expands "down" (leaving zero elements)
>pascal_triangle :: (Integral a) => (Stream :*: Stream) a
>pascal_triangle = Matrix $ 1 `div` (1 - s_z - s_z2*s_z)

>-- | pascal triangle which expands towards the diagonal
>pascal_triangle_diag :: (Integral a) => (Stream :*: Stream) a
>pascal_triangle_diag = Matrix $ 1 `div` (1 - s_z - s_z2)

>pascal_triangle_diag_fractional :: (Fractional a) => (Stream :*: Stream) a
>pascal_triangle_diag_fractional = Matrix $ 1 / (1 - s_z - s_z2)

>binomial_coefficients :: (Integral a) => Stream [a]
>binomial_coefficients = codiagonals $ pascal_triangle_diag

>binomial_coefficients_seq :: (Integral a) => Stream (Seq.Seq a)
>binomial_coefficients_seq = codiagonals_seq $ pascal_triangle_diag

>exp_approx :: (Fractional a) => a -> Stream a
>exp_approx x = fmap (\ ~(j,lst) -> let m = x / fromInteger j in 
>       sum $ map (\ ~(i,c :: Integer) -> fromInteger c * m^^i) lst) $
>       liftA2 (,) naturals $ fmap (List.zip [(0 :: Integer)..]) $ binomial_coefficients

>-- | computes \((a + b)^n\) for all n, using binomial coefficients.
>--   Hint: use 'z' in one of the argument.
>--   @pascal_triangle == Matrix $ polynomial_powers 1 z@
>--   Note: stream_powers can sometimes be a more efficient alternative.
>polynomial_powers :: (Integral a) => a -> a -> Stream a
>polynomial_powers a b = fmap sum coeffs
>   where mapper k ~(a' :: Integer,b' :: Integer) = k * (a ^ a') * (b ^ b')
>         coeffs = codiagonals $ liftA2 mapper pascal_triangle_diag
>                              $ matrix (,) naturals naturals

>polynomial_powers_fractional :: (Fractional a) => a -> a -> Stream a
>polynomial_powers_fractional a b = fmap sum coeffs
>   where mapper k ~(a' :: Integer,b' :: Integer) = k * (a ^^ a') * (b ^^ b')
>         coeffs = codiagonals
>                $ liftA2 mapper pascal_triangle_diag_fractional
>                $ matrix (,) naturals naturals


>-- | a stream of fibonacci numbers,
>-- each element is a sum of two previous elements.
>-- @1,1,2,3,5,8,13,21,34,55,...@
>fib :: (Integral a) => Stream a
>fib = 1 `div` (1 - s_z - s_z*s_z)

>-- | Triangular numbers. <https://en.wikipedia.org/wiki/Generating_function>
>-- @1,3,6,10,15,21,28,36,...@
>triangular_numbers :: (Integral a) => Stream a
>triangular_numbers = 1 `div` (1 - s_z)^(3 :: Int)

>squares :: (Integral a) => Stream a
>squares = s_z*(s_z+1)`div` (1-s_z)^(3 :: Int)

>-- | @1.0,-1.0,1.0,-1.0,...@
>alternating_signs :: (Fractional a) => Stream a
>alternating_signs = 1 / (1 + s_z)

>-- | @1.0,0.0,1.0,0.0,1.0,0.0,...@
>alternating_bits :: (Fractional a) => Stream a
>alternating_bits = 1 / (1 - s_z*s_z)

>alternating_possibly_negative_bits :: (Fractional a) => Stream a
>alternating_possibly_negative_bits = 1 / (1 + s_z*s_z)

>-- | stream of odd integers
>odd_integers :: (Integral a) => Stream a
>odd_integers = filter odd naturals

>odd_integers_ :: (Integral a) => Stream a
>odd_integers_ = 1 `div` (1 - 3*s_z + 4*s_z*s_z `div` (1 + s_z))

>-- | This is an ideal of the set of positive integers, usually denoted \(nZ^+\),
>-- e.g. set of positive integers divisible by 'n'.

>integers_divisible_by :: Integer -> Stream Integer
>integers_divisible_by n = Pre n $ fmap (+n) $ integers_divisible_by n

>positive_residue_class_modulo :: Integer -> Integer -> Stream Integer
>positive_residue_class_modulo m n = Pre (m `mod` n) $ fmap (+n) $ positive_residue_class_modulo m n

>negative_residue_class_modulo :: Integer -> Integer -> Stream Integer
>negative_residue_class_modulo m n = Pre (m `mod` n) $ fmap (\x -> x - n) $ negative_residue_class_modulo m n

>-- | This function produces the equivalence class of
>-- m mod n. The numbers produced by the stream are considered
>-- as equivalent by the equivalence class.

>residue_class_modulo :: Integer -> Integer -> Stream Integer
>residue_class_modulo m n = interleave_stream (positive_residue_class_modulo m n)
>                                      (stail (negative_residue_class_modulo m n))



>-- | peirces_triangle is the number of set partitions.
>-- From Knuth: The art of Computer Programming, Volume 4
>-- "Generating all combinations and partitions", page 64.

>peirces_triangle_func :: (Num a, Ord a) => a -> a -> a
>peirces_triangle_func 0 0 = 1
>peirces_triangle_func 1 1 = 1
>peirces_triangle_func n k | n == k = peirces_triangle_func (n-1) 1
>                          | k > n  = 0
>                          | otherwise = peirces_triangle_func (n-1) k
>                                      + peirces_triangle_func n (k+1)

>-- | peirces_triangle is the number of set partitions.
>-- From Knuth: The art of Computer Programming, Volume 4
>-- "Generating all combinations and partitions", page 64.
>peirces_triangle :: (Num a, Ord a) => (Stream :*: Stream) a
>peirces_triangle = matrix peirces_triangle_func nonzero_naturals nonzero_naturals

>peirces_triangle_list :: (Num a, Ord a) => Stream [a]
>peirces_triangle_list = fmap (takeWhile (/= 0)) $ cells peirces_triangle

>-- | <https://en.wikipedia.org/wiki/Injective_function?wprof=sfla1>
>-- falling factorial powers specify number of injective functions
>-- from domain with specified number of elements to codomain with
>-- specified number of elements.
>-- <https://en.wikipedia.org/wiki/Falling_and_rising_factorials?wprof=sfla1>
>--
>-- Be careful about indices here, could cause off-by-one error!
>falling_factorial_powers_diag :: (ConjugateSymmetric a, Integral a, Closed a) => Stream a :-> Stream a
>falling_factorial_powers_diag = linear $ Matrix $ liftA2 (liftA2 (*)) (fmap constant factorial) (cells pascal_triangle_diag)

>falling_factorial_powers :: (Eq a,Num a) => (Stream :*: Stream) a
>falling_factorial_powers = matrix falling_factorial_power nonzero_naturals nonzero_naturals

>rising_factorial_powers :: (Eq a,Num a) => (Stream :*: Stream) a
>rising_factorial_powers = matrix rising_factorial_power nonzero_naturals nonzero_naturals

>-- | bell numbers <https://en.wikipedia.org/wiki/Bell_number>.
>-- also see Knuth: The Art of Computer Programming, Volume 4.
>bell_numbers :: (Num a, Ord a) => Stream a
>bell_numbers = fmap (\x -> peirces_triangle_func x x) nonzero_naturals

>integer_partitions_with_parts :: (Integral a) => Integer -> Stream a
>integer_partitions_with_parts m = s_z^m `div` (foldr (*) 1 $ map (\a -> 1 - s_z^a) [1..m])

>-- | newton's method of computing square root approximations:
>square_root :: (Integral a) => a -> Stream a
>square_root i = Pre 1 $ fmap (\x -> if x /= 0 then (x + (i `div` x)) `div` 2 else 0) $ square_root i

>fractional_square_root :: (Fractional a, Eq a) => a -> Stream a
>fractional_square_root i = Pre 1 $ fmap (\x -> if x /= 0 then (x + (i / x)) / 2 else 0) $ fractional_square_root i

>-- | This is an implementation of Floyd's cycle-finding algorithm
>-- <http://en.wikipedia.org/wiki/Cycle_detection>.
>detect_cycle :: (Eq a) => Stream a -> [a]
>detect_cycle s = a : takeWhile (/= a) ar
>   where ~(Pre a ar) = find_start (find_rep st st') s
>         st = stail s
>         st' = stail st
>         find_rep ~(Pre x xr) ~z'@(Pre y (Pre _ yr)) 
>            | x /= y = find_rep xr yr
>            | otherwise = z'
>         find_start ~(Pre x xr) ~z'@(Pre y yr) 
>            | x /= y = find_start xr yr
>            | otherwise = z'

>integer_partitions :: (Integral a) => (Stream :*: Stream) a
>integer_partitions = Matrix $ fmap integer_partitions_with_parts nonzero_naturals

>-- | Stream of prime numbers generated using a sieve.
>-- 
>-- <https://wiki.haskell.org/Prime_numbers>
>primes :: (Integral t) => Stream t
>primes = Pre 2 $ Pre 3 $ sieve (stail primes) (drop 2 odd_integers)
>   where sieve ~(Pre p ps) xs = prefix h $ sieve ps $ filter (filt p) t
>              where ~(h,t) = span (< p*p) xs
>         filt p x = x `rem` p /= 0

>-- | derivative calculates the derivative of the generating function.
>-- <https://en.wikipedia.org/wiki/Generating_function>
>-- \[{\rm{derivative}}({\bf{a}})_i = {\bf{b}}_i = (i+1){\bf{a}}_{i+1}\]
>-- \[f(z) = \sum_{i=0}^{\infty}{\bf{a}}_iz^i\]
>-- \[f'(z) = \sum_{i=0}^{\infty}{\bf{b}}_iz^i = {d\over{dz}} \sum_{i=0}^{\infty}{\bf{a}}_iz^i = \sum_{i=0}^{\infty}(i+1){\bf{a}}_{i+1}z^i\]
>derivative :: (Num a) => Stream a -> Stream a
>derivative s = liftA2 (*) nonzero_naturals (stail s)

>subtract_ordered :: (Ord a) => Stream a -> Stream a -> Stream a
>subtract_ordered ~z'@(Pre x xr) ~z''@(Pre y yr)
>   | y > x  = Pre x (subtract_ordered xr z'')
>   | y == x = subtract_ordered xr z''
>   | otherwise = subtract_ordered z' yr
  
>join_ordered :: (Ord a) => Stream a -> Stream a -> Stream a
>join_ordered ~z'@(Pre x xr) ~z''@(Pre y yr) 
>    | x < y = Pre x (join_ordered xr z'')
>    | otherwise = Pre y (join_ordered z' yr)
  

>-- | for two monotone streams, compares whether all elements of the first stream
>-- are also in the second stream. produces 'True' for every element of
>-- the first stream, if that element is part of the second stream.
>-- produces error if the streams are not monotone.
>monotonic_membership :: (Ord a) => Stream a -> Stream a -> Stream Bool
>monotonic_membership ~z1@(Pre x xr) ~zz2@(Pre y yr)
>     | x > shead xr || y > shead yr = fail $ "Not monotonic"
>     | x == y = Pre True (monotonic_membership xr yr)
>     | x <  y = Pre False (monotonic_membership xr zz2)
>     | otherwise = monotonic_membership z1 yr

>-- | Square root algorithm is from
>-- Simpson: Power Series, <http://caps.gsfc.nasa.gov/simpson/ref/series.pdf>

>sqrt_stream :: (Floating a, Closed a) => Stream a -> Stream a
>sqrt_stream ~z'@(Pre x xr) = Pre (sqrt x) $ mprod xr (sqrt_stream z')
> where pprod (a,i) (b,j) = ((i+j)-3*j)*a*b/(2*(i+j)*sqrt x)
>       mprod a b = fmap sum $ codiagonals
>              $ matrix pprod (a <&> nonzero_naturals) (b <&> naturals)

>-- | <https://en.wikipedia.org/wiki/Inverse_trigonometric_functions inverse trigonometric functions>
>-- <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance (ConjugateSymmetric a, Closed a, Eq a,Floating a) => Floating (Stream a) where
>  sqrt = sqrt_stream
>  exp  = exponential
>  log  = stream_logarithm
>  sinh x = (exp x - exp (negate x)) / 2
>  cosh x = (exp x + exp (negate x)) / 2
>  tanh x = sinh x / cosh x
>  asinh x = log (x + sqrt (x*x+1))
>  acosh x = log (x + sqrt (x*x-1))
>  atanh x = log ((1 + x) / (1 - x)) / 2

>stream_epsilon :: (ConjugateSymmetric a, Closed a, Fractional a) => Stream a :-> Stream a
>stream_epsilon = transpose $ linear $ matrix (/) nonzero_naturals nonzero_naturals --(power 2)

>another_epsilon :: (ConjugateSymmetric a, Closed a,Fractional a) => Stream a :-> Stream a
>another_epsilon = transpose $ linear $ matrix (/) (fmap (1/) nonzero_naturals) nonzero_naturals

>-- | derivate a stream function at a particular point.
>stream_derivate :: (Fractional a, Closed a, Infinitesimal str (Stream a)) => 
>                   (Stream a -> Stream a) -> Stream a -> Closure str (Stream a)
>stream_derivate f x = limit $ do
>    dx <- epsilon_stream
>    return $ (f (x + dx) - f x) / dx

>stream_integral :: (Closed b, Infinitesimal str (Stream b), Fractional b, Enum b) => (Stream b -> Stream b) -> (Stream b, Stream b) -> Closure str (Stream b)
>stream_integral f (xa,ya) = limit $ do
>   eps <- epsilon_stream
>   return $ (eps *) $ sum $ fmap f [xa,xa+eps..ya]

>-- | <https://en.wikipedia.org/wiki/Pi>

>stream_pi :: (Infinitesimal str (Stream a),ConjugateSymmetric a, Closed a, Fractional a, Enum a, Eq a, Floating a) => Closure str (Stream a)
>stream_pi = stream_integral (\x -> 1 / sqrt (1 - x*x)) (negate 1,1)

>complex_pi :: (Closed a, RealFloat a) => Stream (Complex a)
>complex_pi = log (fromNum $ negate 1) / fromNum (0 :+ 1)

>-- | <http://en.wikipedia.org/wiki/Twin_prime>

>twin_primes :: (Integral a) => Stream a
>twin_primes = twin_primes_stream_gen 1 primes
>  where twin_primes_stream_gen prev ~(Pre x xr) 
>          | x == prev + 2 = Pre prev (twin_primes_stream_gen x xr)
>          | otherwise     = twin_primes_stream_gen x xr

>-- | <http://en.wikipedia.org/wiki/Formula_for_primes>

>gcd_primes_diff :: (Integral a) => Stream a
>gcd_primes_diff = gcd_primes - s_z*gcd_primes
>gcd_primes :: (Integral a) => Stream a
>gcd_primes = fmap gcd_prime_gen naturals
>gcd_prime_gen :: (Integral a) => a -> a
>gcd_prime_gen 0 = 0
>gcd_prime_gen 1 = 7
>gcd_prime_gen n = p + gcd n p
>    where p = gcd_prime_gen (n - 1)                                            
           
>instance (Limiting str a, Limiting str b) => Limiting str (a,b) where
>  data Closure str (a,b) = PairClosure (Closure str a,Closure str b)
>  limit str = PairClosure (limit a, limit b)
>      where ~(a,b) = funzip str
>  approximations (PairClosure (x,y)) = approximations x <&> approximations y


>instance (Limiting str a, Limiting str b, Limiting str c) => Limiting str (a,b,c) where
>  data Closure str (a,b,c) = TripleClosure (Closure str a, Closure str b, Closure str c)
>  limit str = TripleClosure (limit a, limit b, limit c)
>    where ~(a,b,c) = funzip3 str
>  approximations (TripleClosure (x,y,z')) = liftA3 (,,) (approximations x) (approximations y) (approximations z')
>
>instance (Show (Closure str a), Show (Closure str b), Show (Closure str c)) => Show (Closure str (a,b,c)) where
>   show (TripleClosure (x,y,z')) = "(" ++ show x ++ "," ++ show y
>      ++ "," ++ show z' ++ ")"


instance (Closed a, Closed b, Closed c) => Closed (a,b,c) where
  accumulation_point (TripleClosure (a,b,c)) = (accumulation_point a,
                                                accumulation_point b,
                                                accumulation_point c)

instance (Closed a, Closed b) => Closed (a,b) where
   accumulation_point (PairClosure (s,s')) = (accumulation_point s,
                                              accumulation_point s')

>instance Limiting Stream (a :==: a) where
>  data Closure Stream (a :==: a) = IsoClosure { runIsoClosure :: Stream (a :==: a) }
>  limit ~(Pre x xr) = IsoClosure $ pre x $ fmap (x >>>) $ runIsoClosure $ limit xr
>  approximations = runIsoClosure

>instance Limiting str a => Limiting str (Complex a) where
>  data Closure str (Complex a) = ComplexClosure { runComplexClosure :: Complex (Closure str a) }
>  limit str = ComplexClosure (limit a :+ limit b)
>    where (a,b) = funzip $ fmap (\(a' :+ b') -> (a',b')) str
>  approximations (ComplexClosure (a :+ b)) = liftA2 (:+) (approximations a) (approximations b)

>instance (Show (Closure str a)) => Show (Closure str (Complex a)) where
>   show (ComplexClosure r) = show r

>instance (Limiting Stream a, RealFloat a) => Num (Closure Stream (Complex a)) where
>   x + y = cliftA2 (+) x y
>   x - y = cliftA2 (-) x y
>   x * y = cliftA2 (*) x y
>   negate x = cmap negate x
>   abs x = cmap abs x
>   signum x = cmap signum x
>   fromInteger x = ComplexClosure (limit (constant (fromInteger x))
>                                   :+ limit (constant (fromInteger 0)))

>instance (RealFloat a, Infinitesimal str a) => Infinitesimal str (Complex a) where
>  epsilon_stream = liftA2 (:+) epsilon_stream epsilon_stream

>instance (RealFloat a, Infinitesimal Stream a) => Closed (Complex a) where
>   accumulation_point (ComplexClosure (ca :+ ci))
>     = accumulation_point $ limit $ liftA2 (:+) (approximations ca) (approximations ci)

instance (Num a, Applicative f, Applicative g,
 Traversable f,Traversable g, Limiting str a, Traversable str,
 Infinitesimal str (f a), Infinitesimal str (g a))
 => Infinitesimal str ((f :*: g) a) where
   epsilon_stream = do
     x <- epsilon_stream
     y <- epsilon_stream
     return $ matrix (*) x y

instance (Closed a) => Closed (Complex a) where
   accumulation_point (ComplexClosure (ca :+ ci))
     = accumulation_point ca :+ accumulation_point ci

>instance AppendableVector Vector3 Stream where
>  type (Vector3 :+: Stream) = Stream
>  (Vector3 x y z) |> a = x `Pre` y `Pre` z `Pre` a

>vector_epsilon :: (Infinitesimal Stream a) => Stream (Vector3 a)
>vector_epsilon = epsilon_stream >>!= \x ->
>   epsilon_stream >>!= \y ->
>   epsilon_stream >>!= \z ->
>   return $! Vector3 x y z

>instance (Infinitesimal Stream a, Num a) => Infinitesimal Stream (Stream a) where
>   epsilon_stream = cells $ matrix (*) epsilon_stream epsilon_stream


