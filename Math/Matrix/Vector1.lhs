>{-# LANGUAGE Safe,MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving, FlexibleContexts, TypeOperators, TypeFamilies #-}
>module Math.Matrix.Vector1 where
>import Control.Applicative
>import Data.Monoid hiding (Dual)
>import Data.Complex
>import Math.Tools.I
>import Math.Tools.CoMonad
>import Math.Tools.Median
>import Math.Matrix.Covector
>import Math.Matrix.Matrix
>import Math.Matrix.Interface
>import Math.Matrix.Simple
>import Math.Number.Stream
>import Math.Number.Real

>data Vector1 a = Vector1 { vector_element :: a }
>  deriving (Eq, Ord)

>instance (Num a) => FiniteDimensional (Vector1 a) where
>   finite (Matrix (Covector f)) = Vector1 (f (Covector vector_element))

>instance Foldable Vector1 where
>   foldMap f (Vector1 x) = f x

>vertical :: (Functor v) => v a -> (v :*: Vector1) a
>vertical = Matrix . fmap Vector1

>horizontal :: (Functor v) => v a -> (Vector1 :*: v) a
>horizontal = Matrix . Vector1

>vector1_to_vec1 :: Vector1 a -> OneD -> a
>vector1_to_vec1 (Vector1 x) = svec1 x

http://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation

>bra :: (Functor v, RealFloat a) => v (Complex a) -> (Vector1 :*: v) (Complex a)
>bra = fmap conjugate . horizontal

>ket :: (Functor m) => m a -> (m :*: Vector1) a
>ket = vertical

>(%<|>%) :: (Functor v, RealFloat a, Transposable v Vector1,
>            Scalar (v (Complex a)) ~ Complex a,             
>            InnerProductSpace (v (Complex a))) => 
>           v (Complex a) -> v (Complex a) -> Scalar (v (Complex a))
>(%<|>%) a b = vector_element $ vector_element $ cells $ bra a %*% ket b

>instance (Limiting a) => Limiting (Vector1 a) where
>  data Closure (Vector1 a) = Vector1Closure { unVector1Limit :: Stream (Vector1 a) }
>  limit str = Vector1Closure str
>  approximations (Vector1Closure a) = a 

>instance Indexable Vector1 where
>  diagonal_projections = Vector1 vector_element
>  indexable_indices = Vector1 0

>instance AppendableVector Vector1 Stream where
>  type (Vector1 :+: Stream) = Stream
>  (Vector1 x) |> s = x `Pre` s

>instance (Infinitesimal a) => Infinitesimal (Vector1 a) where
>  epsilon_stream = fmap Vector1 epsilon_stream

>partial_derivate1x :: (Infinitesimal a, Closed a)
>              => Dual (Vector1 a) -> Dual (Vector1 a)
>partial_derivate1x (Covector f) = Covector $ partial_derivate ch f
>   where ch eps (Vector1 x) = Vector1 (x+eps)

>instance (Infinitesimal a, Closed a) => VectorDerivative (Vector1 a) where
>   divergence f = partial_derivate1x (Covector (vector_element . f))
>   grad f z = Vector1 (partial_derivate1x f `bracket` z)
>   curl f z = Vector1 $ partial_derivate1x (Covector (vector_element . f)) `bracket` z

>instance (MedianAlgebra a) => MedianAlgebra (Vector1 a) where
>   med (Vector1 a) (Vector1 b) (Vector1 c) = Vector1 (med a b c)

>type Matrix1 a = (Vector1 :*: Vector1) a

>matrix1 :: a -> Matrix1 a
>matrix1 = Matrix . Vector1 . Vector1

>inVector1 :: (Vector1 a -> Vector1 b) -> a -> b
>inVector1 f x = vector_element (f (Vector1 x))

>inVector1_binary :: (Vector1 a -> Vector1 b -> Vector1 c) -> a -> b -> c
>inVector1_binary f x y = vector_element (f (Vector1 x) (Vector1 y))

>prefix_vector :: (AppendableVector Vector1 n) => a -> n a -> (Vector1 :+: n) a
>prefix_vector d rest = Vector1 d |> rest

>prefix_matrix :: (Applicative n, AppendableVector Vector1 m,
>                  AppendableVector Vector1 n) => 
>                  a -> (m a, n a) -> (n :*: m) a
>                  -> ((Vector1 :+: n) :*: (Vector1 :+: m)) a
>prefix_matrix d (row,col) (Matrix m) = Matrix $ 
>   (d `prefix_vector` row) `prefix_vector` liftA2 prefix_vector col m

>splittable_codiagonal :: (Functor f, Functor (Vector1 :+: f), Functor (Vector1 :+: g),
>                SplittableVector Vector1 f, SplittableVector Vector1 g)
>            => ((Vector1 :+: f) :*: (Vector1 :+: g)) a -> (a,g a, f a, (f :*: g) a)
>splittable_codiagonal m = (a <!> (vector_element,vector_element),vector_element (cells b),
>                           fmap vector_element (cells c), d)
>   where ((a,b),(c,d)) = split_matrix m

>instance (Num a) => Monoid (Vector1 a) where
>  mempty = vzero
>  mappend = (%+)

>instance Monad Vector1 where
>  return x = Vector1 x
>  (Vector1 x) >>= f = f x

>instance (Show (f a)) => Show ((Vector1 :*: f) a) where
>  show (Matrix (Vector1 x)) = "[" ++ show x ++ "]"

>instance (Show a) => Show (Vector1 a) where
>  show (Vector1 x) = "[" ++ show x ++ "]"

>instance Functor Vector1 where
>  fmap f (Vector1 x) = Vector1 (f x)

>instance (Num a) => Num (Vector1 a) where
>  (Vector1 x) + (Vector1 y) = Vector1 (x+y)
>  (Vector1 x) - (Vector1 y) = Vector1 (x-y)
>  (Vector1 x) * (Vector1 y) = Vector1 (x*y)
>  negate (Vector1 x) = Vector1 (negate x)
>  abs (Vector1 x) = Vector1 (abs x)
>  signum (Vector1 x) = Vector1 (signum x)
>  fromInteger x = Vector1 (fromInteger x)

>instance (Fractional a) => Fractional (Vector1 a) where
>  (Vector1 a) / (Vector1 b) = Vector1 (a/b)
>  recip (Vector1 a) = Vector1 (recip a)
>  fromRational r = Vector1 (fromRational r)


>instance Applicative Vector1 where
>  pure x = Vector1 x
>  (Vector1 f) <*> (Vector1 x) = Vector1 (f x)

>diagonal_projections1 :: Vector1 (Vector1 a -> a)
>diagonal_projections1 = Vector1 vector_element

>instance (Num a) => VectorSpace (Vector1 a) where
>  type Scalar (Vector1 a) = a
>  x %* (Vector1 e) = Vector1 (x * e)
>  (Vector1 a) %+ (Vector1 b) = Vector1 (a + b)
>  vzero = Vector1 0
>  vnegate (Vector1 x) = Vector1 (negate x)

>instance (Num a) => NormedSpace (Vector1 a) where
>  norm (Vector1 x) = x

>instance (Num a, ConjugateSymmetric a) => InnerProductSpace (Vector1 a) where
>  (Vector1 x) %. (Vector1 y) = x*conj y

>instance (Num a) => SquareMatrix Vector1 a where
>  diagonal (Matrix (Vector1 (Vector1 x))) = Vector1 x
>  identity = Matrix $ Vector1 (Vector1 1)
>  diagonal_matrix (Vector1 x) = Matrix $ Vector1 $ Vector1 x

>instance (Num a) => FiniteSquareMatrix Vector1 a where
>  determinant (Matrix (Vector1 (Vector1 x))) = x
>  trace (Matrix (Vector1 (Vector1 x))) = x

>instance Transposable Vector1 Vector1 where
>  transpose (Matrix (Vector1 (Vector1 x))) = Matrix $ Vector1 (Vector1 x)

>instance (Num a) => LinearTransform Vector1 Vector1 a where
>  (Vector1 x) <*>> (Matrix (Vector1 (Vector1 y))) = Vector1 (x*y)
>  (Matrix (Vector1 (Vector1 x))) <<*> (Vector1 y) = Vector1 (x*y)

>instance (Num a) => StandardBasis (Vector1 a) where
>  unit_vectors = [Vector1 1]

>instance (Num a) => CoordinateSpace (Vector1 a) where
>  type Coordinate (Vector1 a) = Int
>  index 0 = vector_element
>  listVector [x] = Vector1 x
>  listVector _ = error "Vector1.vector: dimension error"
>  dimension_size _ = 1
>  coordinates _ = [0]

