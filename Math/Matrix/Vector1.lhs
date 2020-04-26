>{-# LANGUAGE Safe,MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving, FlexibleContexts, TypeOperators, TypeFamilies, DeriveGeneric, DeriveDataTypeable #-}
>module Math.Matrix.Vector1 where
>import safe Control.Applicative
>import safe GHC.Generics hiding ((:*:), (:+:))
>import safe Data.Data
>import safe Data.Typeable
>import safe Data.Monoid hiding (Dual)
>import safe Data.Complex
>import safe Data.Sequence (Seq)
>import safe qualified Data.Binary as Bin
>import safe qualified Data.Sequence as Seq
>import Math.Tools.I
>import Math.Tools.CoMonad
>import Math.Tools.Median
>import Math.Tools.Universe
>import Math.Matrix.Covector
>import Math.Matrix.Matrix
>import Math.Matrix.Interface
>import Math.Matrix.Simple
>import Math.Number.Stream
>import Math.Number.Real

>data Vector1 a = Vector1 { vector_element :: a }
>  deriving (Eq, Ord, Typeable, Data, Generic)

>cov1 :: (a ~ Scalar a) => Vector1 (Dual (Vector1 a))
>cov1 = Vector1 (covector $ vector_element)

>instance (a ~ Scalar a) => ProjectionDual Vector1 a where
>   projection_dual = cov1

>instance (Bin.Binary s) => Bin.Binary (Vector1 s) where
>   put (Vector1 x) = Bin.put x
>   get = do { x <- Bin.get ; return (Vector1 x) }

>instance (Universe a) => Universe (Vector1 a) where
>   all_elements = Vector1 <$> all_elements

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector1 a) where
>   conj = fmap conj

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>instance (ConjugateSymmetric a) => ConjugateSymmetric ((Vector1 :*: Vector1) a) where
>   conj = fmap conj . transpose

>instance (Num a, a ~ Scalar a) => FiniteDimensional (Vector1 a) where
>   finite (Matrix (Covector f)) = Vector1 (f (covector $ vector_element))

>instance Foldable Vector1 where
>   foldMap f (Vector1 x) = f x

>instance Traversable Vector1 where
>   traverse f (Vector1 x) = Vector1 <$> f x

>instance ProjectionSpace Vector1 Vector1 where
>   data (Vector1 \\\ Vector1) a = S11Vector
>   project_first = id
>   project_second _ = S11Vector
>   join_vector v _ = v

>instance Functor (Vector1 \\\ Vector1) where
>   fmap _ S11Vector = S11Vector

>project_down :: (Functor (m \\\ m'),
>                ProjectionSpace m m',
>                ProjectionSpace n Vector1)
>               => (m :*: n) a -> (m \\\ m') a
>project_down m = fmap vector_element $ m <!> (project_second, project_first)
 
>project_right :: (ProjectionSpace m Vector1, ProjectionSpace n n')
>              => (m :*: n) a -> (n \\\ n') a
>project_right m = vector_element $ m <!> (project_first, project_second)

>project_diagonal :: (ProjectionSpace m m', ProjectionSpace n n')
>                 => (m :*: n) a -> ((m \\\ m') :*: (n \\\ n')) a
>project_diagonal m = Matrix $ m <!> (project_second, project_second)

>grid :: (Applicative n, Applicative (f \\\ n),
>             ProjectionSpace f n,
>             ProjectionSpace g n',
>             ProjectionSpace g n'')
>            => (n :*: n') a
>            -> (n :*: (g \\\ n')) a
>            -> ((f \\\ n) :*: n'') a
>            -> ((f \\\ n) :*: (g \\\ n'')) a
>            -> (f :*: g) a
>grid (Matrix a) (Matrix right) (Matrix down) (Matrix m) = Matrix $
>   (liftA2 join_vector a right) `join_vector` (liftA2 join_vector down m)


>prefixMatrix :: (Applicative (f \\\ Vector1),
>             ProjectionSpace f Vector1,
>             ProjectionSpace g Vector1)
>            => a
>            -> (f \\\ Vector1) a -> (g \\\ Vector1) a
>            -> ((f \\\ Vector1) :*: (g \\\ Vector1)) a
>            -> (f :*: g) a
>prefixMatrix d down right = grid (Matrix $ Vector1 (Vector1 d))
>                       (Matrix $ Vector1 right)
>                       (Matrix $ fmap Vector1 down)



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

>instance (Read a) => Read (Vector1 a) where
>  readsPrec i str = do pre <- char '[' str
>                       (x,xr) <- readsPrec 0 pre
>                       xr_ <- char ']' xr 
>                       return (Vector1 x,xr_)
>   where char ch (ch2:cr) | ch == ch2 = return cr
>                          | otherwise = []
>         char ch [] = fail "no match"


(%<|>%) :: (Functor v, RealFloat a, Transposable v Vector1,
            Scalar (v (Complex a)) ~ Complex a,             
            InnerProductSpace (v (Complex a))) => 
           v (Complex a) -> v (Complex a) -> Scalar (v (Complex a))

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
>partial_derivate1x (Covector f) = covector $ partial_derivate ch f
>   where ch eps (Vector1 x) = Vector1 (x+eps)

>instance (a ~ Scalar a, Infinitesimal a, Closed a) => VectorDerivative (Vector1 a) where
>   divergence f = partial_derivate1x (covector (vector_element . (-!<) f))
>   grad f = LinearMap $ \z -> Vector1 (partial_derivate1x f `bracket` z)

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

>instance (Num a) => Semigroup (Vector1 a) where
>   (<>) = (%+)

>instance (Num a) => Monoid (Vector1 a) where
>  mempty = vzero
>  mappend = (%+)

>instance (Num a, ConjugateSymmetric a) => Semigroup ((Vector1 :*: Vector1) a) where
>   (<>) = (%**%)

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Monoid ((Vector1 :*: Vector1) a) where
>   mempty = identity
>   mappend = (%**%)

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
>  vzero = Vector1 0
>  vnegate (Vector1 x) = Vector1 (negate x)
>  x %* (Vector1 e) = Vector1 (x * e)
>  (Vector1 a) %+ (Vector1 b) = Vector1 (a + b)

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

