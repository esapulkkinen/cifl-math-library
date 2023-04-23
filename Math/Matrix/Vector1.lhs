>{-# LANGUAGE Safe,MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving, FlexibleContexts, TypeOperators, TypeFamilies, DeriveGeneric, DeriveDataTypeable #-}
>module Math.Matrix.Vector1 where
>import safe Control.Applicative
>import safe Control.Category
>import Prelude hiding ((.),id)
>import safe GHC.Generics hiding ((:*:), (:+:))
>import safe Data.Data
>import safe Data.Typeable
>import safe Data.Monoid hiding (Dual)
>import safe Data.Complex
>import safe Data.Sequence (Seq)
>import safe qualified Data.Binary as Bin
>import safe qualified Data.Sequence as Seq
>import safe qualified Data.Monoid as Mon
>import safe qualified Text.PrettyPrint as Pretty
>import Math.Tools.I
>import Math.Tools.CoMonad
>import Math.Tools.Median
>import Math.Tools.Universe
>import Math.Tools.PrettyP
>import Math.Tools.Arrow
>import Math.Matrix.Interface
>import Math.Number.StreamInterface
>import Math.Number.Interface

>deriving instance (Eq a) => Eq (Vector1 a)
>deriving instance (Ord a) => Ord (Vector1 a)
>deriving instance (Typeable a) => Typeable (Vector1 a)
>deriving instance (Data a) => Data (Vector1 a)
>deriving instance (Generic a) => Generic (Vector1 a)

>instance (Bin.Binary s) => Bin.Binary (Vector1 s) where
>   put (Vector1 x) = Bin.put x
>   get = do { x <- Bin.get ; return (Vector1 x) }

>instance (Universe a) => Universe (Vector1 a) where
>   all_elements = Vector1 <$> all_elements

>instance (Floating a) => Floating (Vector1 a) where
>   pi = Vector1 pi
>   exp (Vector1 x) = Vector1 (exp x)
>   log (Vector1 x) = Vector1 (log x)
>   sqrt (Vector1 x) = Vector1 (sqrt x)
>   (Vector1 x) ** (Vector1 y) = Vector1 (x ** y)
>   logBase (Vector1 x) (Vector1 y) = Vector1 (logBase x y)
>   sin (Vector1 x) = Vector1 (sin x)
>   cos (Vector1 x) = Vector1 (cos x)
>   tan (Vector1 x) = Vector1 (tan x)
>   asin (Vector1 x) = Vector1 (asin x)
>   acos (Vector1 x) = Vector1 (acos x)
>   atan (Vector1 x) = Vector1 (atan x)
>   sinh (Vector1 x) = Vector1 (sinh x)
>   cosh (Vector1 x) = Vector1 (cosh x)
>   tanh (Vector1 x) = Vector1 (tanh x)
>   asinh (Vector1 x) = Vector1 (asinh x)
>   acosh (Vector1 x) = Vector1 (acosh x)
>   atanh (Vector1 x) = Vector1 (atanh x)

>instance DifferentiallyClosed a => DifferentiallyClosed (Vector1 a) where
>   derivate f (Vector1 x) = Vector1 (derivate (vector_element . f . Vector1) x)
>   integral (Vector1 x, Vector1 x') f = Vector1 $
>     integral (x,x') $ \a -> vector_element $ f (Vector1 a)

>instance FunctorArrow Vector1 (->) where
>   amap f = fmap f

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector1 a) where
>   conj = amap conj

>instance Foldable Vector1 where
>   foldMap f (Vector1 x) = f x

>instance Traversable Vector1 where
>   traverse f (Vector1 x) = Vector1 <$> f x

>instance PpShowVerticalF Vector1 where
>   ppf_vertical (Vector1 x) = pp '[' <> Pretty.vcat [pp x] <> pp ']'

>instance PpShowF Vector1 where
>   ppf (Vector1 x) = pp '[' Mon.<> (Pretty.sep [pp x]) Mon.<> pp ']'


>instance Transposable Stream Vector1 a where
>   transpose_impl (Matrix (Pre v vr))
>     = Matrix (Vector1 (Pre (vector_element v) (fmap vector_element vr)))


>instance ProjectionSpace Vector1 Vector1 where
>   data (Vector1 \\\ Vector1) a = S11Vector
>   project_first = id
>   project_second _ = S11Vector
>   join_vector v _ = v

>instance Functor (Vector1 \\\ Vector1) where
>   fmap _ S11Vector = S11Vector

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

>instance (Limiting str a) => Limiting str (Vector1 a) where
>  data Closure str (Vector1 a) = Vector1Closure { unVector1Limit :: str (Vector1 a) }
>  limit str = Vector1Closure str
>  approximations (Vector1Closure a) = a 

>instance (Num a) => Indexable Vector1 a where
>  diagonal_projections = Vector1 (vector_element <!-!> Vector1)
>  indexable_indices = Vector1 0

>instance (Infinitesimal str a) => Infinitesimal str (Vector1 a) where
>  epsilon_stream = fmap Vector1 epsilon_stream

>instance (MedianAlgebra a) => MedianAlgebra (Vector1 a) where
>   med (Vector1 a) (Vector1 b) (Vector1 c) = Vector1 (med a b c)

>type Matrix1 a = (Vector1 :*: Vector1) a

>matrix1 :: a -> Matrix1 a
>matrix1 = Matrix . Vector1 . Vector1

>inVector1 :: (Vector1 a -> Vector1 b) -> a -> b
>inVector1 f x = vector_element (f (Vector1 x))

>inVector1_binary :: (Vector1 a -> Vector1 b -> Vector1 c) -> a -> b -> c
>inVector1_binary f x y = vector_element (f (Vector1 x) (Vector1 y))

>instance (Num a) => Semigroup (Vector1 a) where
>   (<>) = (%+)

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
>  vzero = Vector1 0
>  vnegate (Vector1 x) = Vector1 (negate x)
>  x %* (Vector1 e) = Vector1 (x * e)
>  (Vector1 a) %+ (Vector1 b) = Vector1 (a + b)

>instance (Num a) => NormedSpace (Vector1 a) where
>  norm (Vector1 x) = x

>instance (Num a, ConjugateSymmetric a) => InnerProductSpace (Vector1 a) where
>  (Vector1 x) %. (Vector1 y) = x * conj y

>instance (Num a) => Traceable Vector1 a where
>   trace_impl (Matrix (Vector1 (Vector1 a))) = a
>   determinant_impl (Matrix (Vector1 (Vector1 a))) = a

>instance (Num a) => Diagonalizable Vector1 a where
>  diagonal_impl (Matrix (Vector1 (Vector1 x))) = Vector1 x
>  identity_impl _ = Matrix $ Vector1 (Vector1 1)
>  identity = Matrix $ Vector1 (Vector1 1)
>  diagonal_matrix_impl (Vector1 x) = Matrix $ Vector1 $ Vector1 x


>instance Transposable Vector1 Vector1 a where
>  transpose_impl (Matrix (Vector1 (Vector1 x))) = Matrix $ Vector1 (Vector1 x)

>instance Transposable Vector1 ((->) row) a where
>  transpose_impl (Matrix (Vector1 f)) = Matrix $ \a -> Vector1 (f a)
>instance Transposable ((->) row) Vector1 a where
>  transpose_impl (Matrix f) = Matrix $ Vector1 $ \a -> vector_element (f a)

>instance (Num a) => LinearTransform Vector1 Vector1 a where
>  (Vector1 x) <*>> (Matrix (Vector1 (Vector1 y))) = Vector1 (x*y)
>  (Matrix (Vector1 (Vector1 x))) <<*> (Vector1 y) = Vector1 (x*y)

>instance (Num a) => StandardBasis (Vector1 a) where
>  unit_vectors = [Vector1 1]

>instance (Num a) => VectorSpace ((Vector1 :*: Vector1) a) where
>  type Scalar ((Vector1 :*: Vector1) a) = a
>  vnegate = fmap negate
>  vzero = Matrix $ Vector1 $ Vector1 0
>  m %+ n = liftA2 (+) m n
>  a %* m = fmap (a*) m

>instance (Num a) => CoordinateSpace (Vector1 a) where
>  type Coordinate (Vector1 a) = Int
>  index 0 = vector_element
>  listVector [x] = Vector1 x
>  listVector _ = error "Vector1.vector: dimension error"
>  dimension_size _ = 1
>  coordinates _ = [0]

