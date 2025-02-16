>{-# LANGUAGE Safe, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeOperators, TypeFamilies, PatternGuards, ScopedTypeVariables, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, IncoherentInstances #-}
>module Math.Matrix.Vector2 where
>import safe qualified Text.PrettyPrint as Pretty
>import safe Text.PrettyPrint (vcat,nest,(<+>))
>import safe qualified Data.Monoid as Mon
>import safe Data.Monoid hiding (Dual)
>import safe Data.Complex
>import safe Data.Sequence (Seq, (<|))
>import safe GHC.Generics hiding ((:*:), (:+:))
>import safe Data.Data
>import safe Data.Typeable
>import safe qualified Data.Binary as Bin
>import safe qualified Data.Sequence as Seq
>import safe Control.Applicative
>import safe Control.Category
>import Prelude hiding ((.),id)
>import safe Math.Tools.Arrow
>import safe Math.Tools.Functor
>import safe Math.Tools.PrettyP
>import safe Math.Tools.Median
>import safe Math.Tools.Isomorphism
>import safe Math.Matrix.Interface
>import safe Math.Tools.Visitor
>import safe Math.Tools.CoMonad
>import safe Math.Tools.Universe
>import safe Math.Matrix.Vector1
>import safe Math.Number.Interface
>import safe Math.Number.Group
>import safe Math.Number.StreamInterface
>import safe Math.Tools.I
>import safe qualified Control.Monad.Zip

>data Vector2 s = Vector2 { xcoord2 :: !s, ycoord2 :: !s }

>deriving instance (Eq a) => Eq (Vector2 a)
>deriving instance (Typeable a) => Typeable (Vector2 a)
>deriving instance (Data a) => Data (Vector2 a)
>deriving instance (Generic a) => Generic (Vector2 a)

>instance (Bin.Binary s) => Bin.Binary (Vector2 s) where
>   put (Vector2 x y) = Bin.put x >> Bin.put y 
>   get = do { x <- Bin.get ; y <- Bin.get ; return (Vector2 x y) }

>instance (Read a) => Read (Vector2 a) where
>  readsPrec i str = do pre <- char '[' str
>                       (x,xr) <- readsPrec 0 pre
>                       xr_ <- char ',' xr
>                       (y,yr) <- readsPrec 0 xr_
>                       yr_ <- char ']' yr 
>                       return (Vector2 x y,yr_)
>   where char ch (ch2:cr) | ch == ch2 = return cr
>                          | otherwise = []
>         char ch [] = []

>instance (Universe a) => Universe (Vector2 a) where
>   allElements = Vector2 <$> allElements <*> allElements

>instance Transposable Stream Vector2 a where
>   transposeImpl (Matrix (Pre v vr))
>     = Matrix (Vector2 (Pre (xcoord2 v) (fmap xcoord2 vr))
>                       (Pre (ycoord2 v) (fmap ycoord2 vr)))

>setx2 :: s -> Vector2 s -> Vector2 s
>setx2 x = \v -> v { xcoord2 = x }
>sety2 :: s -> Vector2 s -> Vector2 s
>sety2 x = \v -> v { ycoord2 = x }

>updateRow2 :: g a -> Vector2 ((Vector2 :*: g) a -> (Vector2 :*: g) a)
>updateRow2 x = Vector2 (updateRow setx2 x) (updateRow sety2 x)
>
>updateColumn2 :: (Applicative f) => f a -> Vector2 ((f :*: Vector2) a -> (f :*: Vector2) a)
>updateColumn2 x = Vector2 (updateColumn setx2 x) (updateColumn sety2 x)

>instance UpdateableMatrixDimension Vector2 where
>  writeRow = updateRow2
>  writeColumn = updateColumn2

>type ComplexVector2 a = (Vector2 :*: Complex) a

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>instance (Num a, ConjugateSymmetric a) => ConjugateSymmetric ((Vector2 :*: Vector2) a) where
>   conj = fmap conj . transposeImpl

>i2 :: (Num a) => Vector2 a
>i2 = identityImpl (Vector2 0 1) <!> (xcoord2,id)

>j2 :: (Num a) => Vector2 a
>j2 = identityImpl (Vector2 0 1) <!> (ycoord2,id)

>vector2Iso :: Vector2 a :==: Complex a
>vector2Iso = (\ (Vector2 a b) -> a :+ b) <-> (\(a :+ b) -> Vector2 a b)

>instance ProjectionSpace Vector2 Vector1 where
>   data (Vector2 \\\ Vector1) a = S21Vector (Vector1 a)
>   projectFirst (Vector2 x _) = Vector1 x
>   projectSecond (Vector2 _ y) = S21Vector $ Vector1 y
>   joinVector (Vector1 x) (S21Vector (Vector1 y)) = Vector2 x y

>instance Functor (Vector2 \\\ Vector1) where
>   fmap f (S21Vector x) = S21Vector $ fmap f x

>instance Applicative (Vector2 \\\ Vector1) where
>   pure x = S21Vector $ pure x
>   (S21Vector f) <*> (S21Vector x) = S21Vector $ f <*> x

>matrixRoot :: (Diagonalizable m a,ProjectionSpace m Vector1) => (m :*: m) a -> a
>matrixRoot = vectorElement . projectFirst . diagonalImpl


>instance FunctorArrow Vector2 (:==:) (:==:) where
>  amap f = (\ (Vector2 a b) -> Vector2 (runIso f a) (runIso f b))
>        <-> (\ (Vector2 a b) -> Vector2 (runIsoInverse f a) (runIsoInverse f b))


>instance Control.Monad.Zip.MonadZip Vector2 where
>  mzip ~(Vector2 x y) ~(Vector2 x' y') = Vector2 (x,x') (y,y')
>  mzipWith f ~(Vector2 x y) ~(Vector2 x' y') = Vector2 (f x x') (f y y')
>  munzip (Vector2 ~(x,x') ~(y,y')) = (Vector2 x y, Vector2 x' y')


>instance Comonad Vector2 where
>   extract (Vector2 x _) = x
>   duplicate (Vector2 x y) = Vector2 (Vector2 x y)
>                                     (Vector2 y x)

>instance CircularComonad Vector2 where
>   rotate (Vector2 x y) = Vector2 y x

>type Matrix2 a = (Vector2 :*: Vector2) a

>instance CodiagonalMatrix Vector2 a where
>   data Codiagonal Vector2 a = Codiagonal2 {
>     down_codiagonal2 :: Vector1 a,
>     right_codiagonal2 :: Vector1 a
>   }
>   type (Vector2 \\ a) = Vector1 a
>   codiagonalImpl = codiagonal2
>   (|\|) = mat2
>   downProject = down_codiagonal2
>   rightProject = right_codiagonal2

>instance Functor (Codiagonal Vector2) where
>   fmap f (Codiagonal2 a b) = Codiagonal2 (fmap f a) (fmap f b)

>instance Applicative (Codiagonal Vector2) where
>   pure x = Codiagonal2 (pure x) (pure x)
>   (Codiagonal2 f1 f2) <*> (Codiagonal2 x1 x2) =
>     Codiagonal2 (f1 <*> x1) (f2 <*> x2)

>instance (Show a) => Show (Codiagonal Vector2 a) where
>   show (Codiagonal2 down right) = "* " ++ show (vectorElement right) ++ "\n"
>                                   ++ show (vectorElement down) ++ " *"

deriving instance (Show a) => Show (Codiagonal Vector2 a)

>zero_codiagonal2 :: (Num a) => Codiagonal Vector2 a
>zero_codiagonal2 = Codiagonal2 vzero vzero

>constant2 :: a -> Vector2 a
>constant2 x = Vector2 x x


>-- | <https://en.wikipedia.org/wiki/Complex_number>

>complexMatrix :: (Num a) => Complex a -> (Vector2 :*: Vector2) a
>complexMatrix (a :+ b) = Matrix $ Vector2 (Vector2 a (negate b))
>                                          (Vector2 b a)

>codiagonal2 :: Matrix2 a -> Codiagonal Vector2 a
>codiagonal2 (Matrix (Vector2 (Vector2 _ y)
>                             (Vector2 x _))) =
>   Codiagonal2 (Vector1 x) (Vector1 y)


>delPartial2 :: (DifferentiallyClosed a) => (Vector2 a -> a) -> Vector2 a -> Vector2 a
>delPartial2 f (Vector2 x y) = Vector2 (partial1_2 ff x y) (partial2_2 ff x y)
>   where ff a b = f (Vector2 a b)

>instance DifferentialOperator Vector2 where
>   partial = delPartial2

>diagonalMatrix2 :: (Num a) => Vector2 a -> (Vector2 :*: Vector2) a
>diagonalMatrix2 v = v |\| zero_codiagonal2

>sumCoordinates2 :: (Num a) => Vector2 a -> a
>sumCoordinates2 (Vector2 x y) = x + y

>productCoordinates2 :: (Num a) => Vector2 a -> a
>productCoordinates2 (Vector2 x y) = x * y

>mat2 :: Vector2 a -> Codiagonal Vector2 a -> (Vector2 :*: Vector2) a
>mat2 (Vector2 a b) (Codiagonal2 (Vector1 a') (Vector1 a''))
>    = Matrix $ Vector2 (Vector2 a a'')
>                       (Vector2 a' b)

>matrix2 :: Vector2 a -> Codiagonal Vector2 a -> (Vector2 :*: Vector2) a
>matrix2 (Vector2 d x) (Codiagonal2 (Vector1 b) (Vector1 a)) = Matrix $
>   Vector2 (Vector2 d a) (Vector2 b x)

>vec2 :: (a,a) -> Vector2 a
>vec2 (x,y) = Vector2 x y

>su2_matrix :: (Eq a, RealFloat a) => Complex a -> Complex a -> Matrix2 (Complex a)
>su2_matrix a b 
>   | normSquared a + normSquared b == 1 = Matrix $ 
>       Vector2 (Vector2 a (negate $ conj b)) 
>               (Vector2 b (conj a))

>splitMatrix :: (Functor f, SplittableVector f f, SplittableVector m m)
>            => ((f :+: f) :*: (m :+: m)) a
>            -> (Vector2 :*: Vector2) ((f :*: m) a)
>splitMatrix (Matrix m) = Matrix $ Vector2 (Vector2 (Matrix m1a) (Matrix m1b))
>                                          (Vector2 (Matrix m2a) (Matrix m2b))
>   where (m1,m2) = vsplit m
>         (m1a,m1b) = funzip $ fmap vsplit m1
>         (m2a,m2b) = funzip $ fmap vsplit m2

>instance Visitor (Vector2 a) where
>  data Fold (Vector2 a) b = Vector2Fold (a -> a -> b)
>  visit (Vector2Fold f) (Vector2 x y) = f x y

>instance AppendableVector Vector1 Vector1 where
>  type (Vector1 :+: Vector1) = Vector2
>  (Vector1 x) ||>> (Vector1 y) = Vector2 x y

>instance SplittableVector Vector1 Vector1 where
>  vsplit (Vector2 x y) = (Vector1 x, Vector1 y)

>instance (Ord a) => Ord (Vector2 a) where
>   (Vector2 x y) <= (Vector2 x' y') = x <= x' && y <= y'

>instance (Limiting str a, Monad str) => Limiting str (Vector2 a) where
>   data Closure str (Vector2 a) = Vector2Closure (Vector2 (Closure str a))
>   limit str = Vector2Closure $ Vector2 
>                       (limit $ fmap xcoord2 str)
>                       (limit $ fmap ycoord2 str)
>   approximations (Vector2Closure (Vector2 x y)) = do
>     (a',b') <- approximations x <&> approximations y
>     return $ Vector2 a' b'

> 
>instance Monad Vector2 where
>   return x = Vector2 x x
>   (>>=) = bindDiagonal2

>instance (MedianAlgebra s) => MedianAlgebra (Vector2 s) where
>  med (Vector2 a b) (Vector2 a' b') (Vector2 a'' b'')
>   = Vector2 (med a a' a'') (med b b' b'') 

>instance (Num s) => Semigroup (Vector2 s) where
>  (<>) = (%+)

>instance (Num s) => Monoid (Vector2 s) where
>  mempty = vzero
>  mappend = (%+)

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Semigroup ((Vector2 :*: Vector2) a) where
>   (<>) = (%*%)

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Monoid ((Vector2 :*: Vector2) a) where
>   mempty = identityImpl (Vector2 0 1)
>   mappend = (%*%)

>instance Applicative Vector2 where
>   pure x = Vector2 x x
>   (Vector2 f g) <*> (Vector2 x y) = Vector2 (f x) (g y)

>instance (PpShow a) => PpShow (Vector2 a) where
>  pp (Vector2 x y) = vcat $ liftA2 nest [0,40] (map pp [x,y])

>instance (PpShow (f a)) => PpShow ((Vector2 :*: f) a) where
>  pp (Matrix (Vector2 x y)) = verticalize $ map pp [x,y]

>instance PpShowVerticalF Vector2 where
>   ppfVertical (Vector2 x y) = pp '[' <> vcat [pp x,pp y] <> pp ']'

>instance PpShowF Vector2 where
>   ppf (Vector2 x y) = pp '[' Mon.<> (Pretty.sep [pp x,pp ',', pp y]) Mon.<> pp ']'


>instance (Show (f a)) => Show ((Vector2 :*: f) a) where
>  show (Matrix (Vector2 a b)) = show a ++ "\n" ++ show b

>instance (Show a) => Show (Vector2 a) where
>  show (Vector2 x y) = "[" ++ show x ++ "," ++ show y ++ "]"

>instance (Fractional a, ConjugateSymmetric a) => Group ((Vector2 :*: Vector2) a) where
>   ginvert = inverse2

>instance (Num a) => CoordinateSpace (Vector2 a) where
>  type Coordinate (Vector2 a) = Int
>  index = index_vector2
>  listVector = vector_vector2
>  dimensionSize _ = 2
>  coordinates _ = [0,1,2]


>instance Foldable Vector2 where
>   foldMap f (Vector2 x y) = f x `mappend` f y

>instance Traversable Vector2 where
>   traverse f (Vector2 x y) = Vector2 <$> f x <*> f y


>removex2 :: Vector2 a -> Vector1 a
>removex2 (Vector2 _ y) = Vector1 y
>removey2 :: Vector2 a -> Vector1 a
>removey2 (Vector2 x _) = Vector1 x

>remove2 :: Vector2 (Vector2 a -> Vector1 a)
>remove2 = Vector2 removex2 removey2

>remove_index2 :: Matrix2 a -> Matrix2 (Matrix1 a)
>remove_index2 (Matrix m) = matrix app remove2 remove2
>  where app f g = Matrix $ f $ fmap g m

>instance (Fractional a) => Invertible Vector2 a where
>  cofactorImpl = cofactor2
>  adjucateImpl = adjucate2
>  inverseImpl = inverse2


>cofactor2 :: (Num a) => Matrix2 a -> Matrix2 a
>cofactor2 m = pure (*) <*> fmap fromIntegral signs2 <*> fmap determinantImpl (remove_index2 m)

>adjucate2 :: (Num a) => Matrix2 a -> Matrix2 a
>adjucate2 = transposeImpl . cofactor2
>
>inverse2 :: (Fractional a) => Matrix2 a -> Matrix2 a
>inverse2 m = (1/determinantImpl m) %* adjucate2 m

>vector_indices2 :: Vector2 Integer
>vector_indices2 = Vector2 1 2

>matrix_indices2 :: (Vector2 :*: Vector2) (Integer,Integer)
>matrix_indices2 = matrix (,) vector_indices2 vector_indices2

>signs2 :: (Vector2 :*: Vector2) Integer
>signs2 = fmap (\ (i,j) -> ((i+j+1) `mod` 2) * 2 - 1) matrix_indices2

>instance (Num a, ConjugateSymmetric a) => Num ((Vector2 :*: Vector2) a) where
>   (Matrix v) + (Matrix v') = Matrix $ v + v'
>   (Matrix v) - (Matrix v') = Matrix $ v - v'
>   (*) = (%*%)
>   negate (Matrix v) = Matrix $ negate v
>   abs (Matrix v) = Matrix (abs v)
>   signum (Matrix v) = Matrix $ signum v
>   fromInteger = diagonalMatrix2 . constant2 . fromInteger

instance (Num a, ConjugateSymmetric a) => LieAlgebra ((Vector2 :*: Vector2) a) where
   (%<>%) = matrix_commutator



>vector_vector2 :: [a] -> Vector2 a
>vector_vector2 [x,y] = Vector2 x y
>vector_vector2 _ = error "vector2: Invalid number of elements in vector"

>diagonal2 :: (Num a) => Matrix2 a -> Vector2 a
>diagonal2 (Matrix m) = appIndex diagonalProjections2 m

>instance (Num a) => Indexable Vector2 a where
>   diagonalProjections = diagonalProjections2
>   indexableIndices = Vector2 0 1

>composeIndex :: (m (n a) :==: I (n a)) -> (n a :==: I a) -> (m :*: n) a :==: (I :*: I) a
>composeIndex f g = (Matrix . I . runIso g . unI . runIso f . cells)
>                 <-> (Matrix . runIsoInverse f . amap (runIsoInverse g) . cells)


>instance (Num a) => Indexable (Vector2 :*: Vector2) a where
>   indexableIndices = Matrix $ Vector2 (Vector2 0 1) (Vector2 3 4)
>   diagonalProjections =
>      matrix (\x y -> x . amap ((unI <-> I) . y) . (cells <-> Matrix))
>                 (diagonalProjections :: Vector2 (Index Vector2 a))
>                 (diagonalProjections :: Vector2 (Index Vector2 a))

>diagonalProjections2 ::  (Num a) => Vector2 (Index Vector2 a)
>diagonalProjections2 = Vector2 ((I . xcoord2) <-> xcoord2inv)
>                                  ((I . ycoord2) <-> ycoord2inv)
>   where xcoord2inv (I a) = Vector2 a 0
>         ycoord2inv (I a) = Vector2 0 a

>transpose2 :: (Num a) => Matrix2 a -> Matrix2 a
>transpose2 (Matrix m) = matrix indexProject diagonalProjections2 m

>transpose2Impl :: Matrix2 a -> Matrix2 a
>transpose2Impl ~(Matrix ~(Vector2 ~(Vector2 x1 y1) ~(Vector2 x2 y2))) =
>  Matrix $ Vector2 (Vector2 x1 x2) (Vector2 y1 y2)

>rotate2 :: Vector2 a -> Vector2 a
>rotate2 (Vector2 x y) = Vector2 y x

>bindDiagonal2 :: Vector2 a -> (a -> Vector2 b) -> Vector2 b
>(Vector2 x y) `bindDiagonal2` f = Vector2 xx yy
>      where Vector2 xx _ = f x
>            Vector2 _ yy = f y

>bindCodiagonal2 :: Vector2 a -> (a -> Vector2 b) -> Vector2 b
>(Vector2 x y) `bindCodiagonal2` f = Vector2 xy yx
>      where Vector2 _ xy = f x
>            Vector2 yx _ = f y

>instance Functor Vector2 where 
>   fmap f = \ (Vector2 x y) -> Vector2 (f x) (f y)

>crossProductScalar2 :: (Num a) => Vector2 a -> Vector2 a -> a
>crossProductScalar2 (Vector2 x y) (Vector2 x' y') = (x*y') - (y*x')

>orthogonalVector2 :: (Num a) => Vector2 a -> Vector2 a
>orthogonalVector2 (Vector2 x y) = Vector2 y (negate x)

>crossProduct2 :: (Num a) => (Vector1 :*: Vector2) a -> Vector2 a
>crossProduct2 (Matrix (Vector1 v)) = orthogonalVector2 v


>instance (Num a) => Num (Vector2 a) where
>  (Vector2 x y) + (Vector2 x' y') = Vector2 (x+x') (y+y')
>  (Vector2 x y) - (Vector2 x' y') = Vector2 (x-x') (y-y')
>  (Vector2 x y) * (Vector2 x' y') = error "Trying to calculate cross product of 2d vectors"
>  negate (Vector2 x y) = Vector2 (negate x) (negate y)
>  abs (Vector2 x y) = Vector2 (abs x) (abs y)
>  signum (Vector2 x y) = Vector2 (signum x) (signum y)
>  fromInteger i = error "fromInteger: Vector2 requires 2 components"

>leftMultiply2Gen :: (Functor f, Num a, ConjugateSymmetric a) => Vector2 a -> (f :*: Vector2) a -> f a
>leftMultiply2Gen v (Matrix w) = (sum . (pure (*) <*> v <*>)) <$> fmap conj w

>rightMultiply2Gen :: (VectorSpace (f a), ConjugateSymmetric a, Scalar (f a) ~ a) => (Vector2 :*: f) a -> Vector2 a -> f a
>rightMultiply2Gen (Matrix w) v = vsum $ liftA2 (\a fa -> a %* fa) (conj v) w

>leftMultiply1_2 :: (Num a, ConjugateSymmetric a) => Vector2 a -> (Vector1 :*: Vector2) a -> Vector1 a
>leftMultiply1_2 (Vector2 x y) (Matrix (Vector1 (Vector2 x1 y1)))
>   = Vector1 (x*conj x1+y*conj y1)
>
>leftMultiply2_1 :: (Num a, ConjugateSymmetric a) => Vector1 a -> (Vector2 :*: Vector1) a -> Vector2 a
>leftMultiply2_1 (Vector1 a) (Matrix (Vector2 (Vector1 x) (Vector1 y)))
>   = Vector2 (a*conj x) (a*conj y)

>leftMultiply2 :: (Num a, ConjugateSymmetric a) => Vector2 a -> Matrix2 a -> Vector2 a
>leftMultiply2 (Vector2 x y) (Matrix (Vector2 (Vector2 x1 y1)
>                                      (Vector2 x2 y2)))
>  = Vector2 (x*conj x1+y*conj y1)
>            (x*conj x2+y*conj y2)                               

>rightMultiply2 :: (Num a, ConjugateSymmetric a) => Matrix2 a -> Vector2 a -> Vector2 a
>rightMultiply2 (Matrix (Vector2 (Vector2 x1 y1)
>                         (Vector2 x2 y2))) (Vector2 x y)
>   = Vector2 (x1*x'+x2*y')
>             (y1*x'+y2*y')                  
>    where x' = conj x
>          y' = conj y

>rightMultiply2_1 :: (ConjugateSymmetric a,Num a) => (Vector2 :*: Vector1) a -> Vector2 a -> Vector1 a
>rightMultiply2_1 (Matrix (Vector2 (Vector1 x) (Vector1 y))) (Vector2 x' y')
>  = Vector1 (x*conj x'+y*conj y')
>
>rightMultiply1_2 :: (ConjugateSymmetric a,Num a) => (Vector1 :*: Vector2) a -> Vector1 a -> Vector2 a
>rightMultiply1_2 (Matrix (Vector1 (Vector2 x y))) (Vector1 z)
>  = Vector2 (x*z') (y*z')
>    where z' = conj z

>instance (Num a) => VectorSpace (Vector2 a) where
>  type Scalar (Vector2 a) = a
>  v %* (Vector2 x y) = Vector2 (inVector1 (v %*) x) (inVector1 (v %*) y)
>  (Vector2 x y) %+ (Vector2 x' y') = Vector2 (x+x') (y+y')
>  vzero = Vector2 0 0
>  vnegate (Vector2 x y) = Vector2 (negate x) (negate y)

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector2 a) where
>   conj (Vector2 x y) = Vector2 (conj x) (conj y)

>instance {-# INCOHERENT #-}  (Floating a, ConjugateSymmetric a) => NormedSpace (Vector2 a) where
>  normSquared v = v %. v

>instance {-# INCOHERENT #-} (Num a, ConjugateSymmetric a) => InnerProductSpace (Vector2 a) where
>  (Vector2 x y) %. (Vector2 x' y') = x*conj x' + y*conj y'

>instance (Num a) => Diagonalizable Vector2 a where
>  diagonalImpl = diagonal2 
>  identityImpl _ = identity2
>  diagonalMatrixImpl = diagonalMatrix2
>  identity = identity2

>instance (Num a) => Traceable Vector2 a where
>  traceImpl = trace2
>  determinantImpl = determinant2


>instance Transposable Vector2 ((->) row) a where
>   transposeImpl (Matrix (Vector2 x y))
>     = Matrix $ \a -> Vector2 (x a) (y a)
>instance Transposable ((->) row) Vector2 a where
>   transposeImpl (Matrix f)
>     = Matrix $ Vector2 (\a -> xcoord2 (f a))
>                        (\a -> ycoord2 (f a))


>instance Transposable Vector2 Vector2 a where
>  transposeImpl   = transpose2Impl

>instance Transposable Vector2 Vector1 a where
>  transposeImpl (Matrix (Vector2 (Vector1 x) (Vector1 y)))
>    = Matrix (Vector1 (Vector2 x y))
>instance Transposable Vector1 Vector2 a where
>  transposeImpl (Matrix (Vector1 (Vector2 x y)))
>    = Matrix (Vector2 (Vector1 x) (Vector1 y))

>instance {-# INCOHERENT #-} (ConjugateSymmetric a, Num a) => LinearTransform Vector2 Vector2 a where
>  (<*>>) = leftMultiply2
>  (<<*>) = rightMultiply2

>instance (ConjugateSymmetric a, Num a) => LinearTransform Vector2 Vector1 a where
>  (<*>>) = leftMultiply2_1
>  (<<*>) = rightMultiply2_1
>
>instance (ConjugateSymmetric a, Num a) => LinearTransform Vector1 Vector2 a where
>  (<*>>) = leftMultiply1_2
>  (<<*>) = rightMultiply1_2

>trace2 :: (Num a) => Matrix2 a -> a
>trace2 t | Vector2 x y <- diagonal2x t = x + y


>diagonal2x :: (Num a) => Matrix2 a -> Vector2 a
>diagonal2x (Matrix (Vector2 (Vector2 a _)
>                   (Vector2 _ d))) = Vector2 a d            

>diagonal2y :: (Num a) => Matrix2 a -> Vector2 a
>diagonal2y (Matrix (Vector2 (Vector2 _ b)
>                   (Vector2 c _))) = Vector2 b c

>identity2 :: (Num a) => Matrix2 a
>identity2 = Matrix $ Vector2 (Vector2 1 0)
>                        (Vector2 0 1)


>determinant2 :: (Num a) => Matrix2 a -> a
>determinant2 (Matrix (Vector2 (Vector2 x y)
>                         (Vector2 x' y'))) = x*y' - x'*y
                 
>instance StandardBasis (Vector2 Int) where
>  unitVectors = [Vector2 1 0, Vector2 0 1]


>instance StandardBasis (Vector2 (Vector2 Int)) where
>  unitVectors = [Vector2 (Vector2 1 0)
>                          (Vector2 0 0), 
>                  Vector2 (Vector2 0 1)
>                          (Vector2 0 0),
>                  Vector2 (Vector2 0 0)
>                          (Vector2 1 0),
>                  Vector2 (Vector2 0 0)
>                          (Vector2 0 1)]

>instance (Num a) => StandardBasis (Vector2 a) where
>  unitVectors = [Vector2 1 0, Vector2 0 1]

>index_vector2 :: Int -> Vector2 a -> a
>index_vector2 0 = xcoord2
>index_vector2 1 = ycoord2
>index_vector2 _ = error "index_vector2: invalid index"

>rotateMatrix2 :: (Floating a) => a -> Matrix2 a
>rotateMatrix2 alfa = Matrix $ Vector2 (Vector2 (cos alfa) (-sin alfa))
>                                      (Vector2 (sin alfa) (cos alfa))

>-- | <https://en.wikipedia.org/wiki/Eigenvalue_algorithm>

>eigenvalue2 :: (Floating a) => (Vector2 :*: Vector2) a -> Vector2 a
>eigenvalue2 a = Vector2 ((tra + m) / 2) ((tra - m) / 2)
>  where m = sqrt(tra * tra - 4 * determinantImpl a)
>        tra = traceImpl a

>instance (Floating a) => EigenDecomposable Vector2 a where
>   eigenvalues = eigenvalue2

>instance (Num a) => VectorSpace ((Vector2 :*: Vector2) a) where
>  type Scalar ((Vector2 :*: Vector2) a) = a
>  vzero = Matrix $ Vector2 vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v*)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>instance (Num a) => VectorSpace ((Vector2 :*: Vector1) a) where
>  type Scalar ((Vector2 :*: Vector1) a) = a
>  vzero = Matrix $ Vector2 vzero vzero
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v*)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>instance (Num a) => VectorSpace ((Vector1 :*: Vector2) a) where
>  type Scalar ((Vector1 :*: Vector2) a) = a
>  vzero = Matrix $ Vector1 vzero 
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v*)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>leviCivita2 :: (Num a) => (Vector2 :*: Vector2) a
>leviCivita2 = Matrix $ Vector2 (Vector2 0 1) (Vector2 (-1) 0)
