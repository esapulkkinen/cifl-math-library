>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeOperators, TypeFamilies, PatternGuards, ScopedTypeVariables, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
>module Math.Matrix.Vector2 where
>import safe Text.PrettyPrint (vcat,nest,(<+>))
>import safe Data.Monoid hiding (Dual)
>import safe Data.Complex
>import safe Data.Sequence (Seq, (<|))
>import safe GHC.Generics hiding ((:*:), (:+:))
>import safe Data.Data
>import safe Data.Typeable
>import safe qualified Data.Binary as Bin
>import safe qualified Data.Sequence as Seq
>import safe Control.Applicative
>import safe Math.Tools.Arrow
>import safe Math.Tools.Functor
>import safe Math.Matrix.Matrix
>import safe Math.Tools.PrettyP
>import safe Math.Tools.Median
>import safe Math.Tools.Isomorphism
>import safe Math.Matrix.Interface
>import safe Math.Matrix.Covector
>import safe Math.Tools.Visitor
>import safe Math.Tools.CoMonad
>import safe Math.Tools.Universe
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Simple hiding (determinant2)
>import safe Math.Number.Interface
>import safe Math.Number.Group
>import safe Math.Number.Real
>import safe Math.Number.Stream

>data Vector2 s = Vector2 { xcoord2 :: s, ycoord2 :: s }
>   deriving (Eq, Typeable, Data, Generic)

>-- | this computes partial derivates of the scalar-value 2D vector field
>-- along both variables simultaneously.
>del_vector2 :: (Infinitesimal a) => (Vector2 a -> a) -> Vector2 a -> Vector2 (Closure a)
>del_vector2 f (Vector2 x y) = Vector2 (partial_derivate1_2 ff x y)
>                                      (partial_derivate2_2 ff x y)
>  where ff a b = f (Vector2 a b)


>cov2 :: Vector2 (Dual (Vector2 a))
>cov2 = Vector2 (covector xcoord2) (covector ycoord2)

>instance ProjectionDual Vector2 a where
>   projection_dual = cov2

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
>   all_elements = Vector2 <$> all_elements <*> all_elements


>type ComplexVector2 a = (Vector2 :*: Complex) a

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>instance (ConjugateSymmetric a) => ConjugateSymmetric ((Vector2 :*: Vector2) a) where
>   conj = fmap conj . transpose

>i2 :: (Num a) => Vector2 a
>i2 = identity <!> (xcoord2,id)

>j2 :: (Num a) => Vector2 a
>j2 = identity <!> (ycoord2,id)

>vector2Iso :: Vector2 a :==: Complex a
>vector2Iso = (\ (Vector2 a b) -> a :+ b) <-> (\(a :+ b) -> Vector2 a b)

>instance ProjectionSpace Vector2 Vector1 where
>   data (Vector2 \\\ Vector1) a = S21Vector (Vector1 a)
>   project_first (Vector2 x _) = Vector1 x
>   project_second (Vector2 _ y) = S21Vector $ Vector1 y
>   join_vector (Vector1 x) (S21Vector (Vector1 y)) = Vector2 x y

>instance Functor (Vector2 \\\ Vector1) where
>   fmap f (S21Vector x) = S21Vector $ fmap f x

>instance Applicative (Vector2 \\\ Vector1) where
>   pure x = S21Vector $ pure x
>   (S21Vector f) <*> (S21Vector x) = S21Vector $ f <*> x

>matrix_root :: (SquareMatrix m a,ProjectionSpace m Vector1) => (m :*: m) a -> a
>matrix_root = vector_element . project_first . diagonal

>instance Comonad Vector2 where
>   extract (Vector2 x _) = x
>   duplicate (Vector2 x y) = Vector2 (Vector2 x y)
>                                     (Vector2 y x)

>instance CircularComonad Vector2 where
>   rotate (Vector2 x y) = Vector2 y x

>instance (Num a) => FiniteDimensional (Vector2 a) where
>   finite (Matrix (Covector f)) = Vector2 (f (covector xcoord2))
>                                          (f (covector ycoord2))

>x2_op :: Dual (Vector2 s)
>x2_op = covector xcoord2

>y2_op :: Dual (Vector2 s)
>y2_op = covector ycoord2

>type Matrix2 a = (Vector2 :*: Vector2) a

>instance CodiagonalMatrix Vector2 a where
>   data Codiagonal Vector2 a = Codiagonal2 {
>     down_codiagonal2 :: Vector1 a,
>     right_codiagonal2 :: Vector1 a
>   }
>   type (Vector2 \\ a) = Vector1 a
>   codiagonal = codiagonal2
>   (|\|) = mat2
>   down_project = down_codiagonal2
>   right_project = right_codiagonal2

>instance Functor (Codiagonal Vector2) where
>   fmap f (Codiagonal2 a b) = Codiagonal2 (fmap f a) (fmap f b)

>instance Applicative (Codiagonal Vector2) where
>   pure x = Codiagonal2 (pure x) (pure x)
>   (Codiagonal2 f1 f2) <*> (Codiagonal2 x1 x2) =
>     Codiagonal2 (f1 <*> x1) (f2 <*> x2)

>instance (Show a) => Show (Codiagonal Vector2 a) where
>   show (Codiagonal2 down right) = "* " ++ show (vector_element right) ++ "\n"
>                                   ++ show (vector_element down) ++ " *"

deriving instance (Show a) => Show (Codiagonal Vector2 a)

>zero_codiagonal2 :: (Num a) => Codiagonal Vector2 a
>zero_codiagonal2 = Codiagonal2 vzero vzero

>constant2 :: a -> Vector2 a
>constant2 x = Vector2 x x

>vector2_to_vec2 :: Vector2 a -> TwoD -> a
>vector2_to_vec2 (Vector2 x y) = svec2 x y

>-- | <https://en.wikipedia.org/wiki/Complex_number>

>complexMatrix :: (Num a) => Complex a -> (Vector2 :*: Vector2) a
>complexMatrix (a :+ b) = Matrix $ Vector2 (Vector2 a (negate b))
>                                          (Vector2 b a)

>codiagonal2 :: Matrix2 a -> Codiagonal Vector2 a
>codiagonal2 (Matrix (Vector2 (Vector2 _ y)
>                             (Vector2 x _))) =
>   Codiagonal2 (Vector1 x) (Vector1 y)


>del_partial2 :: (DifferentiallyClosed a) => (Vector2 a -> a) -> Vector2 a -> Vector2 a
>del_partial2 f (Vector2 x y) = Vector2 (partial1_2 ff x y) (partial2_2 ff x y)
>   where ff a b = f (Vector2 a b)

>instance DifferentialOperator Vector2 where
>   partial = del_partial2

>diagonal_matrix2 :: (Num a) => Vector2 a -> (Vector2 :*: Vector2) a
>diagonal_matrix2 v = v |\| zero_codiagonal2

>sum_coordinates2 :: (Num a) => Vector2 a -> a
>sum_coordinates2 (Vector2 x y) = x + y

>product_coordinates2 :: (Num a) => Vector2 a -> a
>product_coordinates2 (Vector2 x y) = x * y

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
>   | norm a * norm a + norm b * norm b == 1 = Matrix $ 
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
>  (Vector1 x) |> (Vector1 y) = Vector2 x y

>instance AppendableVector Vector2 Stream where
>  type (Vector2 :+: Stream) = Stream
>  (Vector2 x y) |> s = x `Pre` y `Pre` s


>instance SplittableVector Vector1 Vector1 where
>  vsplit (Vector2 x y) = (Vector1 x, Vector1 y)

>instance (Ord a) => Ord (Vector2 a) where
>   (Vector2 x y) <= (Vector2 x' y') = x <= x' && y <= y'

>instance (Limiting a) => Limiting (Vector2 a) where
>   data Closure (Vector2 a) = Vector2Closure (Vector2 (Closure a))
>   limit str = Vector2Closure $ Vector2 
>                       (limit $ fmap xcoord2 str)
>                       (limit $ fmap ycoord2 str)
>   approximations (Vector2Closure (Vector2 x y)) = do
>     (a',b') <- approximations x <&> approximations y
>     return $ Vector2 a' b'

>instance (Infinitesimal a, Closed a) => VectorDerivative (Vector2 a) where
>   divergence = divergence2
>   grad = grad2



>divergence2 :: (Infinitesimal a, Closed a) => LinearMap (Vector2 a) (Vector2 a) -> Dual (Vector2 a)
>divergence2 f = partial_derivate2x (linear_dual_2x f)
>             %+ partial_derivate2y (linear_dual_2y f)

>linear_dual_2x :: LinearMap v (Vector2 (Scalar v)) -> Dual v
>linear_dual_2x f = covector (xcoord2 . (-!<) f)
>
>linear_dual_2y :: LinearMap v (Vector2 (Scalar v)) -> Dual v
>linear_dual_2y f = covector (ycoord2 . (-!<) f)

>grad2 :: (Infinitesimal a, Closed a) => Dual (Vector2 a) -> LinearMap (Vector2 a) (Vector2 a)
>grad2 f = LinearMap $ \z -> Vector2 (partial_derivate2x f `bracket` z)
>                                    (partial_derivate2y f `bracket` z)

>curl2 :: (Infinitesimal a, Closed a) => LinearMap (Vector2 a) (Vector2 a) -> LinearMap (Vector2 a) (Vector2 a)
>curl2 f = LinearMap $ \z -> Vector2 (partial_derivate2y fx `bracket` z
>                     - partial_derivate2x fy `bracket` z)
>                    (partial_derivate2x fy `bracket` z 
>                     - partial_derivate2y fx `bracket` z)
>  where fx = linear_dual_2x f
>        fy = linear_dual_2y f

>del2 :: (Infinitesimal v, Closed v) => Vector2 (Dual (Vector2 v) -> Dual (Vector2 v))
>del2 = Vector2 partial_derivate2x partial_derivate2y

>hessian2 :: (Infinitesimal a, Closed a)
> => Dual (Vector2 a) -> (Vector2 :*: Vector2) (Dual (Vector2 a))
>hessian2 f = matrix (\a b -> a (b f)) del2 del2
> 
>instance (Infinitesimal a, Closed a) => VectorCrossProduct (Vector2 a) where
>  curl = curl2
>
>instance (Infinitesimal a, Closed a) => VectorLaplacian (Vector2 a)

>instance Monad Vector2 where
>   return x = Vector2 x x
>   (>>=) = bind_diagonal2

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
>   (<>) = (%**%)

>-- | see "Lawvere,Rosebrugh: Sets for mathematics", pg. 167.
>instance (Num a, ConjugateSymmetric a) => Monoid ((Vector2 :*: Vector2) a) where
>   mempty = identity
>   mappend = (%**%)

>instance Applicative Vector2 where
>   pure x = Vector2 x x
>   (Vector2 f g) <*> (Vector2 x y) = Vector2 (f x) (g y)

>instance (PpShow a) => PpShow (Vector2 a) where
>  pp (Vector2 x y) = vcat $ liftA2 nest [0,40] (map pp [x,y])

>instance (PpShow (f a)) => PpShow ((Vector2 :*: f) a) where
>  pp (Matrix (Vector2 x y)) = verticalize $ map pp [x,y]


>instance (Show (f a)) => Show ((Vector2 :*: f) a) where
>  show (Matrix (Vector2 a b)) = show a ++ "\n" ++ show b

>instance (Show a) => Show (Vector2 a) where
>  show (Vector2 x y) = "[" ++ show x ++ "," ++ show y ++ "]"

>instance (Fractional a, ConjugateSymmetric a) => Group ((Vector2 :*: Vector2) a) where
>   ginvert = inverse

>instance (Num a) => CoordinateSpace (Vector2 a) where
>  type Coordinate (Vector2 a) = Int
>  index = index_vector2
>  listVector = vector_vector2
>  dimension_size _ = 2
>  coordinates _ = [0,1,2]


>instance (Fractional a) => InvertibleMatrix Vector2 a where
>   cofactor = cofactor2
>   adjucate = adjucate2
>   inverse = inverse2

>instance Foldable Vector2 where
>   foldMap f (Vector2 x y) = f x `mappend` f y

>instance Traversable Vector2 where
>   traverse f (Vector2 x y) = Vector2 <$> f x <*> f y

>instance (Fractional a, ConjugateSymmetric a) => Fractional ((Vector2 :*: Vector2) a) where
>   recip = inverse
>   fromRational = diagonal_matrix . constant2 . fromRational

>removex2 :: Vector2 a -> Vector1 a
>removex2 (Vector2 _ y) = Vector1 y
>removey2 :: Vector2 a -> Vector1 a
>removey2 (Vector2 x _) = Vector1 x

>remove2 :: Vector2 (Vector2 a -> Vector1 a)
>remove2 = Vector2 removex2 removey2

>remove_index2 :: Matrix2 a -> Matrix2 (Matrix1 a)
>remove_index2 (Matrix m) = matrix app remove2 remove2
>  where app f g = Matrix $ f $ fmap g m

>cofactor2 :: (Num a) => Matrix2 a -> Matrix2 a
>cofactor2 m = pure (*) <*> fmap fromIntegral signs2 <*> fmap determinant (remove_index2 m)

>adjucate2 :: (Num a) => Matrix2 a -> Matrix2 a
>adjucate2 = transpose . cofactor2
>
>inverse2 :: (Fractional a) => Matrix2 a -> Matrix2 a
>inverse2 m = (1/determinant m) %* adjucate2 m


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
>   fromInteger = diagonal_matrix . constant2 . fromInteger

>instance (Num a, ConjugateSymmetric a) => LieAlgebra ((Vector2 :*: Vector2) a) where
>   (%<>%) = matrix_commutator


>partial_derivate2x :: (Fractional a,Infinitesimal a, Closed a) 
>                   => Dual (Vector2 a) -> Dual (Vector2 a)
>partial_derivate2x (Covector f) = covector $ partial_derivate ch f
>  where ch eps (Vector2 x y) = Vector2 (x+eps) y

>partial_derivate2y :: (Fractional a,Infinitesimal a, Closed a)
>                   => Dual (Vector2 a) -> Dual (Vector2 a)
>partial_derivate2y (Covector f) = covector $ partial_derivate ch f
>  where ch eps (Vector2 x y) = Vector2 x (y+eps)

>vector_vector2 :: [a] -> Vector2 a
>vector_vector2 [x,y] = Vector2 x y
>vector_vector2 _ = error "vector2: Invalid number of elements in vector"

>diagonal2 :: Matrix2 a -> Vector2 a
>diagonal2 (Matrix x) = diagonal_projections2 <*> x

>instance Indexable Vector2 where
>   diagonal_projections = diagonal_projections2
>   indexable_indices = Vector2 0 1

>instance Indexable (Vector2 :*: Vector2) where
>   indexable_indices = Matrix $ Vector2 (Vector2 0 1) (Vector2 3 4)
>   diagonal_projections = fmap (\f -> f . cells) $ 
>      matrix (.) diagonal_projections2 diagonal_projections2

>diagonal_projections2 :: Vector2 (Vector2 a -> a)
>diagonal_projections2 = Vector2 xcoord2 ycoord2

>transpose2 :: Matrix2 a -> Matrix2 a
>transpose2 (Matrix m) = matrix ($) diagonal_projections2 m

>rotate2 :: Vector2 a -> Vector2 a
>rotate2 (Vector2 x y) = Vector2 y x

>bind_diagonal2 :: Vector2 a -> (a -> Vector2 b) -> Vector2 b
>(Vector2 x y) `bind_diagonal2` f = Vector2 xx yy
>      where Vector2 xx _ = f x
>            Vector2 _ yy = f y

>bind_codiagonal2 :: Vector2 a -> (a -> Vector2 b) -> Vector2 b
>(Vector2 x y) `bind_codiagonal2` f = Vector2 xy yx
>      where Vector2 _ xy = f x
>            Vector2 yx _ = f y

>instance Functor Vector2 where 
>   fmap f (Vector2 x y) = Vector2 (f x) (f y)

>cross_product_scalar2 :: (Num a) => Vector2 a -> Vector2 a -> a
>cross_product_scalar2 (Vector2 x y) (Vector2 x' y') = (x*y') - (y*x')

>orthogonal_vector2 :: (Num a) => Vector2 a -> Vector2 a
>orthogonal_vector2 (Vector2 x y) = Vector2 y (negate x)

>cross_product2 :: (Num a) => (Vector1 :*: Vector2) a -> Vector2 a
>cross_product2 (Matrix (Vector1 v)) = orthogonal_vector2 v


>instance (Num a) => Num (Vector2 a) where
>  (Vector2 x y) + (Vector2 x' y') = Vector2 (x+x') (y+y')
>  (Vector2 x y) - (Vector2 x' y') = Vector2 (x-x') (y-y')
>  (Vector2 x y) * (Vector2 x' y') = error "Trying to calculate cross product of 2d vectors"
>  negate (Vector2 x y) = Vector2 (negate x) (negate y)
>  abs (Vector2 x y) = Vector2 (abs x) (abs y)
>  signum (Vector2 x y) = Vector2 (signum x) (signum y)
>  fromInteger i = error "fromInteger: Vector2 requires 2 components"

>left_multiply2 :: (Num a) => Vector2 a -> Matrix2 a -> Vector2 a
>left_multiply2 (Vector2 x y) (Matrix (Vector2 (Vector2 x1 y1)
>                                      (Vector2 x2 y2)))
>  = Vector2 (x*x1+y*y1)
>            (x*x2+y*y2)                               

>right_multiply2 :: (Num a) => Matrix2 a -> Vector2 a -> Vector2 a
>right_multiply2 (Matrix (Vector2 (Vector2 x1 y1)
>                         (Vector2 x2 y2))) (Vector2 x y)
>   = Vector2 (x1*x+x2*y)
>             (y1*x+y2*y)                  


>instance (Num a) => VectorSpace (Vector2 a) where
>  type Scalar (Vector2 a) = a
>  v %* (Vector2 x y) = Vector2 (inVector1 (v %*) x) (inVector1 (v %*) y)
>  (Vector2 x y) %+ (Vector2 x' y') = Vector2 (x+x') (y+y')
>  vzero = Vector2 0 0
>  vnegate (Vector2 x y) = Vector2 (negate x) (negate y)

>instance (ConjugateSymmetric a) => ConjugateSymmetric (Vector2 a) where
>   conj (Vector2 x y) = Vector2 (conj x) (conj y)

>instance (Floating a, ConjugateSymmetric a) => NormedSpace (Vector2 a) where
>  norm v = sqrt (v %. v)

>instance (Num a, ConjugateSymmetric a) => InnerProductSpace (Vector2 a) where
>  (Vector2 x y) %. (Vector2 x' y') = x*conj x' + y*conj y'

>instance (Num a) => SquareMatrix Vector2 a where
>  diagonal = diagonal2 
>  identity    = identity2
>  diagonal_matrix = diagonal_matrix2

>instance (Num a) => FiniteSquareMatrix Vector2 a where
>  determinant = determinant2 
>  trace = trace2 

>instance Transposable Vector2 Vector2 where
>  transpose   = transpose2

>instance {-# OVERLAPPABLE #-} (Num a) => LinearTransform Vector2 Vector2 a where
>  (<*>>) = left_multiply2
>  (<<*>) = right_multiply2

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
>  unit_vectors = [Vector2 1 0, Vector2 0 1]


>instance StandardBasis (Vector2 (Vector2 Int)) where
>  unit_vectors = [Vector2 (Vector2 1 0)
>                          (Vector2 0 0), 
>                  Vector2 (Vector2 0 1)
>                          (Vector2 0 0),
>                  Vector2 (Vector2 0 0)
>                          (Vector2 1 0),
>                  Vector2 (Vector2 0 0)
>                          (Vector2 0 1)]

>instance (Num a) => StandardBasis (Vector2 a) where
>  unit_vectors = [Vector2 1 0, Vector2 0 1]

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
>  where m = sqrt(tra * tra - 4 * determinant a)
>        tra = trace a

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
>
>instance (Num a) => VectorSpace ((Vector1 :*: Vector2) a) where
>  type Scalar ((Vector1 :*: Vector2) a) = a
>  vzero = Matrix $ Vector1 vzero 
>  vnegate (Matrix v) = Matrix $ fmap negate v
>  v %* (Matrix x) = Matrix $ fmap (fmap (v*)) x
>  (Matrix x) %+ (Matrix y) = Matrix $ x %+ y

>toSimple21 :: (Vector2 :*: Vector1) a -> (TwoD :&: OneD) a
>toSimple21 m = Matrix $ m <!> (vector2_to_vec2, vector1_to_vec1)

>toSimple22 :: (Vector2 :*: Vector2) a -> (TwoD :&: TwoD) a
>toSimple22 m = Matrix $ m <!> (vector2_to_vec2, vector2_to_vec2)

>toSimple12 :: (Vector1 :*: Vector2) a -> (OneD :&: TwoD) a
>toSimple12 m = Matrix $ m <!> (vector1_to_vec1, vector2_to_vec2)

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>leviCivita2 :: (Num a) => (Vector2 :*: Vector2) a
>leviCivita2 = Matrix $ Vector2 (Vector2 0 1) (Vector2 (-1) 0)
