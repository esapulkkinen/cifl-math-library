>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, TypeFamilies, Arrows, LambdaCase, DeriveAnyClass, ExistentialQuantification, ScopedTypeVariables #-}
>-- | This module provides matrices with indices.
>-- Matrices are constructed using smatrix and svec operations, example:
>-- 
>-- @
>-- let m = smatrix (*) (svec3 1 2 3) (svec4 5 4 3 2) :: (ThreeD :&: FourD) Int
>-- == 5   4 3 2
>--    10  8 6 4
>--    15 12 9 6
>-- @
>--
>-- indexing occurs with the @(#)@ operator, for example, @m # (1,0) == 10@.
>-- 
>-- The number after svec is the number of elements in the dimension
>-- along row or column. Indices start from zero. The binary operation is applied to each pair
>-- of elements to construct the matrix element.
>--
>-- Matrices can also be constructed using @Matrix@ constructor, with
>-- indices as input, e.g.:
>-- 
>-- @identity :: (ThreeD :&: ThreeD) Integer@
>-- 
>-- @identity = Matrix $ \\i j -> if i == j then 1 else 0@
>--
>-- Note that index types are specifically restricted to be small types,
>-- since the Universe type class operations are often used to enumerate all elements
>-- of the type. This is enforced in show operations, which only show
>-- ten elements per dimension. For convenience, pre-defined index types,
>-- OneD, TwoD, ThreeD, FourD, FiveD are defined, for up to 5 dimensional matrices.
>-- 
>module Math.Matrix.Simple where
>import Prelude hiding (id,(.))
>import Control.Applicative
>import Control.Category
>import Control.Arrow
>import Text.PrettyPrint (empty, vcat,hsep,(<+>))
>import qualified Text.PrettyPrint as Pretty
>import Data.Complex
>import Math.Tools.Universe
>import Math.Tools.PrettyP as Tools.PrettyP
>import Math.Tools.CoMonad
>import Math.Tools.NaturalTransformation
>import Math.Tools.Isomorphism
>import Math.Matrix.Interface
>import Math.Matrix.Matrix
>import Math.Matrix.Covector
>import Math.Number.Real
>import Math.Number.Stream hiding (drop, take)

>type (row :&: col) elem = (((->) row) :*: ((->) col)) elem
>type Matrix3S row col dep elem = (((->) row) :*: (((->) col) :*: ((->) dep))) elem
>type Matrix4S row col dep tim elem = (((->) row) :*: (((->) col) :*: (((->) dep) :*: ((->) tim)))) elem
>type MatrixA arr row col elem = ((arr row) :*: (arr col)) elem
>type SMatrix1 elem = (OneD :&: OneD) elem
>type SMatrix2 elem = (TwoD :&: TwoD) elem
>type SMatrix3 elem = (ThreeD :&: ThreeD) elem
>type SMatrix4 elem = (FourD :&: FourD) elem
>type InfMatrix elem = (Integer :&: Integer) elem

>instance (Universe a) => PpShowF ((->) a) where
>   ppf f = (hsep $ take 10 lst) <> if null lst2 then Pretty.empty else pp ".." 
>     where lst = [pp (f i) | i <- all_elements]
>           lst2 = drop 10 lst

>mapDimensions :: (->) a :~> f -> (->) b :~> g -> (a :&: b) c -> (f :*: g) c
>mapDimensions col row (Matrix m) = Matrix $ nattrans_component col $
>    fmap (nattrans_component row) m

>simpleMatrix :: (Functor m, Functor n) =>
> (a :&: b) c -> m a -> n b -> (m :*: n) c
>simpleMatrix m = m <!> (matrix,id)


>dimensionFlip :: (Functor m, Functor n) =>
> (a :&: b) (c -> d) -> m a -> n c -> (m :*: n) (b -> d)
>dimensionFlip m = m <!> (matrix,flip)

>instance (Universe a) => PpShowVerticalF ((->) a) where
>   ppf_vertical f = vcat $ (take 10 $ lst) ++ [if null (drop 10 lst) then Pretty.empty else pp ".."]
>     where lst = [pp (f j) | j <- all_elements]
>
>instance (Universe row, Universe col, PpShow elem) => Show ((row :&: col) elem) where
>   show (Matrix m) = render $ ppf_vertical (fmap ppf m)
>   
>instance (Universe row, Universe col, Universe dep, PpShow elem) => Show (Matrix3S row col dep elem) where
>   show (Matrix m) = render $ ppf_vertical (fmap ppf m)
>
>instance (Universe row, Universe col, Universe dep, Universe tim, PpShow elem) => Show (Matrix4S row col dep tim elem) where
>   show (Matrix m) = render $ ppf_vertical m

>instance (Num a, Universe row, Universe col) => LinearTransform ((->) row) ((->) col) a where
>   colv <*>> (Matrix m) = \i -> sum [colv j * m i j | j <- all_elements]
>   (Matrix m) <<*> rowv = \j -> sum [m i j * rowv i | i <- all_elements]
>
>instance (Floating b, Universe a, ConjugateSymmetric b) => NormedSpace (a -> b) where
>   norm f = sqrt (f %. f)
>
>instance Transposable ((->) row) ((->) col) where
>   transpose (Matrix f) = Matrix $ \i j -> f j i

>index_delta :: (Eq ind, Num a) => ind -> a -> (ind -> a) -> (ind -> a)
>index_delta ind eps v = \ind' -> if ind == ind' then eps + v ind else v ind

>partial_derivate_ind :: (Closed a, Infinitesimal a, Eq ind)
>   => ind -> (Dual (ind -> a) -> Dual (ind -> a))
>partial_derivate_ind ind (Covector f) = covector $ partial_derivate (index_delta ind) f

>partial_derivate_list :: (Closed a, Infinitesimal a, Eq ind, Universe ind)
>   => [Dual (ind -> a) -> Dual (ind -> a)]
>partial_derivate_list = map partial_derivate_ind all_elements

>divergence_index :: (Scalar b ~ b, Closed b, Infinitesimal b, Eq ind, Universe a, Universe ind)
> => (LinearMap (ind -> b) (a -> b)) -> Dual (ind -> b)
>divergence_index (LinearMap f) = sum (partial_derivate_list <*> flst)
>   where flst = map (\i -> covector (($ i) . f)) all_elements

>grad_index :: (Scalar a ~ Scalar ind, Closed a, Infinitesimal a, Eq ind)
>  => Dual (ind -> a) -> LinearMap (ind -> a) (ind -> a)
>grad_index f = LinearMap $ \z ind -> partial_derivate_ind ind f `bracket` z

>laplace_index :: (a ~ Scalar ind, Scalar a ~ a, Closed a, Infinitesimal a, Eq ind, Universe ind)
>  => Dual (ind -> a) -> Dual (ind -> a)
>laplace_index f = divergence_index (grad_index f)

>instance (b ~ Scalar a, a ~ Scalar a, Closed b, Infinitesimal b, Eq a, Universe a)
> => VectorDerivative (a -> b) where
>  grad = grad_index
>  divergence = divergence_index
>  laplace = laplace_index

>-- | <https://en.wikipedia.org/wiki/Frobenius_inner_product>

>instance (Universe row, Universe col, Num a, ConjugateSymmetric a)
> => InnerProductSpace (((->) row :*: (->) col) a) where
>   (Matrix m) %. (Matrix n) = sum $
>      [(m i j) * conj (n i j) | i <- all_elements, j <- all_elements]

>sumS :: (Num b, Universe a) => (a -> b) -> b
>sumS f = sum [f i | i <- all_elements]

>productS :: (Num b, Universe a) => (a -> b) -> b
>productS f = product [f i | i <- all_elements]

>cov_index :: (b ~ Scalar b) => a -> Dual (a -> b)
>cov_index x = Covector $ \f -> f x
>
>instance (b ~ Scalar b) => ProjectionDual ((->) a) b where
>   projection_dual = cov_index

>instance (Floating a, Universe row, Universe col, ConjugateSymmetric a)
> => NormedSpace (((->) row :*: (->) col) a) where
>   norm m = sqrt (m %. m)

>-- | <https://en.wikipedia.org/wiki/Kronecker_delta>
>kronecker_delta :: (Eq a, Num b) => a -> a -> b
>kronecker_delta i j = if i == j then 1 else 0

>instance (Num a, Eq dim) => SquareMatrix ((->) dim) a where
>   identity = Matrix kronecker_delta
>   diagonal (Matrix f) = \i -> f i i
>   diagonal_matrix f = Matrix $ \i j -> if i == j then f i else 0
>
>instance (Num a, ConjugateSymmetric a,Universe row)
> => LieAlgebra ((row :&: row) a) where
>   x %<>% y = x %***% y %- y %***% x
>
>(%***%) :: (Universe mid, ConjugateSymmetric a, Num a)
> => (row :&: mid) a -> (mid :&: col) a -> (row :&: col) a
>(%***%) = (%**%)

>-- | "Lawvere, Rosebrugh: Sets for Mathematics", pg. 167.
>instance (Universe x, ConjugateSymmetric a, Num a) => Semigroup ((x :&: x) a) where
>   (<>) = (%***%)

>-- | "Lawvere, Rosebrugh: Sets for Mathematics", pg. 167.
>instance (Universe x, Eq x, ConjugateSymmetric a, Num a) => Monoid ((x :&: x) a) where
>   mempty = identity
>   mappend = (%***%)

>class CoFactorDimension dim where
>   cofactorDim :: (Num a) => dim -> (dim :&: dim) a -> a

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).
>leviCivita2S :: (Num a) => (TwoD :&: TwoD) a
>leviCivita2S = Matrix $ \row col ->
>   leviCivita [fromIntegral (toInt2 row), fromIntegral (toInt2 col)]

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).
>leviCivita3S :: (Num a) => Matrix3S ThreeD ThreeD ThreeD a
>leviCivita3S = Matrix $ \row -> Matrix $ \col dep ->
>   leviCivita [fromIntegral (toInt3 row),
>               fromIntegral (toInt3 col),
>               fromIntegral (toInt3 dep)]

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).
>leviCivita4S :: (Num a) => Matrix4S FourD FourD FourD FourD a
>leviCivita4S = Matrix $ \i -> Matrix $ \j -> Matrix $ \k l ->
>   leviCivita [fromIntegral (toInt4 i),fromIntegral (toInt4 j),
>               fromIntegral (toInt4 k),fromIntegral (toInt4 l)]


>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>leviCivitaS :: (Num b, Enum a, Universe a) => (a -> b) -> b
>leviCivitaS = \f -> product [signum ((f aj) - (f ai))
>                            | aj <- all_elements,
>                              ai <- map toEnum $ [0..fromEnum aj-1]]

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).
>leviCivita :: (Num a) => [a] -> a
>leviCivita = \lst -> product [signum ((lst !! aj) - (lst !! ai))
>                             | aj <- [0..length lst-1], ai <- [0..aj-1]]

>-- | <https://en.wikipedia.org/wiki/Cross_product>
>crossProductS :: (Integral a, Num b, Universe a) => (a -> b) -> (a -> b) -> a -> b
>crossProductS u v i = sum [fromIntegral (leviCivitaS (svec3 (toInteger i) (toInteger j) (toInteger k))) * u j * v k
>                          | j <- all_elements,
>                            k <- all_elements]

>-- | <https://en.wikipedia.org/wiki/Cross_product>
>crossProduct :: (Integral a) => [a] -> [a] -> [a]
>crossProduct u v = [sum [leviCivita [i,j indexable_indices,k indexable_indices]
>                    * j u * k v
>                    | k <- take (length v) diagonal_projections,
>                      j <- take (length u) diagonal_projections]
>                    | i <- [0..fromIntegral (length u-1)]]
  

>instance (Eq dim,Num dim, Universe dim, CoFactorDimension dim, Num a) => FiniteSquareMatrix ((->) dim) a where
>   trace (Matrix f) = sum [f i i | i <- all_elements]
>   determinant = sdeterminant
>

>instance (Integral row) => Indexable ((->) row) where
>   diagonal_projections = \i fi -> fi i
>   indexable_indices = \i -> fromIntegral i
>
>instance (Universe a, Integral a, Num b) => CoordinateSpace (a -> b) where
>   type Coordinate (a -> b) = a
>   index i f = f i
>   listVector lst = \i -> lst !! fromIntegral (toInteger i)
>   dimension_size (f :: a -> b) = length (all_elements :: [a])
>   coordinates (f :: a -> b) = (all_elements :: [a])
>   
>instance (Universe row, Integral row) => Summable ((->) row) where
>   sum_coordinates = sumS
>
>instance (Floating  elem, Universe row, Universe col, ConjugateSymmetric elem)
> => Num ((row :&: col) elem) where
>   (Matrix f) + (Matrix g) = Matrix $ \i j -> f i j + g i j
>   (Matrix f) - (Matrix g) = Matrix $ \i j -> f i j - g i j
>   (Matrix f) * (Matrix g) = Matrix $ \i j -> f i j * g i j
>   negate (Matrix f) = Matrix $ \i j -> negate (f i j)
>   abs (Matrix f) = Matrix $ \i j -> abs (f i j)
>   signum x = (1 / norm x) %* x
>   fromInteger a = Matrix $ \i j -> fromInteger a
>
>instance (Num elem,Universe col, Universe dep, Universe row) => VectorSpace (Matrix3S row col dep elem) where
>   type Scalar (Matrix3S row col dep elem) = elem
>   vzero = Matrix $ \i -> Matrix $ \j k -> 0
>   vnegate (Matrix f) = Matrix $ fmap vnegate f
>   (Matrix f) %+ (Matrix g) = Matrix $ liftA2 (%+) f g
>   x %* (Matrix f) = Matrix $ fmap (x %*) f

>instance (Num elem) => VectorSpace ((row :&: col) elem) where
>   type Scalar ((row :&: col) elem) = elem
>   vzero = Matrix $ \i j -> 0
>   vnegate (Matrix f) = Matrix $ \i j -> negate (f i j)
>   (Matrix f) %+ (Matrix g) = Matrix $ \i j -> f i j + g i j
>   x %* (Matrix f) = Matrix $ \i j -> x * f i j
>
>smatrix :: (a -> b -> c) -> (row -> a) -> (col -> b) -> (row :&: col) c
>smatrix f a b = Matrix $ \i j -> f (a i) (b j)

>swapRows :: (Eq row) => row -> row -> (row :&: col) a -> (row :&: col) a
>swapRows i1 i2 m = smatrix (\x y -> (m # (x,y))) (swapIndex i1 i2) id

>mulRow :: (Eq row, Num a) => a -> row -> (row :&: col) a -> (row :&: col) a
>mulRow k r m = Matrix $ \i j -> if i == r then k * (m # (i,j))
>                                          else m # (i,j)

>swapIndex :: (Eq a) => a -> a -> a -> a
>swapIndex i1 i2 i | i == i1 = i2
>                  | i == i2 = i1
>                  | otherwise = i
>smatrixA :: (Arrow arr) => arr (a,b) c -> arr row a -> arr col b
>                        -> MatrixA arr row col c
>smatrixA f a b = Matrix $ proc i -> returnA -< (proc j -> f <<< (a *** b) -< (i,j))

>smatrix3 :: (a -> b -> c -> d)
>         -> (row -> a) -> (col -> b) -> (depth -> c)
>         -> ((->) row :*: ((->) col :*: (->) depth)) d
>smatrix3 f a b c = Matrix $ \i -> Matrix $ \j k -> f (a i) (b j) (c k)

>function_smatrix :: (a -> b -> c) -> (a :&: b) c
>function_smatrix f = smatrix f id id
>
>smatrix_inverse_image :: ((row,col) -> (row',col'))
>                       -> (row' :&: col') a -> (row :&: col) a
>smatrix_inverse_image f (Matrix m) = Matrix $ \i j -> let (i',j') = f (i,j)
>                                                        in m i' j'

>inverse_image_smatrix :: (row -> row', col -> col')
>                      -> (row' :&: col') a -> (row :&: col) a
>inverse_image_smatrix (f,g) (Matrix m) = smatrix m f g
>
>index_function_matrix :: (row -> b -> c)
>                      -> (col -> a -> b)
>                      -> (row :&: col) (a -> c)
>index_function_matrix = smatrix (.)
>
>sapply :: ((a -> b) :&: a) b
>sapply = smatrix id id id
>
>(#) :: (row :&: col) a -> (row,col) -> a
>f # (i,j) = f `cells` i $ j

>index3S :: Matrix3S row col dep elem -> (row,col,dep) -> elem
>index3S m (i,j,k) = m `cells` i `cells` j $ k

>index4S :: Matrix4S row col dep tim elem -> (row, col,dep,tim) -> elem
>index4S m (i,j,k,t) = m `cells` i `cells` j `cells` k $ t
>
>indexA :: (ArrowApply arr) => MatrixA arr row col a -> arr (row,col) a
>indexA (Matrix f) = proc (i,j) -> do { r <- f -< i ; r -<< j }
>
>sdeterminant2 :: (Num a) => (TwoD :&: TwoD) a -> a
>sdeterminant2 m = m # (0,0) * m # (1,1)
>                - m # (1,0) * m # (0,1)

>cofactor4_11 :: (Num a) => (FourD :&: FourD) a -> a
>cofactor4_11 = sdeterminant3 . inverse_image_smatrix (remove41, remove41)
>
>cofactor4_21 :: (Num a) => (FourD :&: FourD) a -> a
>cofactor4_21 = negate . sdeterminant3 . inverse_image_smatrix (remove42, remove41)

>cofactor4_31 :: (Num a) => (FourD :&: FourD) a -> a
>cofactor4_31 = sdeterminant3 . inverse_image_smatrix (remove43, remove41)

>cofactor4_41 :: (Num a) => (FourD :&: FourD) a -> a
>cofactor4_41 = negate . sdeterminant3 . inverse_image_smatrix (remove44, remove41)

>cofactor3_11 :: (Num a) => (ThreeD :&: ThreeD) a -> a
>cofactor3_11 = sdeterminant2 . inverse_image_smatrix (remove31, remove31)

>cofactor3_21 :: (Num a) => (ThreeD :&: ThreeD) a -> a
>cofactor3_21 = negate . sdeterminant2 . inverse_image_smatrix (remove32, remove31)

>cofactor3_31 :: (Num a) => (ThreeD :&: ThreeD) a -> a
>cofactor3_31 = sdeterminant2 . inverse_image_smatrix (remove33,remove31)

>cofactor2_11 :: (Num a) => (TwoD :&: TwoD) a -> a
>cofactor2_11 m = (inverse_image_smatrix (remove21,remove21) m) # (0,0)
>
>cofactor2_21 :: (Num a) => (TwoD :&: TwoD) a -> a
>cofactor2_21 m = negate $ inverse_image_smatrix (remove22,remove21) m # (0,0)

>cofactorDim2 :: (Num a) => TwoD -> (TwoD :&: TwoD) a -> a
>cofactorDim2 = svec2 cofactor2_11 cofactor2_21

>cofactorDim3 :: (Num a) => ThreeD -> (ThreeD :&: ThreeD) a -> a
>cofactorDim3 = svec3 cofactor3_11 cofactor3_21 cofactor3_31

>cofactorDim4 :: (Num a) => FourD -> (FourD :&: FourD) a -> a
>cofactorDim4 = svec4 cofactor4_11 cofactor4_21 cofactor4_31 cofactor4_41

>instance CoFactorDimension TwoD where
>   cofactorDim = cofactorDim2
>instance CoFactorDimension ThreeD where
>   cofactorDim = cofactorDim3
>instance CoFactorDimension FourD where
>   cofactorDim = cofactorDim4

>sdeterminant :: (Num a, Num dim, Universe dim, CoFactorDimension dim)
>             => (dim :&: dim) a -> a
>sdeterminant m = sum $
>  map (\i -> m # (i,0) * cofactorDim i m) $ all_elements

>determinant3S :: (Num a) => (ThreeD :&: ThreeD) a -> a
>determinant3S f = sumS $ \i -> sumS $ \j -> sumS $ \k ->
>   leviCivita3S `index3S` (i,j,k) * (f # (0,i)) * (f # (1,j)) * (f # (2,k))

determinantS :: (dim :&: dim) a -> a
determinantS f = leviCivita jlst * product [f <!> (i,j) | i <- all_elements]


>sdeterminant3 :: (Num a) => (ThreeD :&: ThreeD) a -> a
>sdeterminant3 m = (m # (0,0)) * cofactor3_11 m
>                  + (m # (1,0)) * cofactor3_21 m
>                  + (m # (2,0)) * cofactor3_31 m

>sdeterminant4 :: (Num a) => (FourD :&: FourD) a -> a
>sdeterminant4 m = m # (0,0) * cofactor4_11 m
>                + m # (1,0) * cofactor4_21 m
>                + m # (2,0) * cofactor4_31 m
>                + m # (3,0) * cofactor4_41 m

The remove operations are useful with inverse_image operations
to map indices.

>remove41 :: ThreeD -> FourD
>remove41 = svec3 FourD1 FourD2 FourD3

>remove42 :: ThreeD -> FourD
>remove42 = svec3 FourD0 FourD2 FourD3

>remove43 :: ThreeD -> FourD
>remove43 = svec3 FourD0 FourD1 FourD3

>remove44 :: ThreeD -> FourD
>remove44 = svec3 FourD0 FourD1 FourD2

>remove31 :: TwoD -> ThreeD
>remove31 = svec2 ThreeD1 ThreeD2
>
>remove32 :: TwoD -> ThreeD
>remove32 = svec2 ThreeD0 ThreeD2
>
>remove33 :: TwoD -> ThreeD
>remove33 = svec2 ThreeD0 ThreeD1

>remove21 :: OneD -> TwoD
>remove21 = svec1 TwoD1

>remove22 :: OneD -> TwoD
>remove22 = svec1 TwoD0

>svec1 :: a -> OneD -> a
>svec1 x = \case { OneD0 -> x }

>svec2 :: a -> a -> TwoD -> a
>svec2 x y = \case { TwoD0 -> x ; TwoD1 -> y }

>svec3 :: a -> a -> a -> ThreeD -> a
>svec3 x y z = \case { ThreeD0 -> x; ThreeD1 -> y ; ThreeD2 -> z }

>svec4 :: a -> a -> a -> a -> FourD -> a
>svec4 x y z t = \case { FourD0 -> x; FourD1 -> y ; FourD2 -> z ; FourD3 -> t }
>
>svec5 :: a -> a -> a -> a -> a -> FiveD -> a
>svec5 x y z t u = \case { FiveD0 -> x ; FiveD1 -> y ; FiveD2 -> z ;
>                          FiveD3 -> t ; FiveD4 -> u }


>data OneD = OneD0
>  deriving (Eq,Ord,Enum)
>data TwoD = TwoD0 | TwoD1
>  deriving (Eq,Ord,Enum)
>data ThreeD = ThreeD0 | ThreeD1 | ThreeD2
>  deriving (Eq,Ord,Enum)
>data FourD = FourD0 | FourD1 | FourD2 | FourD3
>  deriving (Eq,Ord,Enum)
>data FiveD = FiveD0 | FiveD1 | FiveD2 | FiveD3 | FiveD4
>  deriving (Eq,Ord,Enum)

>instance Comonad ((->) TwoD) where
>   extract f = f TwoD0
>   duplicate f = \a b -> f (a + b)
>
>instance Comonad ((->) ThreeD) where
>   extract f = f ThreeD0
>   duplicate f = \a b -> f (a + b)
>
>instance Comonad ((->) FourD) where
>   extract f = f FourD0
>   duplicate f = \a b -> f (a + b)


>instance Universe OneD where
>   all_elements=[OneD0]
>instance Universe TwoD where
>   all_elements=[TwoD0,TwoD1]
>instance Universe ThreeD where
>   all_elements=[ThreeD0,ThreeD1,ThreeD2]
>instance Universe FourD where
>   all_elements=[FourD0,FourD1,FourD2,FourD3]
>instance Universe FiveD where
>   all_elements=[FiveD0, FiveD1, FiveD2, FiveD3, FiveD4]
>
>
>instance Real OneD where
>   toRational = toRational . toInt1
>instance Real TwoD where
>   toRational = toRational . toInt2
>instance Real ThreeD where
>   toRational = toRational . toInt3
>instance Real FourD where
>   toRational = toRational . toInt4
>instance Real FiveD where
>   toRational = toRational . toInt5
>
>toInt5 :: FiveD -> Int
>toInt5 FiveD0 = 0
>toInt5 FiveD1 = 1
>toInt5 FiveD2 = 2
>toInt5 FiveD3 = 3
>toInt5 FiveD4 = 4

>toInt4 :: FourD -> Int
>toInt4 FourD0 = 0
>toInt4 FourD1 = 1
>toInt4 FourD2 = 2
>toInt4 FourD3 = 3
>toInt3 :: ThreeD -> Int
>toInt3 ThreeD0 = 0
>toInt3 ThreeD1 = 1
>toInt3 ThreeD2 = 2
>toInt2 :: TwoD -> Int
>toInt2 TwoD0 = 0
>toInt2 TwoD1 = 1
>toInt1 :: OneD -> Int
>toInt1 _ = 0
 
>fromInt5 :: Int -> FiveD
>fromInt5 0 = FiveD0
>fromInt5 1 = FiveD1
>fromInt5 2 = FiveD2
>fromInt5 3 = FiveD3
>fromInt5 4 = FiveD4
>fromInt5 i = fromInt5 (i `mod` 5)
> 
>fromInt4 :: Int -> FourD
>fromInt4 0 = FourD0
>fromInt4 1 = FourD1
>fromInt4 2 = FourD2
>fromInt4 3 = FourD3
>fromInt4 i = fromInt4 (i `mod` 4)
> 
>fromInt3 :: Int -> ThreeD
>fromInt3 0 = ThreeD0
>fromInt3 1 = ThreeD1
>fromInt3 2 = ThreeD2
>fromInt3 i = fromInt3 (i `mod` 3)
> 
>fromInt2 :: Int -> TwoD
>fromInt2 0 = TwoD0
>fromInt2 1 = TwoD1
>fromInt2 i = fromInt2 (i `mod` 2)
> 
>fromInt1 :: Int -> OneD
>fromInt1 _ = OneD0

>srotate_x :: (Floating a) => a -> SMatrix3 a
>srotate_x alfa = Matrix $ svec3 (svec3 1 0 0)
>                                (svec3 0 (cos alfa) (-sin alfa))
>                                (svec3 0 (sin alfa) (cos alfa))

>instance PpShow FiveD where { pp x = pp (toInt5 x) }
>instance PpShow FourD where { pp x = pp (toInt4 x) }
>instance PpShow ThreeD where { pp x = pp (toInt3 x) }
>instance PpShow TwoD where { pp x = pp (toInt2 x) }
>instance PpShow OneD where { pp x = pp (toInt1 x) }


>instance (Show b, Universe a) => Show (a -> b) where
>   show f = show [f i | i <- all_elements]

>instance Show FiveD where
>   show x = show (toInt5 x)

>instance Show FourD where
>   show x = show (toInt4 x)

>instance Show ThreeD where
>   show x = show (toInt3 x)
>instance Show TwoD where
>   show x = show (toInt2 x)
>instance Show OneD where
>   show x = show (toInt1 x)

>instance Num FiveD where
>   x + y = fromInt5 (toInt5 x + toInt5 y)
>   x - y = fromInt5 (toInt5 x - toInt5 y)
>   x * y = fromInt5 (toInt5 x * toInt5 y)
>   negate = fromInt5 . negate . toInt5 
>   abs x = x
>   signum FiveD0 = 0
>   signum _ = 1
>   fromInteger = fromInt5 . fromIntegral

>instance Num FourD where
>   x + y = fromInt4 (toInt4 x + toInt4 y)
>   x - y = fromInt4 (toInt4 x - toInt4 y)
>   x * y = fromInt4 (toInt4 x * toInt4 y)
>   negate x = fromInt4 (negate (toInt4 x))
>   abs x = x
>   signum FourD0 = 0
>   signum _ = 1
>   fromInteger = fromInt4 . fromIntegral
>
>instance Integral ThreeD where
>   quot x y = fromInt3 (toInt3 x `quot` toInt3 y)
>   rem x y = fromInt3 (toInt3 x `rem` toInt3 y)
>   div x y = fromInt3 (toInt3 x `div` toInt3 y)
>   quotRem x y = let (a,b) = quotRem (toInt3 x) (toInt3 y) in (fromInt3 a,fromInt3 b)
>   divMod x y = let (a,b) = divMod (toInt3 x) (toInt3 y) in (fromInt3 a, fromInt3 b)
>   toInteger = toInteger . toInt3
>instance Integral FourD where
>   quot x y = fromInt4 (toInt4 x `quot` toInt4 y)
>   rem x y = fromInt4 (toInt4 x `rem` toInt4 y)
>   div x y = fromInt4 (toInt4 x `div` toInt4 y)
>   quotRem x y = let (a,b) = quotRem (toInt4 x) (toInt4 y) in (fromInt4 a,fromInt4 b)
>   divMod x y = let (a,b) = divMod (toInt4 x) (toInt4 y) in (fromInt4 a, fromInt4 b)
>   toInteger = toInteger . toInt4
>instance Integral FiveD where
>   quot x y = fromInt5 (toInt5 x `quot` toInt5 y)
>   rem x y = fromInt5 (toInt5 x `rem` toInt5 y)
>   div x y = fromInt5 (toInt5 x `div` toInt5 y)
>   quotRem x y = let (a,b) = quotRem (toInt5 x) (toInt5 y) in (fromInt5 a,fromInt5 b)
>   divMod x y = let (a,b) = divMod (toInt5 x) (toInt5 y) in (fromInt5 a, fromInt5 b)
>   toInteger = toInteger . toInt5

>instance Integral OneD where
>   quot x y = fromInt1 (toInt1 x `quot` toInt1 y)
>   rem x y = fromInt1 (toInt1 x `rem` toInt1 y)
>   div x y = fromInt1 (toInt1 x `div` toInt1 y)
>   quotRem x y = let (a,b) = quotRem (toInt1 x) (toInt1 y) in (fromInt1 a,fromInt1 b)
>   divMod x y = let (a,b) = divMod (toInt1 x) (toInt1 y) in (fromInt1 a, fromInt1 b)
>   toInteger = toInteger . toInt1


>instance Integral TwoD where
>   quot x y = fromInt2 (toInt2 x `quot` toInt2 y)
>   rem x y = fromInt2 (toInt2 x `rem` toInt2 y)
>   div x y = fromInt2 (toInt2 x `div` toInt2 y)
>   quotRem x y = let (a,b) = quotRem (toInt2 x) (toInt2 y) in (fromInt2 a,fromInt2 b)
>   divMod x y = let (a,b) = divMod (toInt2 x) (toInt2 y) in (fromInt2 a, fromInt2 b)
>   toInteger = toInteger . toInt2

>   
>instance Num ThreeD where
>   x + y = fromInt3 (toInt3 x + toInt3 y)
>   x - y = fromInt3 (toInt3 x - toInt3 y)
>   x * y = fromInt3 (toInt3 x * toInt3 y)
>   negate x = fromInt3 (negate (toInt3 x))
>   abs x = x
>   signum ThreeD0 = 0
>   signum _ = 1
>   fromInteger = fromInt3 . fromIntegral
>   
>instance Num TwoD where
>   x + y = fromInt2 (toInt2 x + toInt2 y)
>   x - y = fromInt2 (toInt2 x - toInt2 y)
>   x * y = fromInt2 (toInt2 x * toInt2 y)
>   negate x = fromInt2 (negate (toInt2 x))
>   abs x = x
>   signum TwoD0 = 0
>   signum _ = 1
>   fromInteger = fromInt2 . fromIntegral
>   
>instance Num OneD where
>   _ + _ = OneD0
>   _ - _ = OneD0
>   _ * _ = OneD0
>   negate _ = OneD0
>   abs _ = OneD0
>   signum _ = OneD0
>   fromInteger _ = OneD0


