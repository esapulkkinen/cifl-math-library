>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, TypeFamilies, Arrows, LambdaCase, DeriveAnyClass, ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
>{-# LANGUAGE QuantifiedConstraints #-}
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
>import Data.Type.Equality
>import Text.PrettyPrint (empty, vcat,hsep,(<+>))
>import qualified Text.PrettyPrint as Pretty
>import Data.Complex
>import Math.Tools.Universe
>import Math.Tools.PrettyP as Tools.PrettyP
>import Math.Tools.CoMonad
>import Math.Tools.NaturalTransformation
>import Math.Tools.Isomorphism
>import Math.Tools.I
>import Math.Tools.Arrow
>import Math.Matrix.Interface
>import Math.Matrix.Linear
>import Math.Matrix.Points
>import Math.Number.StreamInterface hiding (drop, take)

>type (row :&: col) elem = (row -> elem) :-> (col -> elem)
>
>type Matrix3S row col dep elem = (row -> dep -> elem)
>                             :-> (col :&: dep) elem

>type Matrix4S row col dep tim elem = (row -> (dep :&: tim) elem)
>                                 :-> Matrix3S col dep tim elem

>type MatrixA arr row col elem = (arr row elem) :-> (arr col elem)
>type SMatrix1 elem = (OneD :&: OneD) elem
>type SMatrix2 elem = (TwoD :&: TwoD) elem
>type SMatrix3 elem = (ThreeD :&: ThreeD) elem
>type SMatrix4 elem = (FourD :&: FourD) elem
>type InfMatrix elem = (Integer :&: Integer) elem
>type RatioMatrix elem = (Rational :&: Rational) elem

>instance (Universe a) => PpShowF ((->) a) where
>   ppf f = (hsep $ take 10 lst) <> if null lst2 then Pretty.empty else pp ".." 
>     where lst = [pp (f i) | i <- all_elements]
>           lst2 = drop 10 lst

>mapDimensions :: (Universe a, Universe b, Integral a, ConjugateSymmetric c, Num c)
> => (->) a :~> f -> (->) b :~> g -> (a :&: b) c -> (f :*: g) c
>mapDimensions col row m = Matrix $ nattrans_component col $
>    fmap (nattrans_component row) $ cells $ fromLinear m

>simpleMatrix :: (Integral a, Universe b, Universe a, ConjugateSymmetric c, Num c,Functor m, Functor n) =>
> (a :&: b) c -> m a -> n b -> (m :*: n) c
>simpleMatrix m = fromLinear m <!> (matrix,id)


No longer works, requires Num (c -> d)
dimensionFlip :: (Integral a, Functor m, Functor n) =>
 (a :&: b) (c -> d) -> m a -> n c -> (m :*: n) (b -> d)
dimensionFlip m = fromLinear m <!> (matrix,flip)

>instance (Universe a) => PpShowVerticalF ((->) a) where
>   ppf_vertical f = vcat $ (take 10 $ lst) ++ [if null (drop 10 lst) then Pretty.empty else pp ".."]
>     where lst = [pp (f j) | j <- all_elements]

instance (Universe row, Universe col, PpShow elem) => Show ((row :&: col) elem) where
   show m = render $ ppf_vertical (fmap ppf $ m)
   
instance (Universe row, Universe col, Universe dep, PpShow elem) => Show (Matrix3S row col dep elem) where
   show m = render $ ppf_vertical (fmap ppf m)

instance (Universe row, Universe col, Universe dep, Universe tim, PpShow elem) => Show (Matrix4S row col dep tim elem) where
   show m = render $ ppf_vertical $ m

>instance (Num a) => VectorSpace (((->) row :*: (->) col) a) where
>   type Scalar (((->) row :*: (->) col) a) = a
>   vzero = Matrix $ const $ const 0
>   (Matrix m) %+ (Matrix n) = Matrix $ \i j -> m i j + n i j
>   a %* (Matrix m) = Matrix $ \i j -> a * m i j

>instance (Num a) => VectorSpace (((->) row :*: Vector1) a) where
>   type Scalar (((->) row :*: Vector1) a) = a
>   vzero = Matrix $ const $ vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ \i -> m i %+ n i
>   a %* (Matrix m) = Matrix $ \i -> a %* m i
>instance (Num a) => VectorSpace (((->) row :*: Vector2) a) where
>   type Scalar (((->) row :*: Vector2) a) = a
>   vzero = Matrix $ const $ vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ \i -> m i %+ n i
>   a %* (Matrix m) = Matrix $ \i -> a %* m i
>instance (Num a) => VectorSpace (((->) row :*: Vector3) a) where
>   type Scalar (((->) row :*: Vector3) a) = a
>   vzero = Matrix $ const $ vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ \i -> m i %+ n i
>   a %* (Matrix m) = Matrix $ \i -> a %* m i
>instance (Num a) => VectorSpace (((->) row :*: Vector4) a) where
>   type Scalar (((->) row :*: Vector4) a) = a
>   vzero = Matrix $ const $ vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ \i -> m i %+ n i
>   a %* (Matrix m) = Matrix $ \i -> a %* m i

>instance (Num a) => VectorSpace ((Vector1 :*: (->) col) a) where
>   type Scalar ((Vector1 :*: (->) col) a) = a
>   vzero = Matrix $ Vector1 vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ liftA2 (liftA2 (+)) m n
>   a %* (Matrix (Vector1 x)) = Matrix $ Vector1 (a %* x)
>instance (Num a) => VectorSpace ((Vector2 :*: (->) col) a) where
>   type Scalar ((Vector2 :*: (->) col) a) = a
>   vzero = Matrix $ Vector2 vzero vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ liftA2 (liftA2 (+)) m n
>   a %* (Matrix (Vector2 x y)) = Matrix $ Vector2 (a %* x) (a %* y)
>instance (Num a) => VectorSpace ((Vector3 :*: (->) col) a) where
>   type Scalar ((Vector3 :*: (->) col) a) = a
>   vzero = Matrix $ Vector3 vzero vzero vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ liftA2 (liftA2 (+)) m n
>   a %* (Matrix (Vector3 x y z)) = Matrix $ Vector3 (a %* x) (a %* y) (a %* z)
>instance (Num a) => VectorSpace ((Vector4 :*: (->) col) a) where
>   type Scalar ((Vector4 :*: (->) col) a) = a
>   vzero = Matrix $ Vector4 vzero vzero vzero vzero
>   (Matrix m) %+ (Matrix n) = Matrix $ liftA2 (liftA2 (+)) m n
>   a %* (Matrix (Vector4 t x y z)) = Matrix $ Vector4 (a %* t) (a %* x) (a %* y) (a %* z)


>instance (Num a, Universe row, Universe col) => LinearTransform ((->) row) ((->) col) a where
>   colv <*>> (Matrix m) = \i -> sum [colv j * m i j | j <- all_elements]
>   (Matrix m) <<*> rowv = \j -> sum [m i j * rowv i | i <- all_elements]

>instance (Num a, Universe col) => LinearTransform Vector1 ((->) col) a where
>   f <*>> (Matrix (Vector1 n)) = Vector1 $ sum [f j * n j | j <- all_elements]
>   (Matrix (Vector1 m)) <<*> (Vector1 k) = k %* m

>instance (Universe row, Num a) => LinearTransform ((->) row) Vector1 a where
>   (Vector1 x) <*>> (Matrix m) = \i -> vector_element (m i)
>   (Matrix m) <<*> f = Vector1 $ sum [vector_element (m i) * f i | i <- all_elements]

>instance (Num a, Universe col) => LinearTransform Vector2 ((->) col) a where
>   f <*>> (Matrix (Vector2 x y)) = Vector2 (sum [f j * x j | j <- all_elements]) (sum [f j * y j | j <- all_elements])
>   (Matrix (Vector2 x y)) <<*> (Vector2 x' y') = \j -> x' * x j + y' * y j

>instance (Universe row, Num a) => LinearTransform ((->) row) Vector2 a where
>   (Vector2 x y) <*>> (Matrix m) = \i -> let mi = m i in x * xcoord2 mi + y * ycoord2 mi
>   (Matrix m) <<*> f = Vector2 (sum [xcoord2 (m i) * f i | i <- all_elements])
>                               (sum [ycoord2 (m i) * f i | i <- all_elements])

>instance (Num a, Universe col) => LinearTransform Vector3 ((->) col) a where
>   f <*>> (Matrix (Vector3 x y z)) = Vector3 (sum [f j * x j | j <- all_elements])
>                                             (sum [f j * y j | j <- all_elements])
>                                             (sum [f j * z j | j <- all_elements])
>   (Matrix (Vector3 x y z)) <<*> (Vector3 x' y' z') = \j -> x' * x j + y' * y j + z' * z j


>instance (Universe row, Num a) => LinearTransform ((->) row) Vector3 a where
>   (Vector3 x y z) <*>> (Matrix m) = \i -> let mi = m i in x * xcoord3 mi + y * ycoord3 mi + z * zcoord3 mi
>   (Matrix m) <<*> f = Vector3 (sum [f i * xcoord3 (m i) | i <- all_elements])
>                               (sum [f i * ycoord3 (m i) | i <- all_elements])
>                               (sum [f i * zcoord3 (m i) | i <- all_elements])

>instance (Universe col, Num a) => LinearTransform Vector4 ((->) col) a where
>   f <*>> (Matrix (Vector4 t x y z)) = Vector4 (sum [f j * t j | j <- all_elements])
>                                               (sum [f j * x j | j <- all_elements])
>                                               (sum [f j * y j | j <- all_elements])
>                                               (sum [f j * z j | j <- all_elements])
>   (Matrix (Vector4 t x y z)) <<*> (Vector4 t' x' y' z') = \j -> x' * x j + y' * y j + z' * z j + t' * t j


>instance (Universe row, Num a) => LinearTransform ((->) row) Vector4 a where
>   (Vector4 t x y z) <*>> (Matrix m) = \i -> let mi = m i in t * tcoord4 mi + x * xcoord4 mi + y * ycoord4 mi + z * zcoord4 mi
>   (Matrix m) <<*> f = Vector4 (sum [f i * tcoord4 (m i) | i <- all_elements])
>                               (sum [f i * xcoord4 (m i) | i <- all_elements])
>                               (sum [f i * ycoord4 (m i) | i <- all_elements])
>                               (sum [f i * zcoord4 (m i) | i <- all_elements])

>
>instance (Floating b, Universe a) => NormedSpace (a -> b) where
>   norm f = sqrt (f %. f)

instance Transposable ((->) row) ((->) col) a where
   transpose_impl (Matrix f) = Matrix $ \i j -> f j i

>index_delta :: (Eq ind, Num a) => ind -> a -> (ind -> a) -> (ind -> a)
>index_delta ind eps v = \ind' -> if ind == ind' then eps + v ind else v ind

>partial_derivate_ind :: (Integral ind, Universe ind, Eq ind, ConjugateSymmetric a, Closed a, Infinitesimal Stream a) => ind -> Dual (ind -> a) -> Dual (ind -> a)
>partial_derivate_ind ind' (Covector f) = Covector $ arr_linear $ Vector1 . partial_derivate (index_delta ind') (vector_element . appLinear f)

partial_derivate_list :: (Closed a, Infinitesimal a, Eq ind, Universe ind)
   => [Dual (ind -> a) -> Dual (ind -> a)]

>partial_derivate_list :: (Universe a, Eq a, Integral a, ConjugateSymmetric b, Closed b, Infinitesimal Stream b) => [Dual (a -> b) -> Dual (a -> b)]
>partial_derivate_list = map partial_derivate_ind all_elements

>divergence_index :: (ConjugateSymmetric b, Closed b, Infinitesimal Stream b, Integral ind, Eq ind, Universe a, Universe ind)
> => (ind -> b) :-> (a -> b) -> Dual (ind -> b)
>divergence_index f = vsum (partial_derivate_list <*> flst)
>   where flst = map (\i -> covector (($ i) . appLinear f)) all_elements

>grad_index :: (Scalar (ind -> a) ~ a, Integral ind, Universe ind, ConjugateSymmetric a, Closed a, Infinitesimal Stream a, Eq ind)
>  => Dual (ind -> a) -> (ind -> a) :-> (ind -> a)
>grad_index f = arr_linear $ \z ind -> (partial_derivate_ind ind f) `bracket` z

>laplace_index :: (ConjugateSymmetric a, Closed a, Infinitesimal Stream a, Integral ind,Eq ind, Universe ind)
>  => Dual (ind -> a) -> Dual (ind -> a)
>laplace_index f = divergence_index (grad_index f)

>instance (b ~ Scalar a, Scalar (a -> b) ~ b, Integral a, VectorSpace b, ConjugateSymmetric b, Closed b, Infinitesimal Stream b, Eq a, Universe a)
> => VectorDerivative (a -> b) Dual LinearMap where
>  grad = grad_index
>  divergence = divergence_index
>  laplace = laplace_index

>instance (Num a) => Num (FourD -> a) where
>  f + g = \i -> f i + g i
>  f - g = \i -> f i - g i
>  f * g = \i -> f i * g i
>  negate f = \i -> negate (f i)
>  abs f = \i -> abs (f i)
>  signum f = \i -> signum (f i)
>  fromInteger j = error "fromInteger: FourD -> a requires 4 components"
>instance (Num a) => Num (ThreeD -> a) where
>  f + g = \i -> f i + g i
>  f - g = \i -> f i - g i
>  f * g = \i -> f i * g i
>  negate f = \i -> negate (f i)
>  abs f = \i -> abs (f i)
>  signum f = \i -> signum (f i)
>  fromInteger j = error "fromInteger: FourD -> a requires 3 components"
>instance (Num a) => Num (TwoD -> a) where
>  f + g = \i -> f i + g i
>  f - g = \i -> f i - g i
>  f * g = \i -> f i * g i
>  negate f = \i -> negate (f i)
>  abs f = \i -> abs (f i)
>  signum f = \i -> signum (f i)
>  fromInteger j = error "fromInteger: FourD -> a requires 2 components"

>instance (Num a) => Num (OneD -> a) where
>  f + g = \i -> f i + g i
>  f - g = \i -> f i - g i
>  f * g = \i -> f i * g i
>  negate f = \i -> negate (f i)
>  abs f = \i -> abs (f i)
>  signum f = \i -> signum (f i)
>  fromInteger j = \_ -> fromInteger j

>-- | <https://en.wikipedia.org/wiki/Frobenius_inner_product>

>instance (Integral col, Integral row, Universe row, Universe col, Num a, ConjugateSymmetric a)
> => InnerProductSpace (((->) row :*: (->) col) a) where
>   (Matrix m) %. (Matrix n) = sum $
>      [(m i j) * conj (n i j) | i <- all_elements, j <- all_elements]

>sumS :: (Num b, Universe a) => (a -> b) -> b
>sumS f = sum [f i | i <- all_elements]

>productS :: (Num b, Universe a) => (a -> b) -> b
>productS f = product [f i | i <- all_elements]

>cov_index :: (Integral a, Universe a, Num (Scalar a), ConjugateSymmetric (Scalar a))
>  => a -> Dual (a -> Scalar a)
>cov_index x = covector $ \f -> f x

>instance (Integral row, Integral col, Floating a, Universe row, Universe col, ConjugateSymmetric a)
> => NormedSpace (((->) row :*: (->) col) a) where
>   norm m = sqrt (m %. m)

>instance (VectorSpace a, Integral col, Integral row, Universe row, ConjugateSymmetric a, Universe col, Floating a, Integral row, Scalar (col -> a) ~ a) => NormedSpace ((row :&: col) a) where
>   norm m = norm (fromLinear m)

>-- | <https://en.wikipedia.org/wiki/Kronecker_delta>
>kronecker_delta :: (Eq a, Num b) => a -> a -> b
>kronecker_delta i j = if i == j then 1 else 0

>instance (Num a, Eq dim, Integral dim) => Diagonalizable ((->) dim) a where
>   vector_dimension f = indexable_indices
>   identity = Matrix kronecker_delta
>   diagonal_impl (Matrix f) = \i -> f i i
>   diagonal_matrix_impl f = Matrix $ \i j -> if i == j then f i else 0
>
>instance (Integral row, Universe row, SupportsMatrixMultiplication ((->) row) ((->) row) ((->) row) a)
> => LieAlgebra ((row :&: row) a) where
>   x %<>% y = x %***% y %- y %***% x
>
>(%***%) :: (Integral row, Universe row, Universe col, Universe mid, SupportsMatrixMultiplication ((->) row) ((->) col) ((->) mid) a)
> => (row :&: mid) a -> (mid :&: col) a -> (row :&: col) a
>(%***%) = (%**%)

-- | "Lawvere, Rosebrugh: Sets for Mathematics", pg. 167.
instance (SupportsMatrixMultiplication ((->) x) ((->) x) ((->) x) a) => Semigroup ((x :&: x) a) where
   (<>) = (%***%)

-- | "Lawvere, Rosebrugh: Sets for Mathematics", pg. 167.
instance (SupportsMatrixMultiplication ((->) x) ((->) x) ((->) x) a) => Monoid ((x :&: x) a) where
   mempty = linear $ identity (vector_dimension vzero)
   mappend = (%***%)


>class CoFactorDimension dim where
>   cofactorDim :: (Num a, ConjugateSymmetric a) => dim -> (dim :&: dim) a -> a

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).
>leviCivita2S :: (Num a) => (((->) TwoD) :*: ((->) TwoD)) a
>leviCivita2S = Matrix $ \row col ->
>   leviCivita [fromIntegral (toInt2 row), fromIntegral (toInt2 col)]

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).
>leviCivita3S = Matrix $ \row -> Matrix $ \col dep ->
>   leviCivita [fromIntegral (toInt3 row),
>               fromIntegral (toInt3 col),
>               fromIntegral (toInt3 dep)]

>-- | <https://en.wikipedia.org/wiki/Levi-Civita_symbol>
>-- Note indices are from 0..(n-1).

leviCivita4S :: (Num a) => Matrix4S FourD FourD FourD FourD a

leviCivita4S :: (VectorSpace a, Fractional (Scalar a), ConjugateSymmetric a, Num a)
             => (FourD -> (FourD -> a) :-> (FourD -> a))
             :-> (FourD -> FourD -> a) :-> (FourD -> a) :-> (FourD -> a)

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
>crossProduct :: (Integral a, Monoid a) => [a] -> [a] -> [a]
>crossProduct u v = [sum [leviCivita [i,j `index_project` indexable_indices,k `index_project` indexable_indices]
>                    * j `index_project` u * k `index_project` v
>                    | k <- take (length v) diagonal_projections,
>                      j <- take (length u) diagonal_projections]
>                    | i <- [0..fromIntegral (length u-1)]]
  

>instance (Eq dim,Num dim, Integral dim, Universe dim, CoFactorDimension dim, ConjugateSymmetric a,Num a)
> => Traceable ((->) dim) a where
>   trace_impl m = sum [f i i | i <- all_elements]
>     where (Matrix f) = m
>   determinant_impl = sdeterminant . linear

>instance (Num a, Integral row) => Indexable ((->) row) a where
>   diagonal_projections = \i -> (\fi -> I (fi i)) <-> (\ (I v) j -> v)
>   indexable_indices = \i -> fromIntegral i
>

instance (Universe a, Integral a, InnerProductSpace b) => CoordinateSpace (a -> b) where
   type Coordinate (a -> b) = a
   index i f = f i
   listVector lst = \i -> lst !! fromIntegral (toInteger i)
   dimension_size (f :: a -> b) = length (all_elements :: [a])
   coordinates (f :: a -> b) = (all_elements :: [a])
   
>instance (Num a, Universe row, Integral row) => Summable ((->) row) a where
>   sum_coordinates = sumS
>
>instance (Integral col, Floating elem, Scalar (col -> elem) ~ elem, VectorSpace elem, Fractional (Scalar elem), Integral row, Universe row, Universe col, ConjugateSymmetric elem)
> => Num ((row :&: col) elem) where
>   f + g = linear $ Matrix $ \i j -> (f # (i, j)) + (g # (i, j))
>   f - g = linear $ Matrix $ \i j -> (f # (i, j)) - (g # (i, j))
>   f * g = linear $ Matrix $ \i j -> (f # (i,j)) * (g # (i,  j))
>   negate f = linear $ Matrix $ \i j -> negate $ f # (i, j)
>   abs f = linear $ Matrix $ \i j -> abs (f # (i,j))
>   signum x = (1 / norm x) %* x
>   fromInteger a = linear $ Matrix $ \i j -> fromInteger a

instance (Num elem,Universe col, Universe dep, Universe row) => VectorSpace (Matrix3S row col dep elem) where
   type Scalar (Matrix3S row col dep elem) = elem
   vzero = LinearMap $ \i -> LinearMap $ \j -> LinearMap $ \k -> 0
   vnegate f = fmap vnegate f
   f %+ g = liftA2 (%+) f g
   x %* f = fmap (x %*) f

instance (elem ~ Scalar ((row :&: col) elem), Integral row, ConjugateSymmetric elem,Num elem, Universe row, Universe col)
 => VectorSpace ((row :&: col) elem) where
   type Scalar ((row :&: col) elem) = elem
   vzero = linear $ Matrix $ \i j -> 0
   vnegate f = linear $ Matrix $ \i j -> negate (cells (fromLinear f) i j)
   f %+ g = linear $ Matrix $ \i j -> cells (fromLinear f) i j + cells (fromLinear g) i j
   x %* f = linear $ Matrix $ \i j -> x * cells (fromLinear f) i j

>smatrix :: (Universe col, Integral row, Universe row, ConjugateSymmetric c,Num c) => (a -> b -> c) -> (row -> a) -> (col -> b) -> (row :&: col) c
>smatrix f a b = linear $ Matrix $ \i j -> f (a i) (b j)

>swapRows :: (Num a, ConjugateSymmetric a,Eq row, Integral row, Universe row, Universe col) => row -> row -> (row :&: col) a -> (row :&: col) a
>swapRows i1 i2 m = smatrix (\x y -> (m # (x,y))) (swapIndex i1 i2) id

>mulRow :: (Eq row, ConjugateSymmetric a,Num a, Integral row, Universe row, Universe col) => a -> row -> (row :&: col) a -> (row :&: col) a
>mulRow k r m = linear $ Matrix $ \i j -> if i == r then k * (m # (i,j))
>                                          else m # (i,j)

>swapIndex :: (Eq a) => a -> a -> a -> a
>swapIndex i1 i2 i | i == i1 = i2
>                  | i == i2 = i1
>                  | otherwise = i
>smatrixA :: (Arrow arr, Diagonalizable (arr row) c,
> Linearizable LinearMap (:*:) (arr row) (arr col) c,
> LinearTransform (arr row) (arr col) c)
> => arr (a,b) c -> arr row a -> arr col b
>                        -> MatrixA arr row col c
>smatrixA f a b = linear $ Matrix $ proc i -> returnA -< (proc j -> f <<< (a *** b) -< (i,j))

>smatrix3 :: (a -> b -> c -> d)
>         -> (row -> a) -> (col -> b) -> (depth -> c)
>         -> ((->) row :*: ((->) col :*: (->) depth)) d
>smatrix3 f a b c = Matrix $ \i -> Matrix $ \j k -> f (a i) (b j) (c k)

>function_smatrix :: (Universe a, Universe b, Integral a, ConjugateSymmetric c,Num c) => (a -> b -> c) -> (a :&: b) c
>function_smatrix f = smatrix f id id
>
>smatrix_inverse_image :: (ConjugateSymmetric a,Num a, Integral row, Integral row', Universe col', Universe row',Universe row, Universe col)
> => ((row,col) -> (row',col'))
> -> (row' :&: col') a -> (row :&: col) a
>smatrix_inverse_image f m | Matrix m' <- fromLinear m = linear $ Matrix $ \i j -> let (i',j') = f (i,j)
>                                                        in m' i' j'

>inverse_image_smatrix :: (Integral row', Integral row, Universe row,
> ConjugateSymmetric a,Num a, Universe row', Universe col', Universe col)
> => (row -> row', col -> col')
>                      -> (row' :&: col') a -> (row :&: col) a
>inverse_image_smatrix (f,g) m | Matrix m' <- fromLinear m = smatrix m' f g
>

index_function_matrix :: (Universe col, Integral row, Universe row, Num (a -> c)) => (row -> b -> c)
                      -> (col -> a -> b)
                      -> (row :&: col) (a -> c)
index_function_matrix = smatrix (.)

>
>sapply :: (Integral (a -> b), ConjugateSymmetric b,Num b, Eq a, Universe a, Universe b) => ((a -> b) :&: a) b
>sapply = smatrix id id id
>
>(#) :: (ConjugateSymmetric a, Integral row, Num a, Universe row, Universe col) => (row :&: col) a -> (row,col) -> a
>f # (i,j) = fromLinear f `cells` i $ j

index3S :: Matrix3S row col dep elem -> (row,col,dep) -> elem
index3S m (i,j,k) = m `cells` i `cells` j $ k

index4S :: Matrix4S row col dep tim elem -> (row, col,dep,tim) -> elem
index4S m (i,j,k,t) = m `cells2` i `cells2` j `cells2` k $ t
  where cells2 :: LinearMap (f a) (g a) -> f (g a)
        cells2 = cells . fromLinear

>indexA :: (ArrowApply arr, Diagonalizable (arr row) a,
>     Linearizable LinearMap (:*:) (arr row) (arr col) a,
>     LinearTransform (arr row) (arr col) a)
> => MatrixA arr row col a -> arr (row,col) a
>indexA f = proc (i,j) -> do { r <- cells (fromLinear f) -< i ; r -<< j }
>
>sdeterminant2 :: (ConjugateSymmetric a, Num a) => (TwoD :&: TwoD) a -> a
>sdeterminant2 m = m # (0,0) * m # (1,1)
>                - m # (1,0) * m # (0,1)

>cofactor4_11 :: (ConjugateSymmetric a,Num a) => (FourD :&: FourD) a -> a
>cofactor4_11 = sdeterminant3 . inverse_image_smatrix (remove41, remove41)
>
>cofactor4_21 :: (ConjugateSymmetric a,Num a) => (FourD :&: FourD) a -> a
>cofactor4_21 = negate . sdeterminant3 . inverse_image_smatrix (remove42, remove41)

>cofactor4_31 :: (ConjugateSymmetric a,Num a) => (FourD :&: FourD) a -> a
>cofactor4_31 = sdeterminant3 . inverse_image_smatrix (remove43, remove41)

>cofactor4_41 :: (ConjugateSymmetric a,Num a) => (FourD :&: FourD) a -> a
>cofactor4_41 = negate . sdeterminant3 . inverse_image_smatrix (remove44, remove41)

>cofactor3_11 :: (ConjugateSymmetric a,Num a) => (ThreeD :&: ThreeD) a -> a
>cofactor3_11 = sdeterminant2 . inverse_image_smatrix (remove31, remove31)

>cofactor3_21 :: (ConjugateSymmetric a,Num a) => (ThreeD :&: ThreeD) a -> a
>cofactor3_21 = negate . sdeterminant2 . inverse_image_smatrix (remove32, remove31)

>cofactor3_31 :: (ConjugateSymmetric a,Num a) => (ThreeD :&: ThreeD) a -> a
>cofactor3_31 = sdeterminant2 . inverse_image_smatrix (remove33,remove31)

>cofactor2_11 :: (ConjugateSymmetric a,Num a) => (TwoD :&: TwoD) a -> a
>cofactor2_11 m = (inverse_image_smatrix (remove21,remove21) m) # (0,0)
>
>cofactor2_21 :: (ConjugateSymmetric a,Num a) => (TwoD :&: TwoD) a -> a
>cofactor2_21 m = negate $ inverse_image_smatrix (remove22,remove21) m # (0,0)

>cofactorDim2 :: (ConjugateSymmetric a,Num a) => TwoD -> (TwoD :&: TwoD) a -> a
>cofactorDim2 = svec2 cofactor2_11 cofactor2_21

>cofactorDim3 :: (ConjugateSymmetric a,Num a) => ThreeD -> (ThreeD :&: ThreeD) a -> a
>cofactorDim3 = svec3 cofactor3_11 cofactor3_21 cofactor3_31

>cofactorDim4 :: (ConjugateSymmetric a,Num a) => FourD -> (FourD :&: FourD) a -> a
>cofactorDim4 = svec4 cofactor4_11 cofactor4_21 cofactor4_31 cofactor4_41

>instance CoFactorDimension TwoD where
>   cofactorDim = cofactorDim2
>instance CoFactorDimension ThreeD where
>   cofactorDim = cofactorDim3
>instance CoFactorDimension FourD where
>   cofactorDim = cofactorDim4

>sdeterminant :: (ConjugateSymmetric a, Num a, Integral dim, Universe dim, CoFactorDimension dim)
>             => (dim :&: dim) a -> a
>sdeterminant m = sum $
>  map (\i -> m # (i,0) * cofactorDim i m) $ all_elements

determinant3S :: (Num a) => (ThreeD :&: ThreeD) a -> a
determinant3S f = sumS $ \i -> sumS $ \j -> sumS $ \k ->
   leviCivita3S `index3S` (i,j,k) * (f # (0,i)) * (f # (1,j)) * (f # (2,k))

determinantS :: (dim :&: dim) a -> a
determinantS f = leviCivita jlst * product [f <!> (i,j) | i <- all_elements]


>sdeterminant3 :: (Num a, ConjugateSymmetric a) => (ThreeD :&: ThreeD) a -> a
>sdeterminant3 m = (m # (0,0)) * cofactor3_11 m
>                  + (m # (1,0)) * cofactor3_21 m
>                  + (m # (2,0)) * cofactor3_31 m

>sdeterminant4 :: (Num a, ConjugateSymmetric a) => (FourD :&: FourD) a -> a
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


>svec_inf :: Stream a -> Integer -> a
>svec_inf ~(Pre x xr) = \case { 0 -> x ; i -> svec_inf xr (pred i) }
> 
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

>instance Comonad ((->) FiveD) where
>   extract f = f FiveD0
>   duplicate f = \a b -> f (a + b)
>
>instance Comonad ((->) SixD) where
>   extract f = f SixD0
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
>instance Universe SixD where
>   all_elements=[SixD0, SixD1, SixD2, SixD3, SixD4, SixD5]
>instance Universe SevenD where
>   all_elements=[SevenD0, SevenD1, SevenD2, SevenD3, SevenD4, SevenD5, SevenD6]
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
>instance Real SixD where
>   toRational = toRational . toInt6
>instance Real SevenD where
>   toRational = toRational . toInt7
>
>toInt7 :: SevenD -> Int
>toInt7 SevenD0 = 0
>toInt7 SevenD1 = 1
>toInt7 SevenD2 = 2
>toInt7 SevenD3 = 3
>toInt7 SevenD4 = 4
>toInt7 SevenD5 = 5
>toInt7 SevenD6 = 6

>toInt6 :: SixD -> Int
>toInt6 SixD0 = 0
>toInt6 SixD1 = 1
>toInt6 SixD2 = 2
>toInt6 SixD3 = 3
>toInt6 SixD4 = 4
>toInt6 SixD5 = 5

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
 

>fromInt7 :: Int -> SevenD
>fromInt7 0 = SevenD0
>fromInt7 1 = SevenD1
>fromInt7 2 = SevenD2
>fromInt7 3 = SevenD3
>fromInt7 4 = SevenD4
>fromInt7 5 = SevenD5
>fromInt7 6 = SevenD6
>fromInt7 i = fromInt7 (i `mod` 7)

>fromInt6 :: Int -> SixD
>fromInt6 0 = SixD0
>fromInt6 1 = SixD1
>fromInt6 2 = SixD2
>fromInt6 3 = SixD3
>fromInt6 4 = SixD4
>fromInt6 5 = SixD5
>fromInt6 i = fromInt6 (i `mod` 6)

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

>srotate_x :: (Floating a, ConjugateSymmetric a) => a -> SMatrix3 a
>srotate_x alfa = linear $ Matrix $ svec3 (svec3 1 0 0)
>                                (svec3 0 (cos alfa) (-sin alfa))
>                                (svec3 0 (sin alfa) (cos alfa))

>instance PpShow SevenD where { pp x = pp (toInt7 x) }
>instance PpShow SixD  where { pp x = pp (toInt6 x) }
>instance PpShow FiveD where { pp x = pp (toInt5 x) }
>instance PpShow FourD where { pp x = pp (toInt4 x) }
>instance PpShow ThreeD where { pp x = pp (toInt3 x) }
>instance PpShow TwoD where { pp x = pp (toInt2 x) }
>instance PpShow OneD where { pp x = pp (toInt1 x) }

>-- Being careful here not to have Show (a -> b) instance, which would
>-- violate Rice's theorem when 'a' is infinite. It's not nice to use
>-- the Universe constraint on Show instances, as it made things like
>-- ((+) :: Integer -> Integer -> Integer) showable, which is scary.
>instance (Show b) => Show (OneD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (TwoD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (ThreeD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (FourD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (FiveD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (SixD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (SevenD -> b) where
>   show f = show [f i | i <- all_elements]

>instance Show SevenD where { show x = show (toInt7 x) }
>instance Show SixD where { show x = show (toInt6 x) }
>instance Show FiveD where { show x = show (toInt5 x) }
>instance Show FourD where { show x = show (toInt4 x) }
>instance Show ThreeD where { show x = show (toInt3 x) }
>instance Show TwoD where { show x = show (toInt2 x) }
>instance Show OneD where { show x = show (toInt1 x) }

>instance Num SevenD where
>   x + y = fromInt7 (toInt7 x + toInt7 y)
>   x - y = fromInt7 (toInt7 x - toInt7 y)
>   x * y = fromInt7 (toInt7 x * toInt7 y)
>   negate = fromInt7 . negate . toInt7
>   abs = id
>   signum SevenD0 = 0
>   signum _ = 1
>   fromInteger = fromInt7 . fromIntegral
>
>instance Num SixD where
>   x + y = fromInt6 (toInt6 x + toInt6 y)
>   x - y = fromInt6 (toInt6 x - toInt6 y)
>   x * y = fromInt6 (toInt6 x * toInt6 y)
>   negate = fromInt6 . negate . toInt6
>   abs = id
>   signum SixD0 = 0
>   signum _ = 1
>   fromInteger = fromInt6 . fromIntegral
> 
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
>instance Integral SixD where
>   quot x y = fromInt6 (toInt6 x `quot` toInt6 y)
>   rem x y = fromInt6 (toInt6 x `rem` toInt6 y)
>   div x y = fromInt6 (toInt6 x `div` toInt6 y)
>   quotRem x y = let (a,b) = quotRem (toInt6 x) (toInt6 y) in (fromInt6 a,fromInt6 b)
>   divMod x y = let (a,b) = divMod (toInt6 x) (toInt6 y) in (fromInt6 a, fromInt6 b)
>   toInteger = toInteger . toInt6
>instance Integral SevenD where
>   quot x y = fromInt7 (toInt7 x `quot` toInt7 y)
>   rem x y = fromInt7 (toInt7 x `rem` toInt7 y)
>   div x y = fromInt7 (toInt7 x `div` toInt7 y)
>   quotRem x y = let (a,b) = quotRem (toInt7 x) (toInt7 y) in (fromInt7 a,fromInt7 b)
>   divMod x y = let (a,b) = divMod (toInt7 x) (toInt7 y) in (fromInt7 a, fromInt7 b)
>   toInteger = toInteger . toInt7

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


