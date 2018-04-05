>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Trustworthy,MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, FunctionalDependencies, FlexibleInstances, TypeOperators, TypeFamilies, DefaultSignatures, UnicodeSyntax #-}
>-- | These should match standard definitions of vector spaces.
>-- Used for reference: K. Chandrasekhara Rao: Functional Analysis.
>-- also see Warner: Modern algebra.
>module Math.Matrix.Interface where
>import Data.Monoid
>import Data.Ratio
>import Data.Complex
>import Data.List (intersperse)
>import qualified Data.Set
>import Control.Applicative
>import Control.Monad.Fix (fix)
>import Control.Parallel
>import Math.Tools.PrettyP
>import Math.Tools.Visitor
>import Math.Tools.FixedPoint
>import Math.Tools.Universe
>import Math.Tools.I

>infixl 7 %.%
>infixl 7 %*
>infixl 6 %+
>infixl 6 %-
>infixl 5 :*:
>
>-- | The primary data type for matrices.
>-- Note that indices are represented in the functors,
>-- If you want to use numeric indices, use 'Math.Matrix.Simple'. 
>data (f :*: g) a = Matrix { cells :: f (g a) }

>-- | <https://en.wikipedia.org/wiki/Matrix_%28mathematics%29>
>class (Num (Scalar v)) => VectorSpace v where
>  type Scalar v
>  vzero :: v
>  vnegate :: v -> v
>  (%+)  :: v -> v -> v -- sum
>  (%*)  :: Scalar v -> v -> v -- scalar product

>class (VectorSpace v, Scalar v ~ Complex a) => ComplexVectorSpace v a where

>class InnerProductSpace m where
>  (%.) :: m -> m -> Scalar m -- dot product/inner product

>-- | <https://en.wikipedia.org/wiki/Lie_algebra>
>class (VectorSpace m) => LieAlgebra m where
>  (%<>%) ::  m -> m -> m  -- [x,y]
>  
>  -- (a %* x %+ b %* y) %<>% z == a %* (x %<>% z) + b %* (y %<>% z)  [bilinearity]
>  -- x %<>% x == 0   [alternativity]
>  -- x %<>% (y %<>% z) + z %<>% (x %<>% y) + y %<>% (z %<>% x) == 0
>  --   [jacobi identity]
>  -- x %<>% y == - y %<>% x   [anticommutativity]

>class (VectorSpace m) => NormedSpace m where
>  norm :: m -> Scalar m

>class CompleteSpace m where

>class (Num m) => ConjugateSymmetric m where
>  conj :: m -> m

>class Linear f where
>  linear   :: f (Scalar (f a)) -> f a
>  unlinear :: f a -> f (Scalar (f a))

>class LinearTransform m n a where
>  (<*>>) :: n a -> (m :*: n) a -> m a -- ^ vector times matrix
>  (<<*>) :: (m :*: n) a -> m a -> n a -- ^ matrix times vector

>class (Scalar (f a) ~ a) => VectorLike f a

>class (Functor m, Functor n) => Transposable m n where
>  transpose :: (m :*: n) a -> (n :*: m) a

>type Index m a = m a -> a

>class (Applicative m) => Indexable m where
>  diagonal_projections :: m (Index m a)
>  indexable_indices :: (Integral a) => m a

>class (Indexable m) => Summable m where
>  sum_coordinates :: (Num a) => m a -> a

>-- | <https://en.wikipedia.org/wiki/Square_matrix>
>class (Transposable m m) => SquareMatrix m a where
>  identity :: (m :*: m) a
>  diagonal :: (m :*: m) a -> m a
>  diagonal_matrix :: m a -> (m :*: m) a

>class (SquareMatrix m a) => FiniteSquareMatrix m a where
>  determinant :: (m :*: m) a -> a
>  trace       :: (m :*: m) a -> a

>-- | <http://en.wikipedia.org/wiki/Adjugate>
>class (FiniteSquareMatrix m a) => InvertibleMatrix m a where
>   cofactor :: (m :*: m) a -> (m :*: m) a
>   adjucate :: (m :*: m) a -> (m :*: m) a
>   inverse  :: (m :*: m) a -> (m :*: m) a

>class (Functor m) => EigenDecomposable m a where
>  eigenvalues :: (m :*: m) a -> m a

>class (EigenDecomposable m a) => EigenVectorable m a where
>  eigenvectors :: (m :*: m) a -> (m :*: m) a

>class (Applicative m, Applicative n) => AppendableVector m n where
>  type (m :+: n) :: * -> *
>  (|>) :: m a -> n a -> (m :+: n) a

>class (AppendableVector m n) => SplittableVector m n where
>  vsplit   :: (m :+: n) a -> (m a, n a)

>-- | Iverson bracket: <http://en.wikipedia.org/wiki/Iverson_bracket>

>class Conditional a where
>  fromBoolean :: Bool -> a

>class StandardBasis m where
>  unit_vectors :: [m]

>class (VectorSpace v) => CoordinateSpace v where
>  type Coordinate v 
>  index  :: Coordinate v -> v -> Scalar v
>  listVector :: [Scalar v] -> v  -- convert list to vector
>  dimension_size :: v -> Int
>  coordinates :: v -> [Coordinate v]

>matrix :: (Functor m, Functor n) => (a -> b -> c) -> m a -> n b -> (m :*: n) c
>matrix f x y = Matrix $ flip fmap x $ \a -> 
>                        flip fmap y $ \b -> f a b

>matrixMatrix :: (Functor m, Functor n, Functor m', Functor n')
>             => (a -> b -> c)
>             -> (m :*: m') a
>             -> (n :*: n') b
>             -> ((m :*: n) :*: (m' :*: n')) c
>matrixMatrix f (Matrix x) (Matrix y) = Matrix $ (matrix . matrix) f x y

>invert_matrix :: (Eq a, Num a,InvertibleMatrix m a) => (m :*: m) a -> Maybe ((m :*: m) a)
>invert_matrix m | isInvertibleMatrix m = Just (inverse m)
>                | otherwise = Nothing

>isInvertibleMatrix :: (Eq a, Num a,FiniteSquareMatrix m a) => (m :*: m) a -> Bool
>isInvertibleMatrix m = determinant m /= 0

eigenvectors_generic :: (a ~ Scalar (n a),
                Fractional a, VectorSpace (n a),EigenDecomposable m a)
               => (m :*: m) a -> (n a -> n a) -> (m :*: n) a

eigenvectors_generic :: (Fractional (g a), EigenDecomposable f (g a))
   => (f :*: f) (g a) -> (g a -> g a) -> (f :*: g) a
eigenvectors_generic m a = Matrix $ fmap (fix . (a %/)) (eigenvalues m)

>data Basis m = Basis [m]

>(%-) :: (VectorSpace v) => v -> v -> v
>x %- y = x %+ (vnegate y)

>(%/) :: (Fractional (Scalar v),VectorSpace v) => v -> Scalar v -> v
>x %/ y = (1 / y) %* x

>-- | <https://en.wikipedia.org/wiki/Angle>

>angle :: (InnerProductSpace m, Floating (Scalar m))
>      => m -> m -> Scalar m
>angle x y = acos $
> (x %. y) / (innerproductspace_norm x * innerproductspace_norm y)

>-- | <https://en.wikipedia.org/wiki/Cross_product>
>-- This is a skew-symmetric matrix whose application to a vector
>-- is same as cross product of a with the vector.
>-- @cross_product_matrix v <<*> w == v %<>% w@.
>cross_product_matrix :: (SquareMatrix m a, LieAlgebra (m a)) => m a -> (m :*: m) a
>cross_product_matrix a = Matrix $ fmap (%<>% a) $ cells identity

>i_vec,j_vec,k_vec,l_vec :: (StandardBasis v) => v
>i_vec = unit_vectors !! 0
>j_vec = unit_vectors !! 1
>k_vec = unit_vectors !! 2
>l_vec = unit_vectors !! 3

>innerproductspace_norm :: (Floating (Scalar m), InnerProductSpace m)
>                       => m -> Scalar m
>innerproductspace_norm v = sqrt (v %. v)

>vsum :: (Foldable t, VectorSpace a) => t a -> a
>vsum = foldr (%+) vzero

>fold_rows :: (Functor m) => (n a -> b) -> (m :*: n) a -> m b
>fold_rows f (Matrix x) = fmap f x



>fold_columns :: (Functor n, Transposable m n) 
>              => (m a -> b) -> (m :*: n) a -> n b
>fold_columns f x = fold_rows f (transpose x)

>index_unit :: (StandardBasis v) => Int -> v
>index_unit i = unit_vectors !! i

>projection :: (Fractional (Scalar v), VectorSpace v, InnerProductSpace v)
>           => v -> v -> v
>projection e a = ((e %. a) / (e %. e)) %* e

>normalize :: (Fractional (Scalar a), NormedSpace a) => a -> a
>normalize x = (1 / norm x) %* x


isEigenValue :: (Num a,Eq a, SquareMatrix m a) => (m :*: m) a -> a -> Bool
isEigenValue m v = determinant (m %- (v %* identity)) == 0


>vector_length :: (Floating (Scalar m), InnerProductSpace m)
>              => m -> Scalar m
>vector_length x = sqrt (x %. x)

>divide :: (Fractional (Scalar v), VectorSpace v) => v -> Scalar v -> v
>divide v x = (1/x) %* v

>vaverage :: (Num v,Fractional (Scalar v),VectorSpace v) => [v] -> v
>vaverage lst = (1 / fromIntegral (length lst)) %* vsum lst

>toScalarList :: (StandardBasis m, InnerProductSpace m) => m -> [Scalar m]
>toScalarList m = [m %. c | c <- unit_vectors]

>class (PpShowF f) => PpShowVerticalF f where
>   ppf_vertical :: (PpShow a) => f a -> Doc

>instance (Functor f, PpShowVerticalF f, PpShowF g) => PpShowF (f :*: g) where
>	  ppf (Matrix x) = ppf_vertical $ fmap (nest 4 . ppf) x

>instance (PpShowF g, PpShowVerticalF f, Functor f, PpShow a) => PpShow ((f :*: g) a) where
>	  pp x = ppf x

>fromScalarList :: (VectorSpace a, StandardBasis a) => [Scalar a] -> a
>fromScalarList lst = vsum $ zipWith (%*) lst unit_vectors

>toListCS :: (CoordinateSpace v) => v -> [Scalar v]
>toListCS m = [index i m | i <- coordinates m]

>toListCS2 :: (CoordinateSpace m, CoordinateSpace (Scalar m)) 
>        => m -> [[Scalar (Scalar m)]]
>toListCS2 = map toListCS . toListCS

index2 :: (CoordinateSpace v, CoordinateSpace (Scalar v))
       => (Coordinate v,Coordinate (Scalar v)) 
       -> v -> Scalar (Scalar v)

index2 (row,col) (C e) = index col (index row e)

>coordinates2 :: (CoordinateSpace m, CoordinateSpace (Scalar m))
>         => m -> [[(Coordinate m,Coordinate (Scalar m))]]
>coordinates2 m = [[(x,y) | y <- coordinates (index x m)] | x <- coordinates m]

>basis_of :: (StandardBasis m) => Basis m
>basis_of = Basis unit_vectors

>listMatrix :: (CoordinateSpace n, CoordinateSpace (Scalar n)) 
>       => [[Scalar (Scalar n)]] -> n
>listMatrix m = listVector $ map listVector m



>-- | generalized implementation of matrix multiplication
>-- see <http://en.wikipedia.org/wiki/Matrix_multiplication>

>(%*%) :: (Functor g,Transposable h f,InnerProductSpace (h a))
>      => (g :*: h) a -> (h :*: f) a -> (g :*: f) (Scalar (h a))
>m1 %*% m2 = matrix (%.) (cells m1) (cells $ transpose m2)

>-- | In this version, we must assume Scalar (h a) ~ a constraint,
>-- but the result type is nicer.

>(%**%) :: (Functor g,Transposable h f,InnerProductSpace (h a),
>           Scalar (h a) ~ a)
>      => (g :*: h) a -> (h :*: f) a -> (g :*: f) a
>m1 %**% m2 = matrix (%.) (cells m1) (cells $ transpose m2)


>(%^%) :: (Functor h, SquareMatrix h b, InnerProductSpace (h b), b ~ Scalar (h b))
>      => (h :*: h) b -> Integer -> (h :*: h) b
>x %^% 0 = identity
>x %^% i = x %*% (x %^% (i-1)) 

>(|><|) :: (Functor m, Functor n, ConjugateSymmetric a)
>       => m a -> n a -> (m :*: n) a
>(|><|) = matrix $ \a b -> a * conj b

>identityCS :: (CoordinateSpace m, CoordinateSpace (Scalar m),
>               Num (Scalar (Scalar m))) 
>           => (Int,Int) -> m
>identityCS (b,a) = listVector [ listVector [ if i == j then 1 else 0 
>                                           | i <- [0..(a-1)]]
>                              | j<- [0..(b-1)]]

>(%.%) :: (Num (Scalar m), CoordinateSpace m) => m -> m -> Scalar m
>x %.% y = sum [ index i x * index i y | i <- coordinates x]

>basis_coordinates :: (InnerProductSpace v) => Basis v -> v -> [Scalar v]
>basis_coordinates (Basis basis) x = map (%. x) basis

>coordinateSpaceFunctionMatrix :: (CoordinateSpace m, StandardBasis v)
>               => (v -> Scalar m) -> m
>coordinateSpaceFunctionMatrix f = listVector $ map f $ unit_vectors

>-- | This is the linearity condition:

>functionMatrix :: (Functor f, SquareMatrix f a)
> => (f a -> g b) -> (f :*: g) b
>functionMatrix f = Matrix $ fmap f $ cells identity

>instance (Show v) => Show (Basis v) where
>  show (Basis lst) = show lst

>instance ConjugateSymmetric Integer where { conj = id }
>instance ConjugateSymmetric Int where { conj = id }
>instance ConjugateSymmetric Float where { conj = id }
>instance ConjugateSymmetric Double where { conj = id }
>instance (Integral a) => ConjugateSymmetric (Ratio a) where { conj = id }
>instance (RealFloat a) => ConjugateSymmetric (Complex a)where
>   conj = conjugate
>   
>instance (Integral a) => VectorSpace (Ratio a) where
>   type Scalar (Ratio a) = Ratio a
>   vzero = 0 % 1
>   vnegate r = negate r
>   n %* r = n * r
>   n %+ r = n + r

>instance (Integral a) => NormedSpace (Ratio a) where
>   norm z = abs z

>instance (VectorSpace k) => VectorSpace (Basis k) where
>   type Scalar (Basis k) = Scalar k
>   vzero = Basis []
>   vnegate (Basis lst) = Basis (map vnegate lst)
>   n %* (Basis lst) = Basis (map (n %*) lst)
>   (Basis lst) %+ (Basis lst') = Basis (lst ++ lst')


>instance (Num a) => VectorSpace [a] where
>   type Scalar [a] = a
>   vzero = []
>   vnegate x = map negate x
>   a %* [] = []
>   a %* (c:cr) = (a * c : a %* cr)
>   (c:cr) %+ (d:dr) = (c+d : cr %+ dr)
>   [] %+ lst = lst
>   lst %+ [] = lst

>instance VectorSpace Integer where
>   type Scalar Integer = Integer
>   vzero = 0
>   vnegate = negate
>   a %* b = a * b
>   a %+ b = a + b

>instance VectorSpace Int where
>   type Scalar Int = Int
>   vzero = 0
>   vnegate = negate
>   a %* b = a * b
>   a %+ b = a + b

>instance VectorSpace Float where
>   type Scalar Float = Float
>   vzero = 0
>   vnegate = negate 
>   a %* b = a * b
>   a %+ b = a + b

>instance VectorSpace Double where
>   type Scalar Double = Double
>   vzero = 0
>   vnegate = negate
>   a %* b = a * b
>   a %+ b = a + b

>instance (Num a) => VectorSpace (Endo a) where
>   type Scalar (Endo a) = a
>   vzero = Endo id
>   vnegate (Endo f) = Endo (negate . f)
>   a %* (Endo f) = Endo $ \x -> a * f x
>   (Endo f) %+ (Endo g) = Endo $ \x -> f x + g x

>instance (Num a, ConjugateSymmetric a, Universe a) => InnerProductSpace (Endo a) where
>   (Endo f) %. (Endo g) = sum [f x * conj (g x) | x <- all_elements]

>instance (RealFloat a) => VectorSpace (Complex a) where
>   type Scalar (Complex a) = a
>   vzero = 0
>   vnegate = negate
>   a %* b = (a :+ 0) * b
>   a %+ b = a + b

>instance (RealFloat a) => NormedSpace (Complex a) where
>   norm = magnitude

>instance (Num a) => VectorSpace (Maybe a) where
>   type Scalar (Maybe a) = a
>   vzero = Nothing
>   vnegate = maybe Nothing (Just . negate)
>   x %+ y = liftA2 (+) x y
>   a %* x = fmap (a *) x

>instance (Num a) => NormedSpace (Maybe a) where
>   norm Nothing = 0
>   norm (Just x) = abs x



>instance NormedSpace Integer where { norm = abs }
>instance NormedSpace Int where { norm = abs }
>instance NormedSpace Float where { norm = abs }
>instance NormedSpace Double where { norm = abs }
>instance (Integral a) => InnerProductSpace (Ratio a) where { (%.) = (*) }
>instance InnerProductSpace Integer where { (%.) = (*) }
>instance InnerProductSpace Int where { (%.) = (*) }
>instance InnerProductSpace Float where { (%.) = (*) }
>instance InnerProductSpace Double where { (%.) = (*) }

instance (Num a) => VectorSpace (([] :*: []) a) where
  type Scalar (([] :*: []) a) = a
  vzero = Matrix []
  vnegate (Matrix x) = Matrix $ map vnegate x
  v %* (Matrix x) = Matrix $ map (v %*) x
  (Matrix x) %+ (Matrix y) = Matrix $ zipWith (%+) x y

instance (Floating a) => NormedSpace [a] where
  norm lst = sqrt (sum $ map (\a -> a*a) lst)

>instance AppendableVector [] [] where
>  type ([] :+: []) = []
>  lst |> lst' = lst ++ lst'

>instance {-# OVERLAPPABLE #-}
>     (Show (f a)) => Show (([] :*: f) a) where
>  show (Matrix lst) = concat $ intersperse "\n" (map show lst)

>instance (Num v) => VectorSpace (x -> v) where
>   type Scalar (x -> v) = v
>   vzero = const 0
>   vnegate f = negate . f
>   a %* f = \i -> a * f i
>   f %+ g = \i -> f i + g i

>-- | <https://en.wikipedia.org/wiki/Commutator>
>instance (VectorSpace a, Num a) => LieAlgebra (a -> a) where
>   f %<>% g = (f . g) %- (g . f)

>instance (Num a) => LieAlgebra (Endo a) where
>   f %<>% g = (f <> g) %- (g <> f)

>instance (Integral a,Fractional a) => VectorSpace (Product a) where
>   type Scalar (Product a) = a
>   vzero = Product 1
>   vnegate (Product x) = Product (recip x)
>   a %* (Product x) = Product (x ^ a)
>   (Product x) %+ (Product y) = Product (x * y)

>instance (Num a) => VectorSpace (Sum a) where
>   type Scalar (Sum a) = a
>   vzero = Sum 0
>   vnegate (Sum x) = Sum (negate x)
>   a %* (Sum x) = Sum (a * x)
>   (Sum x) %+ (Sum y) = Sum (x + y)

>instance (Floating a, ConjugateSymmetric a) => NormedSpace (Sum a) where
>   norm x = sqrt (x %. x)

>instance (ConjugateSymmetric a,Num a) => InnerProductSpace (Sum a) where
>   (Sum x) %. (Sum y) = x * conj y

>instance (Num a) => VectorSpace (First a) where
>   type Scalar (First a) = a
>   vzero = First Nothing
>   vnegate (First m) = First $ maybe Nothing (Just . negate) m
>   a %* (First m) = First $ maybe Nothing (Just . (a*)) m
>   (First (Just m)) %+ (First (Just n)) = First (Just (m + n))
>   (First Nothing) %+ (First m) = First m
>   (First m) %+ (First Nothing) = First m

>instance (Floating a) => NormedSpace (First a) where
>   norm x = sqrt(x %. x)

>instance (Num a) => InnerProductSpace (First a) where
>   (First (Just x)) %. (First (Just y)) = x * y
>   (First Nothing) %. (First (Just y))  = y
>   (First (Just x)) %. (First Nothing)  = x
>   (First Nothing) %. (First Nothing)   = 0

>instance (Num a) => VectorSpace (Last a) where
>   type Scalar (Last a) = a
>   vzero = Last Nothing
>   vnegate (Last m) = Last $ maybe Nothing (Just . negate) m
>   a %* (Last m) = Last $ maybe Nothing (Just . (a*)) m
>   (Last (Just m)) %+ (Last (Just n)) = Last $ Just $ m + n
>   (Last Nothing) %+ (Last m) = Last m
>   (Last m) %+ (Last Nothing) = Last m

>instance (Integral a,Fractional a) => LieAlgebra (Product a) where
>   f %<>% g = (f <> g) %- (g <> f)

>instance (VectorSpace v, VectorSpace w, Scalar v ~ Scalar w)
>  => VectorSpace (v,w) where
>   type Scalar (v,w) = Scalar v
>   vzero = (vzero,vzero)
>   vnegate (v,w) = (vnegate v, vnegate w)
>   a %* (x,y) = (a %* x, a %* y)
>   (x,y) %+ (x',y') = (x %+ x', y %+ y')

>instance (Scalar v ~ Scalar w, Num (Scalar w), 
>         InnerProductSpace v, InnerProductSpace w)
>  => InnerProductSpace (v,w) where
>   (a,b) %. (c,d) = a %. c + b %. d

>instance (VectorSpace v, VectorSpace w,VectorSpace u, 
>  Scalar v ~ Scalar w, Scalar w ~ Scalar u)
> => VectorSpace (v,w,u) where
>   type Scalar (v,w,u) = Scalar v
>   vzero = (vzero,vzero,vzero)
>   vnegate (x,y,z) = (vnegate x, vnegate y,vnegate z)
>   a %* (x,y,z) = (a %* x, a %* y,a %* z)
>   (x,y,z) %+ (x',y',z') = (x %+ x', y %+ y',z %+ z')

>instance (Scalar v ~ Scalar w, Scalar w ~ Scalar u, Num (Scalar w),
>          InnerProductSpace v, InnerProductSpace w, InnerProductSpace u)
>  => InnerProductSpace (v,w,u) where
>    (a,b,c) %. (d,e,f) = a %. d + b %. e + c %. f

>instance (Universe a, Num b, ConjugateSymmetric b)
> => InnerProductSpace (a -> b) where
>   f %. g = sum [f i * conj (g i) | i <- all_elements]

this function is identically zero for hilbert spaces.

>hilbertSpace :: (Num m, NormedSpace m) => m -> m -> Scalar m
>hilbertSpace x y = norm(x+y)+norm(x-y) - 2*(norm x*norm x+norm y*norm y)

>lie_adjoint :: (LieAlgebra v) => v -> Endo v
>lie_adjoint x = Endo $ \y -> x %<>% y

>instance Conditional Integer where
>  fromBoolean True  = 1
>  fromBoolean False = 0

>instance Conditional Int where
>  fromBoolean True = 1
>  fromBoolean False = 0

>instance Conditional Float where
>  fromBoolean True = 1.0
>  fromBoolean False = 0.0

instance (Functor m) => Unital (:*:) m where
  type UUnit = I
  leftId = matrixLeftId
  rightId = matrixRightId

>instance Indexable [] where
>   diagonal_projections = head : map (. tail) diagonal_projections
>   indexable_indices = 0 : map (+1) indexable_indices

>-- | <https://en.wikipedia.org/wiki/Norm_(mathematics)>
>is_on_unit_circle :: (NormedSpace v, Eq (Scalar v)) => v -> Bool
>is_on_unit_circle v = norm v == 1

>-- | <https://en.wikipedia.org/wiki/Norm_(mathematics)>
>is_inside_unit_circle :: (NormedSpace v, Ord (Scalar v)) => v -> Bool
>is_inside_unit_circle v = norm v <= 1
