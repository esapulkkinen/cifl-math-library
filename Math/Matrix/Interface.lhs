>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,MultiParamTypeClasses, ScopedTypeVariables #-}
>{-# LANGUAGE FlexibleContexts, FunctionalDependencies, FlexibleInstances #-}
>{-# LANGUAGE TypeOperators, TypeFamilies, DefaultSignatures #-}
>{-# LANGUAGE UnicodeSyntax, DeriveGeneric, DeriveDataTypeable #-}
>{-# LANGUAGE ConstraintKinds, UndecidableInstances, OverloadedStrings #-}
>{-# LANGUAGE QuantifiedConstraints #-}
>{-# LANGUAGE GADTs, AllowAmbiguousTypes, PolyKinds, RankNTypes #-}
>-- | These should match standard definitions of vector spaces.
>-- Used for reference: K. Chandrasekhara Rao: Functional Analysis.
>-- also see Warner: Modern algebra.
>module Math.Matrix.Interface where
>import safe GHC.Generics hiding ((:*:),(:+:))
>import safe Text.PrettyPrint hiding ((<>))
>import safe Data.Data
>import safe Data.Kind (Type)
>import safe Data.Typeable
>import safe Data.Monoid hiding (Dual, Endo)
>import safe Data.Ratio
>import safe Data.Traversable
>import safe Data.Complex
>import safe Data.Foldable
>import safe Data.List (intersperse)
>import safe Data.Word
>import safe Data.Int
>import safe qualified Data.Set
>import safe Prelude hiding (id,(.))
>import safe Control.Category
>import safe Control.Applicative
>import safe qualified Control.Arrow as Arrow
>import safe Data.Type.Equality
>import safe qualified Control.Applicative as Applicative
>import safe Control.Monad.Fix (fix)
>import safe Control.Monad (join, MonadPlus(..))
>import safe Data.Functor.Contravariant
>import safe Math.Tools.PrettyP
>import safe Math.Tools.Visitor
>import safe Math.Tools.FixedPoint
>import safe Math.Tools.Universe
>import safe Math.Tools.I
>import safe Math.Tools.CoFunctor
>import safe Math.Tools.Arrow
>import safe Math.Tools.Isomorphism
>import safe Math.Tools.Endomorphism

>infixl 7 %.%
>infix 7 %*
>infix 7 %.
>infixl 6 %+
>infixl 6 %-
>infixr 5 :*:
>
>-- | The primary data type for matrices.
>-- Note that indices are represented in the functors,
>-- If you want to use numeric indices, use 'Math.Matrix.Simple'.
>-- This can be viewed as a bicategory with one object ('a').
>-- <https://ncatlab.org/nlab/show/bicategory>
>-- or as a monoidal category <https://ncatlab.org/nlab/show/monoidal+category>
>-- or as a matrix over a base field 'a' <https://ncatlab.org/nlab/show/matrix>
>-- or as a tensor product of functors 'f' and 'g' <https://ncatlab.org/nlab/show/tensor+product>
>-- or as a composition of functors 'f' and 'g'
>newtype (f :*: g) a = Matrix { cells :: f (g a) }
>  deriving (Typeable, Generic, Eq)

matrixLinearmap :: (Scalar (f (g a)) ~ a) => ((Dual :*: f) :*: g) a -> ((f :*: g) a) :-> I a
matrixLinearmap (Matrix (Matrix (Covector m))) = m . LinearMap Refl cells



instance FunctorArrow f LinearMap where
  amap (LinearMap p m) = LinearMap Refl $ Matrix $ liftA2 (Matrix . liftA2 Matrix) $ cells m



cellsLinear :: (Indexable f, LinearTransform f g a,Diagonalizable f a) => f a :-> g a -> f (g a)
cellsLinear = cells . fromLinear

>-- | This method of matrix construction is especially nice.
>-- This is the functoriality of the tensor product.

>{-# INLINE matrix #-}
>matrix :: (Functor m, Functor n) => (a -> b -> c) -> m a -> n b -> (m :*: n) c
>matrix f x = \y -> Matrix $ flip fmap x $ \a -> 
>                            flip fmap y $ \b -> f a b

>inverseMatrix :: (Contravariant m, Contravariant n)
> => (g a -> c -> b) -> (m :*: n) c -> n b -> (m :*: g) a
>inverseMatrix f (Matrix x) y = Matrix $ flip inverseImage x $ \a ->
>                                flip inverseImage y $ \b -> f a b

>leftInverseMatrix :: (Contravariant m, Functor n)
>  => (g a -> c -> b) -> (m :*: n) b -> n c -> (m :*: g) a
>leftInverseMatrix f (Matrix x) y = Matrix $ flip inverseImage x $ \a ->
>                                     flip fmap y $ \b -> f a b

>rightInverseMatrix :: (Functor m, Contravariant n)
>  => (t -> a -> b) -> m t -> n b -> (m :*: n) a
>rightInverseMatrix f x y = Matrix $ flip fmap x $ \a ->
>                                      flip inverseImage y $ \b -> f a b

>-- | From "Kenneth Foner: Functional Pearl: Getting a Quick Fix on Comonads"
>-- <https://raw.githubusercontent.com/plaidfinch/GQFC-1.pdf>
>loeb :: (Functor f) => f (f a -> a) -> f a
>loeb ffs = fix (\fa -> fmap ($ fa) ffs)

>loebM :: (Monad m) => m a -> (a -> m (m b -> b)) -> m b
>loebM m f = loeb (m >>= f)

>loebloeb :: (Functor f) => f (f (f a -> a) -> f a -> a) -> f a
>loebloeb = loeb . loeb

>-- | construct a matrix where each element may refer to other elements
>-- for computation.
>spreadsheetMatrix :: (Functor f, Functor g) => (a -> b -> (f :*: g) c -> c)
>  -> f a -> g b -> (f :*: g) c
>spreadsheetMatrix f x y = loeb $ matrix f x y

>mapWithSelf :: (Functor f) => (f b -> a -> b) -> f a -> f b
>mapWithSelf f = loeb . fmap (flip f)

>selfM :: (Monad m) => (m a -> a) -> m a
>selfM = loeb . return

>selfJoinM :: (Monad m) => m (m (m a -> a)) -> m a
>selfJoinM = loeb . (>>= id)

>spreadsheet :: (Functor f, Functor g) => (f :*: g) ((f :*: g) a -> a) -> (f :*: g) a
>spreadsheet = loeb . Matrix . cells

>inverseFix :: (Contravariant p) => (a -> a) -> p a
>inverseFix f = let x = x |>> f in x

>inverseFix2 :: (Contravariant p) => (a -> b) -> (b -> a) -> p a
>inverseFix2 f g = let x = y |>> f
>                      y = x |>> g
>                    in x

>matrixCompose :: (Functor m, Functor n, Category cat)
>               => m (cat b c) -> n (cat a b) -> (m :*: n) (cat a c)
>matrixCompose = matrix (<<<)

>tensorProduct :: (Num a, Functor m, Functor n) => m a -> n a -> (m :*: n) a
>tensorProduct = matrix (*)

>tensorProductLin :: (Linearizable arr (:*:) f g a, Num a, Functor f, Functor g)
> => f a -> g a -> arr (f a) (g a)
>tensorProductLin x y = linear $ tensorProduct x y

>-- | <https://en.wikipedia.org/wiki/Tensor_product>


 fmap (\u -> unI (lm -!< u)) v

 \u -> (unI $ f u) %* v



>-- | bilinearity :
>--    \[f (a + b,c) = f(a, c) + f(b, c)\]
>--    \[f (k b, c) = k f(b, c)\]
>--    \[f (a, b + c) = f(a, b) + f(a,c)\]
>--    \[f (a, k c) = k f(a, c)\]
>-- <https://en.wikipedia.org/wiki/Bilinear_map>
>bilinearImpl :: (VectorSpace (g c), VectorSpace (f c), Scalar (g c) ~ c,
> Indexable f c, Indexable g c, Integral c, VectorSpace ((f :*: g) c))
>  => (f c -> f c -> g c) -> f c -> f c -> (f :*: g) c
>bilinearImpl f a b = asplit a b %+ bsplit a b
>  where asplit a' b' = Matrix $ fmap (\p -> p `indexProject` a' %* f (basisVector p 1 vzero) b') diagonalProjections
>        bsplit a' b' = Matrix $ fmap (\q -> q `indexProject` b' %* f a' (basisVector q 1 vzero)) diagonalProjections



bilinear :: (a -> b -> c) -> (f a :-> g a, f a :-> g a) -> f a :-> g a
bilinear f (x,y) = linear $ bilinear_impl f (fromLinear x) (fromLinear y)

>applicativeMatrix :: (Applicative f, Functor m, Functor n)
>                  => f (a -> b -> c)
>                  -> (m :*: f) a -> (n :*: f) b
>                  -> (m :*: n) (f c)
>applicativeMatrix f (Matrix x) (Matrix y) = matrix (\a b -> f <*> a <*> b) x y

>(>*<) :: (Applicative f, Functor m, Functor n)
>                  => f (a -> b -> c) -> ((m :*: f) a, (n :*: f) b)
>                  -> (m :*: n) (f c)
>f >*< (x,y) = applicativeMatrix f x y

>-- | <https://en.wikipedia.org/wiki/Matrix_%28mathematics%29>
>class (Num (Scalar v)) => VectorSpace v where
>  type Scalar v
>  vzero :: v
>  vnegate :: v -> v
>  (%+)  :: v -> v -> v -- sum
>  (%*)  :: Scalar v -> v -> v -- scalar product

>type NumIdentities a = (AdditiveIdentity a, MultiplicativeIdentity a)
>type Numeric a = (Num a, NumIdentities a)
>type NumFractional a = (Fractional a, NumIdentities a)
>type NumFloating a = (Floating a, NumIdentities a)
>type VectorSpaceOver v a = (VectorSpace v, Scalar v ~ a)
>type PrimitiveSpace v = (v ~ Scalar v, VectorSpace v)
>type ComplexVectorSpace v a = VectorSpaceOver v (Complex a)
>type Linear a b = (VectorSpace a, VectorSpace b, Scalar a ~ Scalar b)
>type LinearInnerProductSpace a b = (Linear a b, InnerProductSpace a, InnerProductSpace b)

>type SupportsMatrixMultiplication f g h a = (InnerProductSpaceFunctor g a, ConjugateSymmetric a,
>                                             Transposable g h a, Functor f, Num a)

>type LinearIso f g a = (LinearTransform f g a, LinearTransform g f a)

>class (VectorSpace v) => BilinearVectorSpace v where
>   biLin :: v -> v -> Scalar v

>-- | <https://en.wikipedia.org/wiki/Dot_product>
>-- | <https://en.wikipedia.org/wiki/Outer_product>
>class (VectorSpace m) => InnerProductSpace m where
>  (%.) :: m -> m -> Scalar m
>  areOrthogonal :: (Eq (Scalar m)) => m -> m -> Bool
>  areOrthogonal a b = a %. b == 0
>  canonicalNorm :: (Floating (Scalar m)) => m -> Scalar m
>  canonicalNorm x = sqrt(x %. x)

>class (InnerProductSpace (f a), a ~ Scalar (f a))
> => InnerProductSpaceFunctor f a where
>  innerProductF :: f a -> f a -> a
>
>  default innerProductF :: (Scalar (f a) ~ a) => f a -> f a -> a
>  innerProductF = (%.)

>outer :: (InnerProductSpace m, Scalar m ~ Scalar v, VectorSpace v)
> => m -> v -> (m -> v)
>outer a b x = (a %. x) %* b


>-- | <https://en.wikipedia.org/wiki/Lie_algebra>
>class (VectorSpace m) => LieAlgebra m where
>  (%<>%) ::  m -> m -> m  -- [x,y]

>class MetricSpace s where
>   type Distance s
>   distance :: s -> s -> Distance s

>class (MetricSpace s, (Floating (Distance s)))
> => DifferentialMetricSpace s where
>   convergeTo :: (Monad m) => s -> (s -> m u) -> m u

>metricSpaceDerivative ::
>   (DifferentialMetricSpace a, MetricSpace b, Distance a ~ Distance b, Fractional (Distance b), Monad m)
>   => (a -> b) -> a -> m (Distance b)
>metricSpaceDerivative f x = 
>  convergeTo x $ \x' ->
>    return $ distance (f x') (f x) / distance x' x


compose_matrix_with :: (Diagonalizable m c, Diagonalizable g b, LinearIso g n b, LinearTransform m f a, LinearTransform m n c, Num c, Scalar c ~ Scalar (f a), Transposable g n b, Diagonalizable n b, Diagonalizable m a, Scalar (n c) ~ Scalar (m (f a))) =>
 (f a -> g b -> c) -> m a :-> f a -> g b :-> n b -> m c :-> n c
compose_matrix_with f m1 m2 = linmatrix (bilinear f)
  (cells_linear m1, cells_linear $ transpose m2)

lie_compose :: (Indexable m, Indexable n, LinearTransform m g a, Diagonalizable m (g a), Diagonalizable g a, LinearTransform m n (g a), LinearIso n g a, Num (g a), LieAlgebra (g a), Transposable g n a, Diagonalizable n a, Diagonalizable m a, Scalar (n (g a)) ~ Scalar (m (g a)))
  => m a :-> g a -> g a :-> n a -> m (g a) :-> n (g a)
lie_compose m1 m2 = linmatrix (bilinear (%<>%)) (cells_linear m1, (cells_linear $ transpose m2))
  

>-- | norm_squared is an optimization that often avoids computing square root
>class (VectorSpace m, Num (Scalar m)) => NormedSpace m where
>  norm :: m -> Scalar m
>  normSquared :: m -> Scalar m
>  normSquared x = let x2 = norm x in x2*x2
>  default norm :: (Floating (Scalar m)) => m -> Scalar m
>  norm x = sqrt (normSquared x)
>  {-# MINIMAL norm | normSquared #-}

>-- | This computes norm of each row, then computes the norm of the resulting column vector.
>matrix_norm :: (Functor f, NormedSpace (g a), NormedSpace (f (Scalar (g a))))
>  => (f :*: g) a -> Scalar (f (Scalar (g a)))
>matrix_norm = norm . fmap norm . cells

>normedSpaceDerivate :: (NormedSpace t, VectorSpace v,
> Fractional (Scalar v), Scalar t ~ Scalar v)
>   => (t -> v) -> (t, t) -> v
>normedSpaceDerivate f (x, dx) = (recip (norm dx)) %* (f (x %+ dx) %- f x)


>class CompleteSpace m where

>class ConjugateSymmetric m where
>  conj :: m -> m

>class (Scalar (m a) ~ Scalar (n a), Functor m, Functor n) => LinearTransform m n a where
>  (<*>>) :: n a -> (m :*: n) a -> m a -- ^ vector times matrix
>  (<<*>) :: (m :*: n) a -> m a -> n a -- ^ matrix times vector


>data Lin b c where
>  Lin :: (f :*: g) a -> Lin (f a) (g a)

>class (Functor m, Functor n) => Transposable m n a where
>  transposeImpl :: (m :*: n) a -> (n :*: m) a

>instance Transposable ((->) row) ((->) col) a where
>  transposeImpl (Matrix f) = Matrix $ \a b -> f b a

>instance (Transposable f h (g (k a)), Transposable g h (k a),
>          Transposable g k a, Transposable f k (g a),
>          Scalar ((f :*: g) a) ~ Scalar ((h :*: k) a))
> => Transposable (f :*: g) (h :*: k) a where
>  transposeImpl = Matrix . Matrix
>                 . fmap (fmap Matrix)
>                 . fmap trans
>                 . fmap (fmap trans)
>                 . trans
>                 . fmap trans
>                 . cells . fmap cells . cells
>   where trans :: Transposable g' f' a' => g' (f' a') -> f' (g' a')
>         trans = cells . transposeImpl . Matrix

>-- | Oddly, scalars must match.
>instance (Scalar a ~ Scalar b) => Transposable ((,) a) ((,) b) c where
>   transposeImpl (Matrix (a,(b,c))) = Matrix (b,(a,c))

transpose :: (Diagonalizable n a, LinearTransform n m a, LinearTransform m n a, Diagonalizable m a,Num a, Transposable m n a) => m a :-> n a -> n a :-> m a

>transpose :: (Transposable g f a, Linearizable arr (:*:) f g a, Linearizable arr (:*:) g f a) => arr (g a) (f a) -> arr (f a) (g a)
>transpose x = linear $ transposeImpl $ fromLinear x

>indexableTranspose :: (Functor n, Num a, Indexable m a) => (n :*: m) a -> (m :*: n) a
>indexableTranspose (Matrix m) = matrix runIndex diagonalProjections m

>updateColumn :: (Applicative h) => (a -> f b -> g c) -> h a -> (h :*: f) b -> (h :*: g) c
>updateColumn f col (Matrix m) = Matrix $ fmap f col <*> m

>updateRow :: (a -> f (g b) -> f' (g' b')) -> a -> (f :*: g) b -> (f' :*: g') b'
>updateRow f row (Matrix m) = Matrix $ f row m

>-- | Example use:
>-- 
>-- > write_column (Vector3 3 4 5) `ycoord3` identity3 == [[1,3,0],[0,4,0],[0,5,1]]
>-- 
>class UpdateableMatrixDimension f where
>  writeRow    :: (Applicative h) => h a -> f ((f :*: h) a -> (f :*: h) a)
>  writeColumn :: (Applicative h) => h a -> f ((h :*: f) a -> (h :*: f) a)

>-- | Cramer's rule <https://en.wikipedia.org/wiki/Cramer%27s_rule>.
>-- 
>-- Solves \({\mathbf x}\) from matrix equation \(A{\mathbf x} = {\mathbf b}\), where \(A\) is first parameter and \({\mathbf b}\)
>-- is second parameter. Returns vector \({\mathbf x}\). Satisfies requirements:
>-- 
>-- > a <<*> solve_matrix a b == b
>--
>-- \[{\mathbf x}_i = {{\det(A[A_{ki} := {\mathbf b}_k])} \over {\det(A)}}\]
>solveMatrix :: (Traceable m b, Fractional b, UpdateableMatrixDimension m)
>     => (m :*: m) b -> m b -> m b
>solveMatrix m b = fmap (\f -> mdetinverse * determinantImpl (f m)) (writeColumn b)
>  where mdetinverse = 1 / determinantImpl m

>(<!-!>) :: (m a -> a) -> (a -> m a -> m a) -> Index m a
>f <!-!> finv = MakeIndex f finv

>runIndex :: Index m a -> m a -> a
>runIndex (MakeIndex f _) x = f x

>appIndex :: (Applicative f) => f (Index m a) -> f (m a) -> f a
>appIndex = liftA2 scalarProjection

>data Index m a = MakeIndex {
>   scalarProjection :: m a -> a,
>   modifyVector :: a -> m a -> m a -- element to insert, vector to use to fill rest of dimensions
> }

>class (Applicative m) => Indexable m a where
>  {-# MINIMAL diagonalProjections, indexableIndices #-}
>  diagonalProjections :: m (Index m a)
>  basisVector :: Index m a -> a -> m a -> m a
>  indexProject :: Index m a -> m a -> a
>  indexableIndices :: (Integral a) => m a
>  basisVector = modifyVector
>  indexProject = scalarProjection

>class AdditiveIdentity a where
>  additiveIdentity :: a
>
>class MultiplicativeIdentity a where
>  multiplicativeIdentity :: a

>-- | <https://en.wikipedia.org/wiki/Square_matrix>
>class (Indexable m a, Transposable m m a) => Diagonalizable m a where
>  identity :: (Num a) => (m :*: m) a
>  diagonalImpl :: (m :*: m) a -> m a
>  diagonalMatrixImpl :: m a -> (m :*: m) a -> (m :*: m) a

>basis :: (Num a, Diagonalizable m a) => m (m a)
>basis = cells identity

>coefficients :: (Foldable m, Applicative m, VectorSpace v) => m (Scalar v) -> m v -> v
>coefficients coeff = vsum . liftA2 (%*) coeff

bilinear_map :: (Foldable m, VectorSpace a, Diagonalizable m b, Diagonalizable m c)
  => (m b -> m c -> a) -> m (Scalar a) -> m (Scalar a) -> a

>bilinearMap :: (Num c, Num b, VectorSpace a, Foldable m, Foldable n, Diagonalizable m b, Diagonalizable n c)
>  => (m b -> n c -> a) -> m (Scalar a) -> n (Scalar a) -> a
>bilinearMap f x y = coefficients x $ fmap (coefficients y) $ cells $ matrix f basis basis

>-- | <https://en.wikipedia.org/wiki/Linear_map#Matrices>
>linearMap_ :: (Num a, Foldable m, Diagonalizable m a, VectorSpace b) => (m a -> b) -> m (Scalar b) -> b
>linearMap_ f x = coefficients x $ fmap f basis

>linearMap' :: (Num a,b ~ Scalar (n b), Foldable m, Diagonalizable m a, VectorSpace (n b))
>  => (m a -> n b) -> m b -> n b
>linearMap' = linearMap_




linear_id = linear_map id


>linearIdentity :: (Linearizable arr (:*:) m m a, LinearTransform m m a, Diagonalizable m a, Num a) => arr (m a) (m a)
>linearIdentity = linear identity

diagonal :: (LinearTransform m m a, Diagonalizable m a) => m a :-> m a -> m a

>diagonal :: (Linearizable arr (:*:) m m a, Diagonalizable m a) => arr (m a) (m a) -> m a
>diagonal = diagonalImpl . fromLinear

>class (Functor m, Functor n) => ProjectionSpace (m :: Type -> Type) (n :: Type -> Type) where
>   data (m \\\ n) a
>   projectFirst   :: m a -> n a
>   projectSecond  :: m a -> (m \\\ n) a
>   joinVector :: n a -> (m \\\ n) a -> m a

>-- | CodiagonalMatrix represents a matrix that can be split along the diagonal.
>-- The Codiagonal type represents a matrix without its diagonal.
>-- The ProjectionVector type represents a vector down from first element of diagonal
>-- when the diagonal is removed. This vector often has less elements than the original vector.
>-- Similarly for vector right from the first element of diagonal.
>class CodiagonalMatrix m a where
>   data Codiagonal m a
>   type (m \\ a)
>   codiagonalImpl :: (m :*: m) a -> Codiagonal m a
>   (|\|) :: m a -> Codiagonal m a -> (m :*: m) a
>   downProject  :: Codiagonal m a -> m \\ a
>   rightProject :: Codiagonal m a -> m \\ a

codiagonal :: (Num a, Diagonalizable m a, LinearTransform m m a, CodiagonalMatrix m a) => m a :-> m a -> Codiagonal m a
codiagonal = codiagonal_impl . fromLinear

>-- | NOTICE: Linearizable instances for matrices that have similar dimensions are special.
>class (Category arr) => Linearizable arr prod f g a | arr -> prod where
>   fromLinear :: arr (f a) (g a) -> (prod f g) a
>   linear :: (prod f g) a -> arr (f a) (g a)

>-- | <https://ncatlab.org/nlab/show/dimension>
>class (Diagonalizable m a) => Traceable m a where
>  traceImpl :: (m :*: m) a -> a
>  determinantImpl :: (m :*: m) a -> a
>  vectorDimension :: m a -> a
>  default vectorDimension :: (Num a) => m a -> a
>  vectorDimension (f :: m a) = traceImpl (identity :: (m :*: m) a)

>class (Category arr,Traceable m a) => LinearTraceable arr m a | m a -> arr where
>  determinant :: arr (m a) (m a) -> a
>  trace       :: arr (m a) (m a) -> a
>  default determinant :: (Linearizable arr (:*:) m m a) => arr (m a) (m a) -> a
>  default trace :: (Linearizable arr (:*:) m m a) => arr (m a) (m a) -> a
>  determinant = determinantImpl . fromLinear
>  trace = traceImpl . fromLinear

>-- | <http://en.wikipedia.org/wiki/Adjugate>
>-- \({\mathsf{cofactor}}(A) = |A|(A^{-1})^{\top}\) <https://en.wikipedia.org/wiki/Cross_product>
>class (Traceable m a) => Invertible m a where
>   cofactorImpl :: (m :*: m) a -> (m :*: m) a
>   adjucateImpl :: (m :*: m) a -> (m :*: m) a
>   inverseImpl  :: (m :*: m) a -> (m :*: m) a

>class (LinearTraceable arr m a) => LinearInvertible arr m a where
>   cofactor :: arr (m a) (m a) -> arr (m a) (m a)
>   adjucate :: arr (m a) (m a) -> arr (m a) (m a)
>   inverse  :: arr (m a) (m a) -> arr (m a) (m a)
>   default cofactor :: (Invertible m a, Linearizable arr (:*:) m m a) => arr (m a) (m a) -> arr (m a) (m a)
>   default adjucate :: (Invertible m a, Linearizable arr (:*:) m m a) => arr (m a) (m a) -> arr (m a) (m a)
>   default inverse :: (Invertible m a, Linearizable arr (:*:) m m a) => arr (m a) (m a) -> arr (m a) (m a)
>   cofactor = linear . cofactorImpl . fromLinear
>   adjucate = linear . adjucateImpl . fromLinear
>   inverse  = linear . inverseImpl . fromLinear

>-- | this computes \[f(A) = (A^{-1})^{\top}\]
>-- it's used to compute dual basis for a set of basis vectors.
>dualBasisImpl :: (Invertible m a) => (m :*: m) a -> (m :*: m) a
>dualBasisImpl a = transposeImpl (inverseImpl a)
>
>dualBasis :: (Invertible m a, Linearizable arr (:*:) m m a)
> => arr (m a) (m a) -> arr (m a) (m a)
>dualBasis = linear . dualBasisImpl . fromLinear

is_unitary :: (Invertible m a, Eq (LinearMap (m a) (m a)),
              ConjugateSymmetric (LinearMap (m a) (m a)))
  => m a :-> m a -> Bool
is_unitary m = conj -!< m == inverse m

>class (Functor m) => EigenDecomposable m a where
>  eigenvalues :: (m :*: m) a -> m a

>class (EigenDecomposable m a) => EigenVectorable m a where
>  eigenvectors :: (m :*: m) a -> (m :*: m) a

>class (Applicative m, Applicative n) => AppendableVector m n where
>  type (m :+: n) :: Type -> Type
>  (||>>) :: m a -> n a -> (m :+: n) a

>class (AppendableVector m n) => SplittableVector m n where
>  vsplit   :: (m :+: n) a -> (m a, n a)

>-- | Iverson bracket: <http://en.wikipedia.org/wiki/Iverson_bracket>

>class Conditional a where
>  fromBoolean :: Bool -> a

>class StandardBasis m where
>  unitVectors :: [m]

>class (VectorSpace v) => CoordinateSpace v where
>  type Coordinate v 
>  index  :: Coordinate v -> v -> Scalar v
>  listVector :: [Scalar v] -> v  -- convert list to vector
>  dimensionSize :: v -> Int
>  coordinates :: v -> [Coordinate v]

>-- | vector space with scalars in Num class
>class (Num (Scalar v), VectorSpace v) => NumSpace v 

>-- | vector space with fractional scalars
>class (Fractional (Scalar v), NumSpace v) => FractionalSpace v 


>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>
>class (VectorSpace v) => FiniteDimensional v d i arr where
>   finite :: arr ((d :*: d) v) (i v)

>class HasIdentityLinear v arr where
>   matIdentity :: (Ord a, Num a, ConjugateSymmetric a) => arr (v a) (v a)

>class (VectorSpace v) => Dualizable v d where
>  covector :: (v -> Scalar v) -> d v
>  bracket :: d v -> v -> Scalar v

>-- | <https://en.wikipedia.org/wiki/Laplace_operator>
>--   <https://en.wikipedia.org/wiki/Divergence>
>--   <https://en.wikipedia.org/wiki/Gradient>
>--   <https://en.wikipedia.org/wiki/Directional_derivative>
>-- Notice: for directional derivative,
>-- the direction is not automatically normalized, since that needs NormedSpace
>class (VectorSpace v) => VectorDerivative v d arr | d -> arr, v arr -> d where
>  divergence :: arr v v -> d v  -- (Del %. f)(v)
>  grad       :: d v -> arr v v    -- (Del f)(v)
>  directionalDerivative :: v -> d v -> d v -- (v %. Del f)(x)
>  laplace    :: d v -> d v    -- (Del^2 f)(v)
>  laplace = divergence . grad
>  {-# MINIMAL divergence, grad, directionalDerivative #-}

>-- | version of directional derivative that normalizes the direction:
>-- <https://mathworld.wolfram.com/DirectionalDerivative.html>
>normalizedDirectionalDerivative :: (VectorDerivative v d arr, NormedSpace v, Fractional (Scalar v))
>   => v -> d v -> d v
>normalizedDirectionalDerivative v d =
>   directionalDerivative ((1/norm v) %* v) d

>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>
>class VectorCrossProduct v arr where
>  curl       :: arr v v -> arr v v  -- (Del * f)(v)
>
>-- | <https://en.wikipedia.org/wiki/Vector_Laplacian>
>class VectorLaplacian v arr where
>  vectorLaplace :: arr v v -> arr v v -- (Del^2 A)(v)

  default vector_laplace :: (VectorCrossProduct v arr) => arr v v -> arr v v


>class (Functor f) => ProjectionDual f d a where
>   projectionDual :: f (d (f a))

>matrixM :: (Traversable f, Traversable g, Monad m) => 
>           (a -> b -> m c) -> f a -> g b -> m ((f :*: g) c)
>matrixM f row col = do res <- flip mapM row $ \a ->
>                              flip mapM col $ \b -> f a b
>                       return $ Matrix res

>matrixMatrix :: (Functor m, Functor n, Functor m', Functor n')
>             => (a -> b -> c)
>             -> (m :*: m') a
>             -> (n :*: n') b
>             -> ((m :*: n) :*: (m' :*: n')) c
>matrixMatrix f (Matrix x) (Matrix y) = Matrix $ (matrix . matrix) f x y



eigenvectors_generic :: (a ~ Scalar (n a),
                Fractional a, VectorSpace (n a),EigenDecomposable m a)
               => (m :*: m) a -> (n a -> n a) -> (m :*: n) a

eigenvectors_generic :: (Fractional (g a), EigenDecomposable f (g a))
   => (f :*: f) (g a) -> (g a -> g a) -> (f :*: g) a
eigenvectors_generic m a = Matrix $ fmap (fix . (a %/)) (eigenvalues m)

>newtype Basis m = Basis [m]

>(%-) :: (VectorSpace v) => v -> v -> v
>x %- y = x %+ (vnegate y)

>(%/) :: (Fractional (Scalar v),VectorSpace v) => v -> Scalar v -> v
>x %/ y = (1 / y) %* x

>-- | <https://en.wikipedia.org/wiki/Angle>
>innerproductspaceCosAngle :: (InnerProductSpace m, Floating (Scalar m)) => m -> m -> Scalar m
>innerproductspaceCosAngle x y = (x %. y)
>              / (innerproductspaceNorm x * innerproductspaceNorm y)

>normedLieAlgebraSinAngle :: (LieAlgebra m, NormedSpace m, Floating (Scalar m)) => m -> m -> Scalar m
>normedLieAlgebraSinAngle x y = norm (x %<>% y)
>              / (norm x * norm y)

>-- | <https://en.wikipedia.org/wiki/Angle>
>angle :: (InnerProductSpace m, Floating (Scalar m)) => m -> m -> Scalar m
>angle x y = acos (innerproductspaceCosAngle x y)

-- | <https://en.wikipedia.org/wiki/Cross_product>
-- This is a skew-symmetric matrix whose application to a vector
-- is same as cross product of a with the vector.
-- @cross_product_matrix v <<*> w == v %<>% w@.
cross_product_matrix :: (Num a, LinearTransform m m a, Traceable m a, LieAlgebra (m a)) => m a -> m a :-> m a
cross_product_matrix a = linear $ Matrix $ fmap (%<>% a) $ cells (identity $ vector_dimension a)

>iVec,jVec,kVec,lVec :: (StandardBasis v) => v
>iVec = unitVectors !! 0
>jVec = unitVectors !! 1
>kVec = unitVectors !! 2
>lVec = unitVectors !! 3

>{-# INLINABLE innerproductspaceNorm #-}
>innerproductspaceNorm :: (Floating (Scalar m), InnerProductSpace m)
>                       => m -> Scalar m
>innerproductspaceNorm v = sqrt (v %. v)

>{-# INLINABLE innerproductspaceNormSquared #-}
>innerproductspaceNormSquared :: (Floating (Scalar m), InnerProductSpace m)
>         => m -> Scalar m
>innerproductspaceNormSquared v = v %. v

>vsum :: (Foldable t, VectorSpace a) => t a -> a
>vsum = foldr (%+) vzero

fold_rows :: (Indexable m, LinearTransform m n a,Diagonalizable m a) => (n a -> b) -> m a :-> n a -> m b
fold_rows f x = fmap f (cells $ fromLinear x)

fold_columns :: (Diagonalizable m a, Diagonalizable n a, LinearTransform m n a, LinearTransform n m a, Transposable m n a) 
              => (m a -> b) -> LinearMap (m a) (n a) -> n b
fold_columns f x = fold_rows f (transpose x)

>indexUnit :: (StandardBasis v) => Int -> v
>indexUnit i = unitVectors !! i

>projection :: (Fractional (Scalar v), VectorSpace v, InnerProductSpace v)
>           => v -> v -> v
>projection e a = ((e %. a) / (e %. e)) %* e

>normalize :: (Fractional (Scalar a), NormedSpace a) => a -> a
>normalize x = (1 / norm x) %* x


isEigenValue :: (Num a,Eq a, SquareMatrix m a) => (m :*: m) a -> a -> Bool
isEigenValue m v = determinant (m %- (v %* identity)) == 0


>vectorLength :: (Floating (Scalar m), InnerProductSpace m)
>              => m -> Scalar m
>vectorLength x = sqrt (x %. x)

>divide :: (Fractional (Scalar v), VectorSpace v) => v -> Scalar v -> v
>divide v x = (1/x) %* v

>vaverage :: (Num v,Fractional (Scalar v),VectorSpace v) => [v] -> v
>vaverage lst = (1 / fromIntegral (length lst)) %* vsum lst

>toScalarList :: (StandardBasis m, InnerProductSpace m) => m -> [Scalar m]
>toScalarList m = [m %. c | c <- unitVectors]

>instance (Functor f, PpShowVerticalF f, PpShowF g) => PpShowF (f :*: g) where
>	  ppf (Matrix x) = ppfVertical $ fmap (nest 4 . ppf) x

>instance (PpShowF g, PpShowVerticalF f, Functor f, PpShow a) => PpShow ((f :*: g) a) where
>	  pp x = ppf x

>fromScalarList :: (VectorSpace a, StandardBasis a) => [Scalar a] -> a
>fromScalarList lst = vsum $ zipWith (%*) lst unitVectors

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

>basisOf :: (StandardBasis m) => Basis m
>basisOf = Basis unitVectors

>listMatrix :: (CoordinateSpace n, CoordinateSpace (Scalar n)) 
>       => [[Scalar (Scalar n)]] -> n
>listMatrix m = listVector $ map listVector m

>-- | generalized implementation of matrix multiplication
>-- see <http://en.wikipedia.org/wiki/Matrix_multiplication>

>{-# INLINABLE (%*%) #-}
>(%*%) :: (SupportsMatrixMultiplication f g h a) => (f :*: g) a -> (g :*: h) a -> (f :*: h) a
>(%*%) (Matrix m1) m2 = matrix (%.) m1 (cells $ transposeImpl m2)

>(%**%) :: (SupportsMatrixMultiplication f g h a,
>        Linearizable arr (:*:) f h (Scalar (g a)), Linearizable arr (:*:) f g a,
>        Linearizable arr (:*:) g h a)
>    => arr (f a) (g a) -> arr (g a) (h a)
>    -> arr (f (Scalar (g a))) (h (Scalar (g a)))
>(%**%) a b = linear (fromLinear a %*% fromLinear b)

(%*%) :: (SupportsMatrixMultiplication f g h a) => (g a) :-> (h a) -> (h a) :-> (f a) -> (g a) :-> (f a)
m1 %*% m2 = linmatrix (bilinear (%.)) (cells_linear m1, (cells_linear $ transpose m2))

>-- | In this version, we must assume VectorSpaceOver (h a) a constraint,
>-- but the result type is nicer.

(%**%) :: (SupportsMatrixMultiplication f g h a) => g a :-> h a -> h a :-> f a -> g a :-> f a
m1 %**% m2 = linmatrix (bilinear (%.))
  (cells_linear m1, (cells_linear $ transpose m2))

>type MatrixNorm arr h m a = (LinearTraceable arr m (Scalar (h a)), 
>      InnerProductSpaceFunctor h a, 
>      ConjugateSymmetric a, 
>      Transposable h m a)

-- | <https://en.wikipedia.org/wiki/Frobenius_inner_product>
frobenius_inner_product :: (Traceable h a,
 SupportsMatrixMultiplication m m h a,
 Diagonalizable h a, LinearTransform h m a, LinearTransform m h a, Scalar a ~ a, MatrixNorm h m a, a ~ Scalar (h a), ConjugateSymmetric (h a), Scalar (m a) ~ Scalar (m (h a)), Indexable m)
 => LinearMap (h a) (m a) -> LinearMap (h a) (m a) -> a
frobenius_inner_product a b = trace (hermitian_conjugate a . b)

hermitian_conjugate :: (Diagonalizable f a, Diagonalizable g a,
 LinearTransform f g a, LinearTransform g f a,
 Transposable f g a, ConjugateSymmetric a)
   => f a :-> g a -> g a :-> f a
hermitian_conjugate f = linear $ fmap conj $ transpose_impl $ fromLinear f

-- | <https://en.wikipedia.org/wiki/Matrix_norm#Frobenius_norm>
frobenius_norm :: (Traceable h a, SupportsMatrixMultiplication m m h a, Diagonalizable h a, LinearIso h m a, Scalar a ~ a, MatrixNorm h m a, ConjugateSymmetric (h a), Floating (Scalar (h a)), a ~ Scalar (h a), Scalar (m a) ~ Scalar (m (h a)), Indexable m)
  => LinearMap (h a) (m a) -> Scalar (h a)
frobenius_norm a = sqrt (frobenius_inner_product a a)

linear_power :: (Diagonalizable h (h b), Diagonalizable h b) => h b :-> h b -> Integer -> h b :-> h b
linear_power (LinearMap p f) 0 = LinearMap Refl $ identity_impl (vector_dimension $ cells f)
linear_power f i = f . linear_power f (i-1)

>(%^%) :: (a ~ Scalar (f a), SupportsMatrixMultiplication f f f a, AdditiveIdentity a, MultiplicativeIdentity a, Diagonalizable f (f a), Diagonalizable f a)
> => (f :*: f) a -> Integer -> (f :*: f) a
>x %^% 0 = identity
>x %^% i = x %*% (x %^% (i-1))

(%^%) :: (LinearTransform h h b, Scalar b ~ b, Functor h, Diagonalizable h b, Transposable h h b, Indexable h
 ,InnerProductSpace (h b), VectorSpaceOver (h b) b, Scalar (h (h b)) ~ b)
      => LinearMap (h b) (h b) -> Integer -> LinearMap (h b) (h b)
x %^% 0 = linear_identity (vector_dimension $ diagonal x)
x %^% i = x %*% (x %^% (i-1)) 

>(|><|) :: (Functor m, Functor n, InnerProductSpace a)
>       => m a -> n a -> (m :*: n) (Scalar a)
>(|><|) = matrix $ \a b -> a %. b

>identityCS :: (CoordinateSpace m, CoordinateSpace (Scalar m),
>               Num (Scalar (Scalar m))) 
>           => (Int,Int) -> m
>identityCS (b,a) = listVector [ listVector [ if i == j then 1 else 0 
>                                           | i <- [0..(a-1)]]
>                              | j<- [0..(b-1)]]

>(%.%) :: (Num (Scalar m), CoordinateSpace m) => m -> m -> Scalar m
>x %.% y = sum [ index i x * index i y | i <- coordinates x]

>basisCoordinates :: (InnerProductSpace v) => Basis v -> v -> [Scalar v]
>basisCoordinates (Basis basis) x = map (%. x) basis

>coordinateSpaceFunctionMatrix :: (CoordinateSpace m, StandardBasis v)
>               => (v -> Scalar m) -> m
>coordinateSpaceFunctionMatrix f = listVector $ map f $ unitVectors

>-- | This is the linearity condition:

>functionMatrix :: (Diagonalizable f b, Num b) => (f b -> g b) -> (f :*: g) b
>functionMatrix f = Matrix $ fmap f $ cells $ identity

>instance (Show v) => Show (Basis v) where
>  show (Basis lst) = show lst

>instance ConjugateSymmetric Integer where { conj = id }
>instance ConjugateSymmetric Int where { conj = id }
>instance ConjugateSymmetric Float where { conj = id }
>instance ConjugateSymmetric Double where { conj = id }
>instance (Integral a) => ConjugateSymmetric (Ratio a) where { conj = id }
>instance (RealFloat a) => ConjugateSymmetric (Complex a) where
>   conj = conjugate
>instance (ConjugateSymmetric a) => ConjugateSymmetric (a -> a) where
>   conj x = x . conj

>instance (ConjugateSymmetric a, ConjugateSymmetric b) => ConjugateSymmetric (a,b) where
>   conj (a,b) = (conj a, conj b)


>instance (ConjugateSymmetric a, ConjugateSymmetric b, ConjugateSymmetric c) => ConjugateSymmetric (a,b,c) where
>   conj (a,b,c) = (conj a, conj b, conj c)
>instance (ConjugateSymmetric a, ConjugateSymmetric b, ConjugateSymmetric c, ConjugateSymmetric d) => ConjugateSymmetric (a,b,c,d) where
>   conj (a,b,c,d) = (conj a, conj b,conj c, conj d)

>-- | <https://ncatlab.org/nlab/show/Cayley-Dickson+construction>
>data CD a = CD a a
>   deriving (Show, Eq)
>
>instance (ConjugateSymmetric a, Num a) => ConjugateSymmetric (CD a) where
>  conj (CD a b) = CD (conj a) (negate b)

>instance (ConjugateSymmetric a, Num a) => Num (CD a) where
>  (CD a b) + (CD c d) = CD (a + c) (b + d)
>  (CD a b) - (CD c d) = CD (a - c) (b - d)
>  negate (CD a b) = CD (negate a) (negate b)
>  abs (CD a b) = CD (abs a) (abs b)
>  signum (CD a b) = CD (signum a) (signum b)
>  (CD a b) * (CD c d) = CD (a*c - d*conj b) (conj a * d + c*b)
>  fromInteger i = CD (fromInteger i) 0

>-- | <https://en.wikipedia.org/wiki/Convex_combination>
>-- This computes \[f([a_0,a_1,...,a_n], [{\mathbf b}_0,{\mathbf b}_1,...,{\mathbf b}_n]) = {{\sum_{j=0}^n{a_j{\mathbf b}_j}} \over \sum_{i=0}^n{a_i}}\]
>convex_combination :: (VectorSpace v, Fractional (Scalar v), Foldable t,
>  Applicative t) => t (Scalar v) -> t v -> v
>convex_combination a b = (1/sum a) %* vsum (liftA2 (%*) a b)

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

instance Diagonalizable Stream Integer where
   vector_dimension [] = []
   vector_dimension (_:cr) = 0 : map succ (vector_dimension cr)
   identity lst = linear $ linmatrix (\x y -> if x == y then 1 else 0) -!< (lst,lst)
   diagonal x | (Matrix m) <- fromLinear x = 
     map (\i -> (m !! fromInteger i) !! fromInteger i) indexable_indices
   diagonal_matrix v   = linear $ linmatrix (\x y -> if x == y then v !! fromInteger x else 0) -!< (dim,dim)
      where dim = vector_dimension v
 
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

>-- | a pair of vector spaces is a vector space if they are over the same set of scalars.
>instance (VectorSpace a, VectorSpace b, Scalar a ~ Scalar b) => VectorSpace (a,b) where
>   type Scalar (a,b) = Scalar a
>   vzero = (vzero,vzero)
>   vnegate (a,b) = (vnegate a,vnegate b)
>   (a,b) %+ (a',b') = (a %+ a', b %+ b')
>   a %* (b,c) = (a %* b, a %* c)

>instance (MetricSpace a, MetricSpace b, Distance a ~ Distance b, Floating (Distance b))
>  => MetricSpace (a,b) where
>   type Distance (a,b) = Distance a
>   distance (x,y) (x',y') = sqrt( (distance x x')^2 + (distance y y')^2)

>-- | Note: Scalar (Complex a) = Complex a
>instance (RealFloat a) => VectorSpace (Complex a) where
>   type Scalar (Complex a) = Complex a
>   vzero = 0 :+ 0
>   vnegate = negate
>   a %* b = a * b
>   (r1 :+ i1) %+ (r2 :+ i2) = (r1 + r2) :+ (i1 + i2)

>-- | <https://en.wikipedia.org/wiki/Inner_product_space>
>instance {-# OVERLAPPABLE #-} (RealFloat a) => InnerProductSpace (Complex a) where
>   a %. b = a * conj b

>isPositiveDefiniteInnerProductSpace m
> | m /= (0 :+ 0) = realPart (m %. m) > 0
> | otherwise = True


>instance {-# OVERLAPPABLE #-} (RealFloat a) => NormedSpace (Complex a) where
>   norm x = sqrt (x %. x)
>   normSquared x = x %. x

>instance (RealFloat a) => MetricSpace (Complex a) where
>   type Distance (Complex a) = Scalar (Complex a)
>   distance a b = norm (b - a)

>instance (Num a) => StandardBasis (Complex a) where
>   unitVectors = [(1 :+ 0), (0 :+ 1)]

>instance (VectorSpace a) => VectorSpace (Maybe a) where
>   type Scalar (Maybe a) = Scalar a
>   vzero = Just vzero
>   vnegate = maybe Nothing (Just . vnegate)
>   x %+ y = liftA2 (%+) x y
>   a %* x = fmap (a %*) x

>instance (Num (Scalar a), NormedSpace a) => NormedSpace (Maybe a) where
>   norm Nothing = 0
>   norm (Just x) = norm x

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
>  lst ||>> lst' = lst ++ lst'

>instance {-# OVERLAPPABLE #-}
>     (Show (f a)) => Show (([] :*: f) a) where
>  show (Matrix lst) = concat $ intersperse "\n" (map show lst)

>instance (Num v) => VectorSpace (x -> v) where
>   type Scalar (x -> v) = v
>   vzero = const 0
>   vnegate f = negate . f
>   a %* f = \i -> a * f i
>   f %+ g = \i -> f i + g i

>vec2Cast :: a :~: b -> a :~: b -> a :~: b
>vec2Cast Refl Refl = Refl


>-- | <https://en.wikipedia.org/wiki/Commutator>
>instance (VectorSpace a, Num a) => LieAlgebra (a -> a) where
>   f %<>% g = (f . g) %- (g . f)

>instance (Num a) => LieAlgebra (Endo a) where
>   f %<>% g = (f <> g) %- (g <> f)

>instance (Floating a) => VectorSpace (Product a) where
>   type Scalar (Product a) = a
>   vzero = Product 1
>   vnegate (Product x) = Product (recip x)
>   a %* (Product x) = Product (x ** a)
>   (Product x) %+ (Product y) = Product (x * y)

>instance (Floating a, ConjugateSymmetric a) => InnerProductSpace (Product a) where
>  (Product x) %. (Product y) = x ** y
>
>instance (Floating a, ConjugateSymmetric a) => InnerProductSpaceFunctor Product a

>instance (Num a) => VectorSpace (Sum a) where
>   type Scalar (Sum a) = a
>   vzero = Sum 0
>   vnegate (Sum x) = Sum (negate x)
>   a %* (Sum x) = Sum (a * x)
>   (Sum x) %+ (Sum y) = Sum (x + y)

>instance (NumFloating a, ConjugateSymmetric a) => NormedSpace (Sum a) where
>   normSquared x = x %. x

>instance (ConjugateSymmetric a, Num a) => InnerProductSpace (Sum a) where
>   (Sum x) %. (Sum y) = x * conj y

>instance (ConjugateSymmetric a, Num a) => InnerProductSpaceFunctor Sum a

>instance (Num a) => VectorSpace (First a) where
>   type Scalar (First a) = a
>   vzero = First Nothing
>   vnegate (First m) = First $ maybe Nothing (Just . negate) m
>   a %* (First m) = First $ maybe Nothing (Just . (a*)) m
>   (First (Just m)) %+ (First (Just n)) = First (Just (m + n))
>   (First Nothing) %+ (First m) = First m
>   (First m) %+ (First Nothing) = First m

>instance (Floating a, ConjugateSymmetric a) => NormedSpace (First a) where
>   normSquared x = x %. x

>instance (Num a, ConjugateSymmetric a) => InnerProductSpace (First a) where
>   (First (Just x)) %. (First (Just y)) = x * conj y
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

>instance (Floating a) => LieAlgebra (Product a) where
>   f %<>% g = (f <> g) %- (g <> f)

>instance (LinearInnerProductSpace v w, Eq v, Eq w, Num (Scalar w), Ord (Scalar w)) => InnerProductSpace (v,w) where
>   (a,b) %. (c,d) = a %. c + b %. d

>-- | <https://en.wikipedia.org/wiki/Lie_algebra>
>instance (LieAlgebra a, LieAlgebra b, Scalar a ~ Scalar b) => LieAlgebra (a,b) where
>   (a,b) %<>% (a',b') = (a %<>% a', b %<>% b')

>instance (Linear v w, Linear w u) => VectorSpace (v,w,u) where
>   type Scalar (v,w,u) = Scalar v
>   vzero = (vzero,vzero,vzero)
>   vnegate (x,y,z) = (vnegate x, vnegate y,vnegate z)
>   a %* (x,y,z) = (a %* x, a %* y,a %* z)
>   (x,y,z) %+ (x',y',z') = (x %+ x', y %+ y',z %+ z')

>instance (MetricSpace v, MetricSpace w, MetricSpace u, Distance v ~ Distance w,
>  Distance w ~ Distance u, Floating (Distance v))
> => MetricSpace (v,w,u) where
>   type Distance (v,w,u) = Distance v
>   distance (x,y,z) (x',y',z') = sqrt((distance x x')^2+(distance y y')^2 + (distance z z')^2)

>instance (LieAlgebra v, LieAlgebra w, LieAlgebra u, Scalar v ~ Scalar w, Scalar w ~ Scalar u)
>  => LieAlgebra (v,w,u) where
>   (a,b,c) %<>% (a',b',c') = (a %<>% a', b %<>% b', c %<>% c')

>instance (Ord (Scalar u), Eq v, Eq w, Eq u,
> LinearInnerProductSpace v w,
> LinearInnerProductSpace w u,
> Num (Scalar w))
>  => InnerProductSpace (v,w,u) where
>    (a,b,c) %. (d,e,f) = a %. d + b %. e + c %. f

>-- | <https://en.wikipedia.org/wiki/Dot_product>
>instance (Universe a, Num b, ConjugateSymmetric b)
> => InnerProductSpace (a -> b) where
>   f %. g = sum [f i * conj (g i) | i <- allElements]

>instance (Num b, ConjugateSymmetric b, Universe a) => InnerProductSpaceFunctor ((->) a) b

this function is identically zero for hilbert spaces.

>hilbertSpace :: (Num m, NormedSpace m) => m -> m -> Scalar m
>hilbertSpace x y = norm(x+y)+norm(x-y) - 2*(normSquared x+normSquared y)

>lieAdjoint :: (LieAlgebra v) => v -> Endo v
>lieAdjoint x = Endo $ \y -> x %<>% y

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

>-- | <https://en.wikipedia.org/wiki/Norm_(mathematics)>
>isOnUnitCircle :: (NormedSpace v, Eq (Scalar v)) => v -> Bool
>isOnUnitCircle v = normSquared v == 1

>-- | <https://en.wikipedia.org/wiki/Norm_(mathematics)>
>isInsideUnitCircle :: (NormedSpace v, Ord (Scalar v)) => v -> Bool
>isInsideUnitCircle v = normSquared v <= 1

instance VectorSpace (f (Complex a)) => VectorSpace ((f :*: Complex) a) where
  type Scalar ((f :*: Complex) a) = Scalar (f (Complex a))
  vzero = Matrix vzero
  vnegate (Matrix v) = Matrix (vnegate v)
  (Matrix v) %+ (Matrix w) = Matrix (v %+ w)
  c %* (Matrix v) = Matrix (c %* v)

>instance {-# OVERLAPPING #-} (Functor f) => Transposable f Complex a where
>  transposeImpl (Matrix m) = Matrix $ fmap realPart m :+ fmap imagPart m

>instance {-# OVERLAPPING #-} (Applicative f) => Transposable Complex f a where
>  transposeImpl (Matrix (m :+ n)) = Matrix $ liftA2 (:+) m n

>-- | notice matrix of two complex numbers has special properties as matrix.
>instance {-# OVERLAPS #-} Transposable Complex Complex a where
>  transposeImpl (Matrix ((m :+ mi)
>                      :+ (ni :+ n)))
>    = Matrix $ ((m :+ ni)
>             :+ (mi :+ n))

>-- | diagonalizable instance for complex numbers.
>-- diagonal ((a+bi)+i(c+di)) = (a-d) + i(b+c)
>instance (Num a) => Diagonalizable Complex a where
>  identity = Matrix $ (1 :+ 0) :+ (0 :+ 1)
>  diagonalImpl (Matrix ((a :+ b) :+ (c :+ d))) = (a-d) :+ (b+c)
>  diagonalMatrixImpl (a :+ b) m = Matrix $ (a :+ imagPart (realPart (cells m)))
>                                        :+ (realPart (imagPart (cells m)) :+ negate b)

>instance (Show (f a)) => Show ((Complex :*: f) a) where
>  show (Matrix (a :+ b)) = show a ++ " :+ " ++ show b

>instance (Num a) => VectorSpace (I a) where
>  type Scalar (I a) = a
>  vzero = I 0
>  vnegate (I x) = I (negate x)
>  (I x) %+ (I y) = I (x + y)
>  k %* (I x) = I (k * x)

instance {-# OVERLAPPING #-}
 (Num a, Indexable f a, Indexable g a) => VectorSpace ((f :*: g) a) where
  type Scalar ((f :*: g) a) = a
  vzero = matrix (\a b -> 0) (indexable_indices :: f a) (indexable_indices :: g a)
  vnegate m = fmap negate m
  (Matrix f) %+ (Matrix g) = Matrix $ liftA2 (liftA2 (+)) f g
  k %* (Matrix f) = Matrix $ fmap (fmap (*k)) f

>instance (Functor f, Functor g) => Functor (g :*: f) where
>   fmap f (Matrix x) = Matrix $ fmap (fmap f) x

>instance (Foldable f, Foldable g) => Foldable (f :*: g) where
>   foldMap f (Matrix m) = foldMap (foldMap f) m

>instance (Traversable f, Traversable g) => Traversable (f :*: g) where
>   traverse f (Matrix m) = Matrix <$> traverse (traverse f) m

>instance (Applicative f, Applicative g) => Applicative (f :*: g) where
>   pure = Matrix . pure . pure
>   (Matrix fs) <*> (Matrix xs) = Matrix $ pure (<*>) <*> fs <*> xs

>instance (Alternative f, Alternative g) => Alternative (g :*: f) where
>   empty = Matrix Applicative.empty
>   (Matrix a) <|> (Matrix b) = Matrix $ pure (<|>) <*> a <*> b

>leftUnitor :: (I :*: f) a -> f a
>leftUnitor (Matrix (I x)) = x

>rightUnitor :: (Functor f) => (f :*: I) a -> f a
>rightUnitor (Matrix f) = fmap unI f

>associator :: (Functor f) => ((f :*: g) :*: h) a -> (f :*: (g :*: h)) a
>associator (Matrix f) = Matrix $ fmap Matrix $ cells f
>
>unassociator :: (Functor f) => (f :*: (g :*: h)) a -> ((f :*: g) :*: h) a
>unassociator (Matrix f) = Matrix $ Matrix $ fmap cells f

>instance (Functor f, Functor g, Num a, StandardBasis (g a), StandardBasis (f a))
>   => StandardBasis ((f :*: g) a) where
>   unitVectors = concat $ cells $ matrix (matrix (*)) unitVectors unitVectors

>instance (Num a) => Indexable Complex a where
>  diagonalProjections = diagonalProjectionsComplex
>  indexableIndices = 0 :+ 1

>diagonalProjectionsComplex :: Complex (Index Complex a)
>diagonalProjectionsComplex =  (MakeIndex realPart (\a c -> (a :+ imagPart c)))
>                           :+ (MakeIndex imagPart (\a c -> (realPart c :+ a)))

example use: m <!> (xcoord3,ycoord3)

>(<!>) :: (Functor f, Functor g) => (g :*: f) a -> (g c -> b,f a -> c) -> b
>m <!> (x,y) = MatrixFold (x,y) `visit` m

>instance (Functor g) => Visitor ((g :*: f) a) where
>   data Fold ((g :*: f) a) b = forall c. MatrixFold (g c -> b,f a -> c)
>   visit (MatrixFold (gt,ft)) (Matrix x) = gt (fmap ft x)

>reduceI :: (I :*: I) a -> I a
>reduceI (Matrix (I x)) = x

>sumCoordinates :: (Foldable t, Num a) => t a -> a
>sumCoordinates = Data.Foldable.foldr (+) 0

>instance (Monad f, Monad g, forall b. Transposable g f b) => Monad (f :*: g) where
>   v >>= f = joinMatrix $ fmap f v

>instance (forall b. Transposable g f b, MonadFail f, MonadFail g) => MonadFail (f :*: g) where
>   fail msg = Matrix $ fmap (const $ fail msg) (fail msg)

>joinMatrix :: (Monad g, Monad f, forall b. Transposable g f b)
> => (f :*: g) ((f :*: g) a) -> (f :*: g) a
>joinMatrix = Matrix . fmap join . join . fmap (cells . transposeImpl . Matrix) . cells . fmap cells

>instance Transposable IO IO a where
>  transposeImpl = id

>type Projection f = forall a. f a -> a
>type ElementRemovals f g g' = forall a. f (g a -> g' a)

>data Determinance f g f' g' gg a' a res = Determinance {
>  determinanceRemoves1 :: g (g a -> gg a),
>  determinanceRemoves2 :: g (f (gg a) -> f' (g' a')),
>  determinanceCoordProj1 :: Projection f,
>  determinanceCoordProj2 :: Projection g,
>  determinanceCombine :: g a -> res,
>  determinanceNestedDeterminant :: (f' :*: g') a' -> a
> }

>genericDeterminant :: (Functor f, Applicative g, Num a) =>
>    Determinance f g f' g' gg a' a res -> (f :*: g) a -> res
>genericDeterminant (Determinance removes1 removes2 coord_proj1 coord_proj2 combine det) (Matrix m)
>     = combine $ (*) <$> coord_proj1 m <*> amv  
>   where amv = fmap (det . Matrix . coord_proj2 removes2 . (`fmap` m)) removes1

>instance (Num a) => Num (Endo a) where
>   (+) = (%+)
>   x - y = x %+ vnegate y
>   (Endo f) * (Endo g) = Endo $ f . g
>   negate (Endo f) = Endo $ negate . f
>   abs (Endo f) = Endo $ abs . f
>   signum f = f - abs f
>   fromInteger = Endo . const . fromInteger

>instance (Num a) => VectorSpace (Endo a) where
>   type Scalar (Endo a) = a
>   vzero = Endo (const 0)
>   vnegate (Endo f) = Endo $ negate . f
>   a %* (Endo f) = Endo $ \x -> a * f x
>   (Endo f) %+ (Endo g) = Endo $ \x -> f x + g x

>instance (Num a) => VectorSpace (((->) row :*: (->) col) a) where
>   type Scalar (((->) row :*: (->) col) a) = a
>   vzero = Matrix $ const $ const 0
>   vnegate (Matrix m) = Matrix $ \i j -> negate (m i j)
>   (Matrix m) %+ (Matrix n) = Matrix $ \i j -> m i j + n i j
>   a %* (Matrix m) = Matrix $ \i j -> a * m i j

>instance (Num a, Universe row, Universe col) => LinearTransform ((->) row) ((->) col) a where
>   colv <*>> (Matrix m) = \i -> sum [colv j * m i j | j <- allElements]
>   (Matrix m) <<*> rowv = \j -> sum [m i j * rowv i | i <- allElements]

>instance (Floating b, Universe a, ConjugateSymmetric b) => NormedSpace (a -> b) where
>   norm f = sqrt (f %. f)
>   normSquared f = f %. f

>-- | <https://en.wikipedia.org/wiki/Frobenius_inner_product>
>instance (Integral col, Integral row, Universe row, Universe col, Floating a, ConjugateSymmetric a)
> => InnerProductSpace (((->) row :*: (->) col) a) where
>   (Matrix m) %. (Matrix n) = sum $
>      [conj (m i j) * (n i j) | i <- allElements, j <- allElements]

>instance (Integral row, Integral col, Floating a, Universe row, Universe col, ConjugateSymmetric a)
> => NormedSpace (((->) row :*: (->) col) a) where
>   norm m = sqrt (m %. m)
>   normSquared m = m %. m

>instance MetricSpace (Ratio Integer) where
>   type Distance (Ratio Integer) = Ratio Integer
>   distance x y = abs (x - y)
>instance MetricSpace Integer where
>   type Distance Integer = Integer
>   distance x y = abs (x - y)
> 
>instance MetricSpace Int where
>   type Distance Int = Word
>   distance x y = fromIntegral $ abs (x - y)

>instance MetricSpace Int8 where
>   type Distance Int8 = Word8
>   distance x y = fromIntegral $ abs (x - y)

>instance MetricSpace Int16 where
>   type Distance Int16 = Word16
>   distance x y = fromIntegral $ abs (x - y)

>instance MetricSpace Int32 where
>   type Distance Int32 = Word32
>   distance x y = fromIntegral $ abs (x - y)

>instance MetricSpace Int64 where
>   type Distance Int64 = Word64
>   distance x y = fromIntegral $ abs (x - y)

>instance MetricSpace Double where
>  type Distance Double = Double
>  distance x y = abs(x - y)
>  
>instance MetricSpace Float where
>  type Distance Float = Float
>  distance x y = abs(x - y)

>instance AdditiveIdentity Integer where { additiveIdentity = 0 }
>instance AdditiveIdentity Int where { additiveIdentity = 0 }
>instance AdditiveIdentity Word where { additiveIdentity = 0 }
>instance AdditiveIdentity Float where { additiveIdentity = 0 }
>instance AdditiveIdentity Double where { additiveIdentity = 0 }

>instance MultiplicativeIdentity Integer where { multiplicativeIdentity = 1 }
>instance MultiplicativeIdentity Int where { multiplicativeIdentity = 1 }
>instance MultiplicativeIdentity Word where { multiplicativeIdentity = 1 }
>instance MultiplicativeIdentity Float where { multiplicativeIdentity = 1 }
>instance MultiplicativeIdentity Double where { multiplicativeIdentity = 1 }
