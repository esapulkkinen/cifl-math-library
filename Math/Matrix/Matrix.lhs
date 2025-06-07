>{-# LANGUAGE Safe,ExistentialQuantification, TypeOperators, Rank2Types #-}
>{-# LANGUAGE Arrows, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
>{-# LANGUAGE FlexibleInstances, PatternSynonyms #-}
>{-# LANGUAGE QuantifiedConstraints, UndecidableInstances, GADTs, DataKinds #-}
>{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses, AllowAmbiguousTypes #-}
>module Math.Matrix.Matrix where
>import Data.Complex
>import Data.Array (Array)
>import qualified Data.Array as Array
>import Control.Category
>import Control.Arrow
>import Control.Monad
>import Control.Monad.Zip
>import Data.Foldable
>import Data.Traversable
>import Data.Semigroup
>import Math.Tools.Orthogonal
>import Math.Tools.Adjunction
>import Math.Tools.Visitor
>import qualified Text.PrettyPrint as Pretty
>import Math.Tools.PrettyP hiding (empty)
>import Math.Tools.FixedPoint
>import Math.Tools.Arrow
>import Math.Tools.CoFunctor
>import Math.Tools.Isomorphism
>import qualified Control.Applicative as Applicative
>import Control.Applicative
>import Math.Tools.I
>import Math.Matrix.Interface
>import Math.Matrix.Linear
>import Math.Matrix.SIMD
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Tools.CoMonad
>import Data.Type.Equality
>import Data.Kind
>import Data.Functor.Contravariant
>import GHC.TypeLits
>import Prelude hiding (id, (.))

The :*: should be used when the number of dimensions of matrix is
exactly 2.

>type family Transpose a
>type instance Transpose ((f :*: g) a) = (g :*: f) a

conjIso :: (ConjugateSymmetric a) => (a :-> a) :==: (a :-> a)
conjIso = conj <-> conj

>transposeIso :: (Linearizable LinearMap (:*:) m n a, Linearizable LinearMap (:*:) n m a, Transposable n m a, Transposable m n a) => (m a :-> n a) :==: (n a :-> m a)
>transposeIso = transpose <-> transpose

>vnegateIso :: (VectorSpace v) => v :==: v
>vnegateIso = vnegate <-> vnegate

>-- | See video by Bartosz Milewski ("Category theory II 7.2: Comonads
>-- categorically and examples")
>instance (Comonad f, Comonad g, forall a. Transposable g f a, forall b. Transposable f g b,
> forall a. Diagonalizable f (f (g a)),
> forall a. LinearTransform g f (f (g a)),
> forall a. Diagonalizable g (g a),
> forall a. LinearTransform f g (g a)
> )
>  => Comonad (f :*: g) where
>   extract (Matrix m) = extract (extract m)
>   duplicate (Matrix m) = fmap Matrix 
>                        $ transposeImpl $ Matrix $ fmap duplicate $ cells
>                        $ transposeImpl $ Matrix $ fmap duplicate m

>deriving instance (Eq a, Applicative f, Applicative g, Foldable f, Foldable g, Ord (f (g a))) => Ord ((f :*: g) a)

instance (Eq a, Applicative f, Applicative g, Foldable f, Foldable g)
    => Eq ((f :*: g) a) where
  x == y = liftA2 (==) x y <!> (and,and)


instance Comonad ((,) a :*: (->) a) where
   extract (Matrix (x,f)) = f x
   duplicate (Matrix (a,f)) = Matrix (a,\x -> Matrix (x,f))


>(!$!) :: (Linearizable LinearMap (:*:) f g b, Num b, Functor f, Functor g)
> => f (a -> b) -> g a -> f b :-> g b
>(!$!) f x = linear $ matrix ($) f x

>matrixBind :: (Functor f) => f a -> (a -> g b) -> (f :*: g) b
>matrixBind x = Matrix . flip fmap x

>-- | this is same as Matrix.Interface.matrix
>matrix2d :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> (f :*: g) c
>matrix2d f x y  = x `matrixBind` \a -> f a <$> y

>matrix3d :: (Functor f, Functor g, Functor h)
> => (a -> b -> c -> d)
> -> f a -> g b -> h c
> -> (f :*: (g :*: h)) d
>matrix3d f x y z = x `matrixBind` \a -> matrix (f a) y z

>matrix4d :: (Functor f, Functor g, Functor h, Functor i)
> => (a -> b -> c -> d -> e)
> -> f a -> g b -> h c -> i d
> -> (f :*: (g :*: (h :*: i))) e
>matrix4d f x y z t = x `matrixBind` \a -> matrix3d (f a) y z t

>-- | needs type annotation
>constantMatrix :: (Applicative m, Applicative n) => a -> (m :*: n) a
>constantMatrix a = matrix (\_ _ -> a) (pure ()) (pure ())

>applyCol :: (Applicative f) => (f :*: (->) a) b -> f a -> f b
>applyCol (Matrix m) x = pure ($) <*> m <*> x

>applyRow :: ((->) a :*: g) b -> a -> g b
>applyRow (Matrix m) x = m x

>matrixA :: (ArrowApply arr,FunctorArrow f arr arr, FunctorArrow g arr arr) 
>        => arr (a,b) c -> arr (f a, g b) ((f :*: g) c)
>matrixA f = proc z -> do
>              res <- outerA f -< z
>              returnA -< Matrix res

>-- | <https://en.wikipedia.org/wiki/Modular_exponentiation>
>-- @modular_exponentiation m b c == m^b  (mod c)@

>mod_linear_map :: (Num a, Linearizable LinearMap (:*:) f f a, Diagonalizable f a, Integral (f a)) => f a -> f a :-> f a
>mod_linear_map c = linear $ functionMatrix (`mod` c)


>modularExponentiation :: (SupportsMatrixMultiplication f f f (g b),
>                          Scalar (f (g b)) ~ g b,
>                          Linearizable LinearMap (:*:) f f (g b),
>                          Linearizable LinearMap (:*:) g g b,
>                          Diagonalizable g b, LinearTransform f f b,
>                          FunctorArrow f LinearMap LinearMap,
>                          Integral b) => f (g b) :-> f (g b) -> Integer -> b -> f (g b) :-> f (g b)
>modularExponentiation m b c 
>   | b == 0         = MatIdentity
>   | b `mod` 2 == 1 = amap (linear (modmatrix c)) . 
>                         (linearMatrixMultiply m $ modularExponentiation m (pred b) c)
>   | otherwise = amap (linear (modmatrix c)) . (linearMatrixMultiply d d)
>          where d = modularExponentiation m (b `div` 2) c
>                modmatrix :: (Diagonalizable g b, Integral b) => b -> (g :*: g) b
>                modmatrix c = functionMatrix (fmap (`mod` c))

>isSymmetric :: (Eq (m (m a)), LinearTransform m m a, Linearizable LinearMap (:*:) m m a, Diagonalizable m a, Transposable m m a, Applicative m, Foldable m, Eq a)
> => m a :-> m a -> Bool
>isSymmetric m = transpose m == m

>applyVector :: (Num b, LinearTransform m m b, Linearizable LinearMap (:*:) m m b, Transposable m m b, Diagonalizable m b)
> => m (a -> b) -> m a -> m b
>applyVector f x = diagonal (f !$! x)

>applyColumns :: (Functor n, Functor m, Applicative f)
>              => (m :*: f) (a -> b) -> (n :*: f) a -> (m :*: n) (f b)
>applyColumns (Matrix a) (Matrix b) = matrix (<*>) a b

>matrixAssoc :: (Functor f) => ((f :*: g) :*: h) a -> (f :*: (g :*: h)) a
>matrixAssoc (Matrix (Matrix x)) = Matrix (fmap Matrix x)

>matrixUnassoc :: (Functor f) => (f :*: (g :*: h)) a -> ((f :*: g) :*: h) a
>matrixUnassoc (Matrix x) = Matrix $ Matrix $ fmap cells x

>matrixLeftId :: (I :*: f) a -> f a
>matrixLeftId (Matrix (I x)) = x

>matrixRightId :: (Functor f) => (f :*: I) a -> f a
>matrixRightId (Matrix f) = fmap unI f


>(!*!) :: (Functor f, Functor g, Num a) => g a -> f a -> (g :*: f) a
>x !*! y = matrix (*) x y

>(<!!>) :: (Array.Ix i, Array.Ix j) => (Array i :*: Array j) a -> (i,j) -> a
>(<!!>) m (x,y) = m <!> ((Array.! x),(Array.! y))

>projectDown :: (Functor (m \\\ m'),
>                ProjectionSpace m m',
>                ProjectionSpace n Vector1)
>               => (m :*: n) a -> (m \\\ m') a
>projectDown m = fmap vectorElement $ m <!> (projectSecond, projectFirst)
 
>projectRight :: (ProjectionSpace m Vector1, ProjectionSpace n n')
>              => (m :*: n) a -> (n \\\ n') a
>projectRight m = vectorElement $ m <!> (projectFirst, projectSecond)

>projectDiagonal :: (ProjectionSpace m m', ProjectionSpace n n')
>                 => (m :*: n) a -> (m' :*: (n \\\ n')) a

>projectDiagonal m = Matrix $ m <!> (projectFirst, projectSecond)



matrixCommutator :: (Applicative h, Transposable h h, InnerProductSpace (h a), Scalar (h a) ~ a) => (h :*: h) a -> (h :*: h) a -> (h :*: h) a

>matrixCommutator :: (VectorSpace ((f :*: f) a),
> InnerProductSpaceFunctor f a, ConjugateSymmetric a,
> Transposable f f a, Num a)
> => (f :*: f) a -> (f :*: f) a -> (f :*: f) a
>matrixCommutator m n = m %*% n %- n %*% m

>instance (Num a, ConjugateSymmetric a) => LieAlgebra ((Vector4 :*: Vector4) a) where
>   (%<>%) = matrixCommutator


>instance (Num a, ConjugateSymmetric a) => LieAlgebra ((Vector3 :*: Vector3) a) where
>   (%<>%) = matrixCommutator


matrixCommutator :: (Num a, Applicative h, Transposable h h a, Scalar a ~ a,
                      Scalar (h (h a)) ~ a,
                      InnerProductSpace (h a), VectorSpaceOver (h a) a)
                     => h a :-> h a -> h a :-> h a -> h a :-> h a
matrixCommutator m n = alift2 (pure $ proc a -> returnA -< proc b -> returnA -< a - b) -!< (m %**% n,n %**% m)

>matrixAnticommutator m n = m %*% n %+ n %*% m

matrix_anticommutator :: (Num a, Applicative h, Transposable h h a,
                      InnerProductSpace (h a), VectorSpaceOver (h a) a)
                     => h a :-> h a -> h a :-> h a-> h a :-> h a
matrix_anticommutator m n = liftA2 (+) (m %**% n) (n %**% m)

normalized_anticommutator :: (Fractional a, Applicative h, Transposable h h a,
                      InnerProductSpace (h a), VectorSpaceOver (h a) a)
                     => 
normalized_anticommutator m n = liftA (/2) (matrix_anticommutator m n)

normalized_anticommutator :: (LinearTransform h h a, Transposable h h a, InnerProductSpace (h a), Diagonalizable h a, Fractional a, VectorSpace (h a), a ~ Scalar a, a ~ Scalar (h a), a ~ Scalar (h (h a))) => h a :-> h a -> h a :-> h a -> h a :-> h a

>normalizedAnticommutator m n = (1/2) %* matrixAnticommutator m n

commute :: (Fractional a, Applicative f, Transposable f f a,
            InnerProductSpace (f a), VectorSpaceOver (f a) a) =>
   Complex (f a :-> f a) -> Complex (f a :-> f a) -> f (Complex a) :-> f (Complex a)

commute :: Complex (g a :-> g a) -> Complex (g a :-> g a) -> ((g :*: g) (Complex a)) :-> ((g :*: g) (Complex a))

>commute (x :+ y) (x' :+ y') = matrix (:+) (normalizedAnticommutator x x')
>                                          (matrixCommutator y y')


>bind_diagonal :: (Monad m) => (m :*: m) a -> (a -> m b) -> m b
>bind_diagonal (Matrix f) g = do { v <- f ; r <- v ; g r }

>pairMatrix :: (Functor f, Functor g) => f a -> g b -> (f :*: g) (a,b)
>pairMatrix = matrix (,)

>(!+!) :: (Functor f, Functor g, Num a) => g a -> f a -> (g :*: f) a
>x !+! y = matrix (+) x y

>cofunctorInverseImage :: (Functor f, CoFunctor g) => (a -> b) -> (g :*: f) b -> (g :*: f) a
>cofunctorInverseImage f (Matrix x) = Matrix $ inverseImage (fmap f) x

>instance (Contravariant f, Functor g) => Contravariant (g :*: f) where
>  contramap f (Matrix x) = Matrix $ fmap (inverseImage f) x

>cofunctorMap :: (CoFunctor f, CoFunctor g) => (a -> b) -> (g :*: f) a -> (g :*: f) b
>cofunctorMap f (Matrix x) = Matrix (inverseImage (inverseImage f) x)

instance (Comonad f, Comonad g, forall b. Transposable f g b) => Comonad (g :*: f) where
  extract (Matrix v) = extract (extract v)
  duplicate (Matrix m) = Matrix $ fmap (fmap (transposeImpl . Matrix)) (duplicate . fmap duplicate m)


instance (Monad f, Monad g, forall a. Indexable f a) => Monad ((:*:) f g) where
   return x = Matrix $ return (return x)
   (Matrix v) >>= f = Matrix $ liftA2 (\d x -> x >>= (index_project d . cells . f)) diagonal_projections v

>inTranspose :: (Monad g, Indexable f b, Integral b) => (f :*: g) a -> (a -> (g :*: f) b) -> (f :*: g) b
>inTranspose (Matrix v) f = Matrix $ liftA2 (\d x -> x >>= (fmap (indexProject d) . cells . f))
>                                            diagonalProjections v

>instance (MonadZip g, MonadZip f, forall b. Transposable f g b) => MonadZip (g :*: f) where
>   mzipWith f (Matrix a) (Matrix b) = Matrix $ mzipWith (mzipWith f) a b

>instance (MonadPlus g, MonadPlus f, forall b. Transposable f g b) => MonadPlus (g :*: f) where
>   mzero = Matrix mzero
>   mplus (Matrix f) (Matrix g) = Matrix $ liftA2 mplus f g

>instance (Array.Ix i) => PpShowF (Array i) where
>   ppf ary = ppf [ary Array.! i | i <- Array.range (Array.bounds ary) ]

>instance (Array.Ix i) => PpShowVerticalF (Array i) where
>   ppfVertical ary = Pretty.vcat [pp (ary Array.! i) | i <- Array.range (Array.bounds ary) ]

>instance (Functor f, Functor g) => Builder ((g :*: f) a) where
>   data Unfold ((g :*: f) a) b = forall c d. MatrixUnfold (c -> d -> a)
>                                                          (b -> g c)
>                                                          (b -> f d)
>   build (MatrixUnfold m g f) x = matrix m (g x) (f x)

instance (Functor f, Functor g) => Builder (Matrix g f a) b where
   type Unfold (Matrix g f a) b = (c -> d -> a,Unfold (g c) b,Unfold (f d) b)
   build (CUnfold h z1 z2) v = Matrix (outer h (build z1 v) (build z2 v))

instance (Functor g, Functor f, Builder a) => Builder (Matrix g f a) where
   data Unfold (Matrix g f a) b = MatrixUnfold (b -> g b) (b -> f b) (Unfold a b)
   build (MatrixUnfold gt ft z) x = Matrix (fmap (fmap (build z). ft) (gt x))

>fmap_split :: (SplittableVector m n, AppendableVector m' n') =>
>   (m a -> m' a') -> (n a -> n' a') -> (m :+: n) a -> (m' :+: n') a'
>fmap_split f g x = let (a,b) = vsplit x in (f a ||>> g b)

>split_matrix :: (SplittableVector m n, SplittableVector f f', Functor (f :+: f'), Functor (m :+: n))
>             => ((f :+: f') :*: (m :+: n)) a -> 
>                           (((f :*: m) a, (f :*: n) a),
>                           ((f' :*: m) a, (f' :*: n) a))
>split_matrix m = ((Matrix $ fmap fst a, Matrix $ fmap snd a),
>                  (Matrix $ fmap fst b, Matrix $ fmap snd b))
>   where (a,b) = m <!> (vsplit,vsplit)


>append_matrix :: (AppendableVector m' n', AppendableVector m n) =>
>    (m :*: m') a -> (m :*: n') a -> (n :*: m') a -> (n :*: n') a -> ((m :+: n) :*: (m' :+: n')) a
>append_matrix (Matrix a) (Matrix b) (Matrix c) (Matrix d) = Matrix $ (liftA2 (||>>) a b) ||>> (liftA2 (||>>) c d)

>matrix_iso :: f (g a) :==: (f :*: g) a
>matrix_iso = Matrix <-> cells

>matrix_size :: (CoordinateSpace (g (f a)), CoordinateSpace (f (g a)), Transposable f g a)
>  => (f :*: g) a -> (Int,Int)
>matrix_size m = ((dimensionSize $ cells m),
>                 (dimensionSize $ cells $ transposeImpl m))

>data Strassen_Split f g a where
>  Strassen_Two :: (Vector2 :*: Vector2) a -> Strassen_Split Vector1 Vector1 a
>  Strassen_PowerOfTwo :: Strassen_Split f g a -> Strassen_Split f g a
>                     -> Strassen_Split f g a -> Strassen_Split f g a
>                     -> Strassen_Split f g a

>strassen_plus :: (Num a) => Strassen_Split f g a -> Strassen_Split f g a -> Strassen_Split f g a
>strassen_plus (Strassen_Two a) (Strassen_Two b) = Strassen_Two (a %+ b)
>strassen_plus (Strassen_PowerOfTwo a b c d) (Strassen_PowerOfTwo a' b' c' d')
>   = Strassen_PowerOfTwo (strassen_plus a a') (strassen_plus b b') (strassen_plus c c') (strassen_plus d d')
>strassen_minus :: (Num a) => Strassen_Split f g a -> Strassen_Split f g a -> Strassen_Split f g a
>strassen_minus (Strassen_Two a) (Strassen_Two b) = Strassen_Two (a %- b)
>strassen_minus (Strassen_PowerOfTwo a b c d) (Strassen_PowerOfTwo a' b' c' d')
>   = Strassen_PowerOfTwo (strassen_minus a a') (strassen_minus b b') (strassen_minus c c') (strassen_minus d d')

>-- | <https://en.wikipedia.org/wiki/Strassen_algorithm>
>strassen_matrix_multiply ::
> (ConjugateSymmetric a,Num a) => Strassen_Split f g a -> Strassen_Split g h a -> Strassen_Split f h a
>strassen_matrix_multiply (Strassen_Two m) (Strassen_Two n) = Strassen_Two (m %*% n)
>strassen_matrix_multiply (Strassen_PowerOfTwo a11 a12 a21 a22) (Strassen_PowerOfTwo b11 b12 b21 b22)
>     = Strassen_PowerOfTwo c11 c12 c21 c22
>  where c11 = m1 `strassen_plus` m4 `strassen_minus` m5 `strassen_plus` m7
>        c12 = m3 `strassen_plus` m5
>        c21 = m2 `strassen_plus` m4
>        c22 = m1 `strassen_minus` m2 `strassen_plus` m3 `strassen_plus` m6
>        m1 = (a11 `strassen_plus` a22) `strassen_matrix_multiply` (b11 `strassen_plus` b22)
>        m2 = (a21 `strassen_plus` a22) `strassen_matrix_multiply` b11
>        m3 = a11 `strassen_matrix_multiply` (b12 `strassen_minus` b22)
>        m4 = a22 `strassen_matrix_multiply` (b21 `strassen_minus` b11)
>        m5 = (a11 `strassen_plus` a12) `strassen_matrix_multiply` b22
>        m6 = (a21 `strassen_minus` a11) `strassen_matrix_multiply` (b11 `strassen_plus` b12)
>        m7 = (a12 `strassen_minus` a22) `strassen_matrix_multiply` (b21 `strassen_plus` b22)
