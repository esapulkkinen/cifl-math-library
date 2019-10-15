>{-# LANGUAGE Safe,ExistentialQuantification, TypeOperators, Rank2Types #-}
>{-# LANGUAGE Arrows, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
>{-# LANGUAGE FlexibleInstances, PatternSynonyms #-}
>module Math.Matrix.Matrix where
>import Data.Complex
>import Data.Array (Array)
>import qualified Data.Array as Array
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
>import Math.Tools.CoMonad

The :*: should be used when the number of dimensions of matrix is
exactly 2.

>type family Transpose a
>type instance Transpose ((f :*: g) a) = (g :*: f) a

>conjIso :: (ConjugateSymmetric a) => a :==: a
>conjIso = conj <-> conj

>transposeIso :: (Transposable m n, Transposable n m) => (m :*: n) a :==: (n :*: m) a
>transposeIso = transpose <-> transpose

>vnegateIso :: (VectorSpace v) => v :==: v
>vnegateIso = vnegate <-> vnegate

>-- | See video by Bartosz Milewski ("Category theory II 7.2: Comonads
>-- categorically and examples")
>instance (Comonad f, Comonad g, Transposable g f, Transposable f g)
>  => Comonad (f :*: g) where
>   extract (Matrix m) = extract (extract m)
>   duplicate (Matrix m) = fmap Matrix
>                        $ transpose $ Matrix $ fmap duplicate $ cells
>                        $ transpose $ Matrix $ fmap duplicate m

>deriving instance (Eq a, Applicative f, Applicative g, Foldable f, Foldable g, Ord (f (g a))) => Ord ((f :*: g) a)

>instance (Eq a, Applicative f, Applicative g, Foldable f, Foldable g)
>    => Eq ((f :*: g) a) where
>  x == y = liftA2 (==) x y <!> (and,and)

instance Comonad ((,) a :*: (->) a) where
   extract (Matrix (x,f)) = f x
   duplicate (Matrix (a,f)) = Matrix (a,\x -> Matrix (x,f))


>(!$!) :: (Functor f, Functor g) => f (a -> b) -> g a -> (f :*: g) b
>(!$!) = matrix ($)

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

>matrixA :: (ArrowApply arr,FunctorArrow f arr, FunctorArrow g arr) 
>        => arr (a,b) c -> arr (f a, g b) ((f :*: g) c)
>matrixA f = proc z -> do
>              res <- outerA f -< z
>              returnA -< Matrix res

>-- | <https://en.wikipedia.org/wiki/Modular_exponentiation>
>-- @modular_exponentiation m b c == m^b  (mod c)@
>modular_exponentiation :: (Integral a, SquareMatrix f a, Functor f,
>                           InnerProductSpace (f a), a ~ Scalar (f a)) 
>                      => (f :*: f) a -> Integer -> a -> (f :*: f) a
>modular_exponentiation m b c 
>   | b == 0         = identity
>   | b `mod` 2 == 1 = fmap (`mod` c) $ 
>                         m %*% modular_exponentiation m (pred b) c
>   | otherwise = fmap (`mod` c) $ d %*% d
>          where d = modular_exponentiation m (b `div` 2) c

>isSymmetric :: (Transposable m m, Applicative m, Foldable m, Eq a) => (m :*: m) a -> Bool
>isSymmetric m = transpose m == m

>apply_vector :: (SquareMatrix m b) => m (a -> b) -> m a -> m b
>apply_vector f x = diagonal (f !$! x)

>apply_columns :: (Functor n, Functor m, Applicative f)
>              => (m :*: f) (a -> b) -> (n :*: f) a -> (m :*: n) (f b)
>apply_columns (Matrix a) (Matrix b) = matrix (<*>) a b

>matrixAssoc :: (Functor f) => (f :*: g :*: h) a -> (f :*: (g :*: h)) a
>matrixAssoc (Matrix (Matrix x)) = Matrix (fmap Matrix x)

>matrixUnassoc :: (Functor f) => (f :*: (g :*: h)) a -> (f :*: g :*: h) a
>matrixUnassoc (Matrix x) = Matrix $ Matrix $ fmap cells x

>matrixLeftId :: (I :*: f) a -> f a
>matrixLeftId (Matrix (I x)) = x

>matrixRightId :: (Functor f) => (f :*: I) a -> f a
>matrixRightId (Matrix f) = fmap unI f

example use: m <!> (xcoord3,ycoord3)

>(<!>) :: (Functor f, Functor g) => (g :*: f) a -> (g c -> b,f a -> c) -> b
>m <!> (x,y) = MatrixFold (x,y) `visit` m

>(!*!) :: (Functor f, Functor g, Num a) => g a -> f a -> (g :*: f) a
>x !*! y = matrix (*) x y

>(<!!>) :: (Array.Ix i, Array.Ix j) => (Array i :*: Array j) a -> (i,j) -> a
>(<!!>) m (x,y) = m <!> ((Array.! x),(Array.! y))



matrix_commutator :: (Applicative h, Transposable h h, InnerProductSpace (h a), Scalar (h a) ~ a) => (h :*: h) a -> (h :*: h) a -> (h :*: h) a

>matrix_commutator :: (Num a, Applicative h, Transposable h h,
>                      InnerProductSpace (h a), VectorSpaceOver (h a) a)
>                     => (h :*: h) a -> (h :*: h) a -> (h :*: h) a 
>matrix_commutator m n = liftA2 (-) (m %**% n) (n %**% m)

>matrix_anticommutator :: (Num a, Applicative h, Transposable h h,
>                      InnerProductSpace (h a), VectorSpaceOver (h a) a)
>                     => (h :*: h) a -> (h :*: h) a -> (h :*: h) a 
>matrix_anticommutator m n = liftA2 (+) (m %**% n) (n %**% m)

>normalized_anticommutator :: (Fractional a, Applicative h, Transposable h h,
>                      InnerProductSpace (h a), VectorSpaceOver (h a) a)
>                     => (h :*: h) a -> (h :*: h) a -> (h :*: h) a 
>normalized_anticommutator m n = liftA (/2) (matrix_anticommutator m n)

>commute :: (Fractional a, Applicative f, Transposable f f,
>            InnerProductSpace (f a), VectorSpaceOver (f a) a) =>
>   Complex ((f :*: f) a) -> Complex ((f :*: f) a) -> (f :*: f) (Complex a)
>commute (x :+ y) (x' :+ y') = liftA2 (:+) (normalized_anticommutator x x')
>                                          (matrix_commutator y y')


>bind_diagonal :: (Monad m) => (m :*: m) a -> (a -> m b) -> m b
>bind_diagonal (Matrix f) g = do { v <- f ; r <- v ; g r }

>pairMatrix :: (Functor f, Functor g) => f a -> g b -> (f :*: g) (a,b)
>pairMatrix = matrix (,)

>(!+!) :: (Functor f, Functor g, Num a) => g a -> f a -> (g :*: f) a
>x !+! y = matrix (+) x y

>cofunctor_inverse_image :: (Functor f, CoFunctor g) => (a -> b) -> (g :*: f) b -> (g :*: f) a
>cofunctor_inverse_image f (Matrix x) = Matrix $ inverse_image (fmap f) x

>instance (CoFunctor f, Functor g) => CoFunctor (g :*: f) where
>  inverse_image f (Matrix x) = Matrix $ fmap (inverse_image f) x


>cofunctor_map :: (CoFunctor f, CoFunctor g) => (a -> b) -> (g :*: f) a -> (g :*: f) b
>cofunctor_map f (Matrix x) = Matrix (inverse_image (inverse_image f) x)

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

>instance (Monad f, Monad g, Indexable f) => Monad ((:*:) f g) where
>   return x = Matrix $ return (return x)
>   (Matrix v) >>= f = Matrix $ liftA2 (\d x -> x >>= (d . cells . f)) diagonal_projections v

>in_transpose :: (Monad g, Indexable f) => (f :*: g) a -> (a -> (g :*: f) b) -> (f :*: g) b
>in_transpose (Matrix v) f = Matrix $ liftA2 (\d x -> x >>= (fmap d . cells . f)) diagonal_projections v

>instance (MonadZip g, MonadZip f, Indexable g) => MonadZip (g :*: f) where
>   mzipWith f (Matrix a) (Matrix b) = Matrix $ mzipWith (mzipWith f) a b

>instance (MonadPlus g, Indexable g, MonadPlus f) => MonadPlus (g :*: f) where
>   mzero = Matrix mzero
>   mplus (Matrix f) (Matrix g) = Matrix $ liftA2 mplus f g

>instance (Array.Ix i) => PpShowF (Array i) where
>   ppf ary = ppf [ary Array.! i | i <- Array.range (Array.bounds ary) ]

>instance (Array.Ix i) => PpShowVerticalF (Array i) where
>   ppf_vertical ary = Pretty.vcat [pp (ary Array.! i) | i <- Array.range (Array.bounds ary) ]

>instance (Functor f, Functor g) => Builder ((g :*: f) a) where
>   data Unfold ((g :*: f) a) b = forall c d. MatrixUnfold (c -> d -> a)
>                                                          (b -> g c)
>                                                          (b -> f d)
>   build (MatrixUnfold m g f) x = matrix m (g x) (f x)

>instance (Functor g) => Visitor ((g :*: f) a) where
>   data Fold ((g :*: f) a) b = forall c. MatrixFold (g c -> b,f a -> c)
>   visit (MatrixFold (gt,ft)) (Matrix x) = gt (fmap ft x)


instance (Functor f, Functor g) => Builder (Matrix g f a) b where
   type Unfold (Matrix g f a) b = (c -> d -> a,Unfold (g c) b,Unfold (f d) b)
   build (CUnfold h z1 z2) v = Matrix (outer h (build z1 v) (build z2 v))

instance (Functor g, Functor f, Builder a) => Builder (Matrix g f a) where
   data Unfold (Matrix g f a) b = MatrixUnfold (b -> g b) (b -> f b) (Unfold a b)
   build (MatrixUnfold gt ft z) x = Matrix (fmap (fmap (build z). ft) (gt x))

>fmap_split :: (SplittableVector m n, AppendableVector m' n') =>
>   (m a -> m' a') -> (n a -> n' a') -> (m :+: n) a -> (m' :+: n') a'
>fmap_split f g x = let (a,b) = vsplit x in (f a |> g b)

>split_matrix :: (SplittableVector m n, SplittableVector f f', Functor (f :+: f'), Functor (m :+: n))
>             => ((f :+: f') :*: (m :+: n)) a -> 
>                           (((f :*: m) a, (f :*: n) a),
>                           ((f' :*: m) a, (f' :*: n) a))
>split_matrix m = ((Matrix $ fmap fst a, Matrix $ fmap snd a),
>                  (Matrix $ fmap fst b, Matrix $ fmap snd b))
>   where (a,b) = m <!> (vsplit,vsplit)


>join_matrix :: (AppendableVector m' n', AppendableVector m n) =>
>    (m :*: m') a -> (m :*: n') a -> (n :*: m') a -> (n :*: n') a -> ((m :+: n) :*: (m' :+: n')) a
>join_matrix (Matrix a) (Matrix b) (Matrix c) (Matrix d) = Matrix $ (liftA2 (|>) a b) |> (liftA2 (|>) c d)

>matrix_iso :: f (g a) :==: (f :*: g) a
>matrix_iso = Matrix <-> cells

