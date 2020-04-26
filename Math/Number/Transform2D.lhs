>{-# LANGUAGE Safe, TypeOperators, GADTs, FlexibleContexts #-}
>module Math.Number.Transform2D where
>import safe Data.Sequence (Seq, (<|), (|>))
>import safe qualified Data.Sequence as Seq
>import safe qualified Data.Set as Set
>import safe qualified Control.Category as C
>import safe Control.Applicative
>import safe Control.Arrow
>import safe Math.Number.Stream
>import safe Math.Matrix.Interface
>import safe Math.Tools.Median
>import safe Math.Tools.Arrow as TArrow
>
>data Transform2D a b = Transform2D {
>   transform_diagonal   :: a -> b,
>   transform_commutator :: a -> a -> b,
>   transform_symmetry :: a -> b -> a -> b }

>-- | This operation allows customizing the behaviour of the codiagonals transformation.
>codiagonalsWith :: Transform2D a b -> (Stream :*: Stream) a -> Stream b
>codiagonalsWith z@(Transform2D diag pair thr) q = Pre (diag d) $ Pre (pair x y) $
>             liftA3 thr xr (codiagonalsWith z m) yr 
>    where ~(d,(Pre x xr, Pre y yr),m) = dematrix q

>element2D :: a -> a -> Transform2D () a
>element2D x y = Transform2D (\ () -> x) (\ () () -> y) (\ () m () -> m)

>vector2D :: (Floating (Scalar v), InnerProductSpace v)
>  => (v -> Scalar v -> v -> Scalar v) -> Transform2D v (Scalar v)
>vector2D = Transform2D innerproductspace_norm (%.)

>diagonalMatrix2D :: (SquareMatrix m a)
>         => (a -> a -> a)
>         -> (m a -> (m :*: m) a -> m a -> (m :*: m) a)
>         -> Transform2D (m a) ((m :*: m) a)
>diagonalMatrix2D f = Transform2D diagonal_matrix (matrix f)

>functor2D :: (TArrow.FunctorArrow f arr, C.Category arr) =>
>   (arr a a -> arr a a -> arr (f a) (f a))
>   -> Transform2D (arr a a) (arr (f a) (f a))
>functor2D g = Transform2D TArrow.amap g (\x y z -> TArrow.amap x <<< y <<< TArrow.amap z)

>category2D :: (C.Category cat) => Transform2D (cat a a) (cat a a)
>category2D = Transform2D id (<<<) (\x y z -> x <<< y <<< z)

>arrow2D :: (TArrow.BiArrow arr, Arrow arr) => Transform2D (a -> a) (arr a a)
>arrow2D = Transform2D arr (TArrow.<->) (\f x g -> arr f <<< x <<< arr g)

>median2D :: (MedianAlgebra a) => Transform2D a a
>median2D = Transform2D id min med

>commutatorSum2D :: (LieAlgebra a) => Transform2D a a
>commutatorSum2D = Transform2D vnegate (%<>%) (\x y z -> x %+ y %+ z)

>transpose2D :: Transform2D a b -> Transform2D a b
>transpose2D (Transform2D f c s) = Transform2D f (flip c) $ \a m b -> s b m a

>applicative2D :: (Applicative f) => (a -> b) -> (a -> a -> b) -> Transform2D (f a) (f b)
>applicative2D f g = Transform2D (fmap f) (liftA2 g) (\x f y -> x *> f <* y)

>compose2D :: Transform2D b c -> Transform2D a b -> Transform2D a c
>compose2D (Transform2D d c s) (Transform2D d' c' s')
>  = Transform2D (d . d')
>                (\a b -> c (c' a b) (c' b a))
>                (\a m b -> s (s' a (d' a) b) m (s' a (d' b) b))

>nested2D :: (Transform2D a a -> Transform2D a a -> Transform2D a a -> Transform2D a a)
>         -> Transform2D (Transform2D a a) (Transform2D a a)
>nested2D = Transform2D transpose2D compose2D

>monoid2D :: (Monoid a) => (a -> a -> a -> a) -> Transform2D a a
>monoid2D = Transform2D (const mempty) mappend

>productSum2D :: (Num a) => Transform2D a a
>productSum2D = Transform2D id (*) $ \a m b -> a + m + b

>set2D :: (Ord a) => Transform2D a (Set.Set a)
>set2D = Transform2D Set.singleton (\a b -> Set.insert b (Set.singleton a)) (\a s b -> Set.insert a (Set.insert b s))

>project2D :: (a -> (a,a) -> a -> (a,a)) -> Transform2D a (a,a)
>project2D f = Transform2D (\a -> (a,a)) (,) f

>flattenList2D :: Transform2D [a] [a]
>flattenList2D = Transform2D id (++) (\a b c -> a ++ b ++ c)

>matrix2D :: (Transposable m m, InnerProductSpace (m a), Scalar (m a) ~ a)
>        => ((m :*: m) a -> (m :*: m) a -> (m :*: m) a -> (m :*: m) a)
>        -> Transform2D ((m :*: m) a) ((m :*: m) a)
>matrix2D = Transform2D transpose (%*%)

>matrixProduct2D :: (VectorSpace ((f :*: f) a), Transposable f f,
> InnerProductSpace (f a), VectorSpaceOver (f a) a)
> => Transform2D ((f :*: f) a) ((f :*: f) a)
>matrixProduct2D = Transform2D transpose (\a b -> a %**% b %- b %**% a) (\a m b -> a %**% m %**% transpose b)

>list2D :: Transform2D a [a]
>list2D = Transform2D (\a -> [a]) (\a b -> [a,b]) (\a m b -> a : m ++ [b])

>seq2D :: Transform2D a (Seq a)
>seq2D = Transform2D (\a -> Seq.singleton a) (\a b -> Seq.singleton a Seq.|> b)
>                    (\a m b -> a Seq.<| (m Seq.|> b))

