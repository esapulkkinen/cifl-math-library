>{-# LANGUAGE TypeOperators, Safe #-}
>module Math.Matrix.Indexable where
>import Control.Applicative
>import Math.Matrix.Interface
>import Math.Matrix.Matrix
>
>instance (Indexable f, Indexable g) => Indexable (f :*: g) where
>  diagonal_projections = matrix (\x y -> (<!> (x,y))) 
>                                diagonal_projections diagonal_projections
>  indexable_indices = matrix (\x y -> (x+y)*(x+y+1)`div` 2 + x) indexable_indices indexable_indices

>instance (Summable f, Summable g) => Summable (f :*: g) where
>   sum_coordinates ~(Matrix m) = sum_coordinates (fmap sum_coordinates m)

>indexable_diagonal :: (Indexable f) => (f :*: f) a -> f a
>indexable_diagonal ~(Matrix m) = diagonal_projections <*> m

>indexable_apply :: (Indexable f) => (f :*: f) (a -> b) -> f a -> f b
>indexable_apply ~(Matrix m) x = diagonal_projections <*> m <*> x

>indexable_transpose :: (Functor f, Functor g, Indexable g) =>
>                       (f :*: g) a -> (g :*: f) a
>indexable_transpose ~(Matrix m) = matrix id diagonal_projections m

>indexable_matrix_projections :: (Indexable f, Indexable g)
>                             => (f :*: g) (Index (f :*: g) a)
>indexable_matrix_projections = matrix (\f g (Matrix m) -> (g . f) m) diagonal_projections diagonal_projections
>
>transposedIndices :: (Indexable f, Indexable g) => (g :*: f) (Index (f :*: g) a)
>transposedIndices = fmap (. cells) $ matrix (.) diagonal_projections diagonal_projections


>join_matrix :: (Indexable f, Indexable g) 
>            => (f :*: g) ((f :*: g) b) -> (f :*: g) b
>join_matrix m = indexable_matrix_projections <*> m

>apply_index :: Index m a -> m a -> a
>apply_index = id

>with :: (Indexable f) => (Index f a -> b) -> f b
>with f = liftA f diagonal_projections
 
>with2 :: (Indexable f, Indexable g)
>      => (Index f a -> Index g b -> c) -> (f :*: g) c
>with2 f = Matrix $ with $ \i -> with $ \j -> f i j

>with3 :: (Indexable f, Indexable g, Indexable h)
>      => (Index f a -> Index g b -> Index h c -> d) -> (f :*: (g :*: h)) d
>with3 f = Matrix $ with $ \i -> Matrix $ with $ \j -> with $ \k -> f i j k

>kronecker_delta :: (Indexable f, Num a) => (f :*: f) a
>kronecker_delta = matrix (\i j -> if i == j then 1 else 0) indexable_indices indexable_indices

>equal_indices :: (Indexable f) => Index f Int -> Index f Int -> Bool
>equal_indices a b = a indexable_indices == b indexable_indices

>sum_indices :: (Summable f, Num b) => (f :*: f) b -> b
>sum_indices f = sum_coordinates $ with $ \i -> f <!> (i,i)
