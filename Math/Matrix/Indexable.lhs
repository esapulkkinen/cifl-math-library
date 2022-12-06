>{-# LANGUAGE TypeOperators, Safe #-}
>{-# LANGUAGE FlexibleInstances #-}
>{-# LANGUAGE MultiParamTypeClasses #-}
>{-# LANGUAGE FlexibleContexts #-}
>{-# LANGUAGE RankNTypes #-}
>{-# LANGUAGE QuantifiedConstraints #-}
>module Math.Matrix.Indexable where
>import Control.Applicative
>import Prelude hiding ((.), id)
>import Control.Category
>import Math.Matrix.Interface
>import Math.Tools.I
>import Math.Tools.Isomorphism
>import Math.Tools.Arrow

>instance (Num a) => Indexable I a where
>  diagonal_projections = I id
>  indexable_indices = I 0

>instance (Integral a, Indexable f a, Indexable g a) => Indexable (f :*: g) a where
>  diagonal_projections = matrix (\x y -> (I . (<!> (index_project x,index_project y))) <->
>                                           (\ a' -> matrix (*) (isomorphism_section x $ a') (isomorphism_section y $ a'))) 
>                                diagonal_projections diagonal_projections
>  indexable_indices = matrix (\x y -> (x+y)*(x+y+1)`div` 2 + x) indexable_indices indexable_indices

indexable_diagonal :: (Indexable f) => (f :*: f) a -> a -> f a
indexable_diagonal ~(Matrix m) a = index_project (diagonal_projections a) <*> m

indexable_apply :: (Indexable f) => (f :*: f) (a -> b) -> f a -> f b
indexable_apply ~(Matrix m) x = index_project (diagonal_projections x) <*> m <*> x

indexable_transpose :: (Functor f, Functor g, Indexable g) =>
                       (f :*: g) a -> a -> (g :*: f) a
indexable_transpose ~(Matrix m) n = matrix id (index_project (diagonal_projections n)) m

indexable_matrix_projections :: (Indexable f, Indexable g)
                             => a -> b -> (f :*: g) (Index (f :*: g) a)

indexable_matrix_projections ::
   a -> a -> (m :*: I) (m a :==: I a)

>indexable_matrix_projections ::
>  (Indexable m (n a), Indexable n a)
> => (m :*: n) (Index (m :*: n) a)
>indexable_matrix_projections = matrix (\f g -> (unI <-> I) . amap g . f . (cells <-> Matrix))
>       diagonal_projections
>       diagonal_projections

transposedIndices :: (Indexable f, Indexable g) => a -> b -> (g :*: f) (Index (f :*: g) a)
transposedIndices a b = fmap (. (cells <-> Matrix)) $ matrix (.) (diagonal_projections a) (diagonal_projections b)


join_matrix :: (Indexable f, Indexable g) 
            => (f :*: g) ((f :*: g) b) -> b -> c -> (f :*: g) b

join_matrix m a c = fmap (\ (Iso s p) -> (s <*> m) <-> (p <*> m)) $
                       indexable_matrix_projections a c

join_matrix m = cells (fmap cells m)


>apply_index :: (Indexable m a) => Index m a -> m a -> a
>apply_index = index_project

>with :: (Indexable f a) => (Index f a -> b) -> f b
>with f = liftA f $ diagonal_projections
 
>with2 :: (Indexable f a, Indexable g b)
>      => (Index f a -> Index g b -> c) -> (f :*: g) c
>with2 f = Matrix $ with $ \i -> with $ \j -> f i j

>with3 :: (Indexable f a, Indexable g b, Indexable h c)
>      => (Index f a -> Index g b -> Index h c -> d)-> (f :*: (g :*: h)) d
>with3 f = Matrix $ with $ \i -> Matrix $
>                   with $ \j ->
>                   with $ \k -> f i j k

kronecker_delta :: (forall b. (Eq b) => Indexable f b, Num a) => (f :*: f) a
kronecker_delta = matrix (\i j -> if i == j then 1 else 0) indexable_indices indexable_indices

>equal_indices :: (Indexable f Int) => Index f Int -> Index f Int -> Bool
>equal_indices a b = a `index_project` indexable_indices
>                  == b `index_project` indexable_indices

>sum_indices :: (Foldable f, Indexable f b, Num b) => (f :*: f) b -> b
>sum_indices f = sum_coordinates $ with $ \i ->
>    f <!> (index_project i,index_project i)
