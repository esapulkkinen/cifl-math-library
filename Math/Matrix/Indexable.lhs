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

>instance Indexable I a where
>  diagonalProjections = I (MakeIndex unI (\x (I _) -> I x))
>  indexableIndices = I 0

>instance {-# OVERLAPPABLE #-} (forall b. Indexable f b, forall c. Indexable g c) => Indexable (f :*: g) a where
>  diagonalProjections = indexableMatrixProjections
>  indexableIndices = matrix (\x y -> (x+y)*(x+y+1)`div` 2 + x) indexableIndices indexableIndices

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

>indexableMatrixProjections ::
>  (forall b. Indexable m b, forall c. Indexable n c)
> => (m :*: n) (Index (m :*: n) a)
>indexableMatrixProjections = matrix comp diagonalProjections diagonalProjections
>  where comp f g = MakeIndex (scalarProjection g . scalarProjection f . cells)
>                             (\ a (Matrix v) -> Matrix $ modifyVector f (modifyVector g a (scalarProjection f v)) v)


 matrix (\f g -> (unI <-> I) . amap g . f . (cells <-> Matrix))
       diagonalProjections
       diagonalProjections

transposedIndices :: (Indexable f, Indexable g) => a -> b -> (g :*: f) (Index (f :*: g) a)
transposedIndices a b = fmap (. (cells <-> Matrix)) $ matrix (.) (diagonal_projections a) (diagonal_projections b)


join_matrix :: (Indexable f, Indexable g) 
            => (f :*: g) ((f :*: g) b) -> b -> c -> (f :*: g) b

join_matrix m a c = fmap (\ (Iso s p) -> (s <*> m) <-> (p <*> m)) $
                       indexable_matrix_projections a c

join_matrix m = cells (fmap cells m)


>applyIndex :: (Indexable m a) => Index m a -> m a -> a
>applyIndex = indexProject

>with :: (Indexable f a, Num a) => (Index f a -> b) -> f b
>with f = liftA f $ diagonalProjections
 
>with2 :: (Indexable f a, Indexable g b, Num a, Num b)
>      => (Index f a -> Index g b -> c) -> (f :*: g) c
>with2 f = Matrix $ with $ \i -> with $ \j -> f i j

>with3 :: (Indexable f a, Indexable g b, Indexable h c, Num a, Num b, Num c)
>      => (Index f a -> Index g b -> Index h c -> d)-> (f :*: (g :*: h)) d
>with3 f = Matrix $ with $ \i -> Matrix $
>                   with $ \j ->
>                   with $ \k -> f i j k

kronecker_delta :: (forall b. (Eq b) => Indexable f b, Num a) => (f :*: f) a
kronecker_delta = matrix (\i j -> if i == j then 1 else 0) indexable_indices indexable_indices

>equalIndices :: (Indexable f Int) => Index f Int -> Index f Int -> Bool
>equalIndices a b = a `indexProject` indexableIndices
>                  == b `indexProject` indexableIndices

>sumIndices :: (Foldable f, Indexable f b, Num b) => (f :*: f) b -> b
>sumIndices f = sumCoordinates $ with $ \i ->
>    f <!> (indexProject i,indexProject i)
