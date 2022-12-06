>{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
>{-# LANGUAGE FlexibleContexts, UndecidableInstances, GADTs #-}
>module Math.Matrix.LinearOperations where
>import Math.Matrix.Vector1
>import Math.Matrix.Vector4
>import Math.Matrix.Interface
>import Math.Matrix.Covector
>import Math.Matrix.Linear
>import qualified Math.Matrix.Linear as Covector
>import Math.Number.Interface
>import Math.Number.StreamInterface

>cov4 :: (Num a, ConjugateSymmetric a) => Vector4 (Dual (Vector4 a))
>cov4 = Vector4 (covector tcoord4) (covector xcoord4) (covector ycoord4) (covector zcoord4)

>diagonal_projections4 :: (Num a, ConjugateSymmetric a) => Vector4 (Dual (Vector4 a))
>diagonal_projections4 = Vector4 (covector tcoord4)
>                                (covector xcoord4)
>                                (covector ycoord4)
>                                (covector zcoord4)

>index4_dual :: (Num a, ConjugateSymmetric a) => Int -> Dual (Vector4 a)
>index4_dual = covector . index4

>instance (Num a, ConjugateSymmetric a) => ProjectionDual Vector4 Dual a where
>   projection_dual = cov4

>partial_derivate4x :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partial_derivate4x = partial_derivate dx_4

>partial_derivate4y :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partial_derivate4y = partial_derivate dy_4

>partial_derivate4z :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partial_derivate4z = partial_derivate dz_4

>partial_derivate4t :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partial_derivate4t = partial_derivate dt_4

>derivate4t_squared :: (Closed a, ConjugateSymmetric a)
>     => Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4t_squared = operator_map (partial_derivate4t . partial_derivate4t)

>derivate4t :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4t = operator_map partial_derivate4t

>derivate4x :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4x = operator_map (partial_derivate4x)

>derivate4y :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4y = operator_map (partial_derivate4y)
> 
>derivate4z :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4z = operator_map (partial_derivate4z)

>del_partial4 :: (DifferentiallyClosed a) => (Vector4 a -> a) -> Vector4 a -> Vector4 a
>del_partial4 f (Vector4 x y z t) = Vector4 (partial1_4 ff x y z t)
>                                           (partial2_4 ff x y z t)
>                                           (partial3_4 ff x y z t)
>                                           (partial4_4 ff x y z t)
>   where ff a b c d = f (Vector4 a b c d)

>instance DifferentialOperator Vector4 where
>   partial = del_partial4


>del4 :: (Closed a, ConjugateSymmetric a)
>      => Vector4 (Dual (Vector4 a) -> Dual (Vector4 a))
>del4 = Vector4 derivate4t derivate4x derivate4y derivate4z

>del4_ :: (Closed a) => Vector4 ((Vector4 a -> a) -> Vector4 a -> a)
>del4_ = Vector4 partial_derivate4t partial_derivate4x partial_derivate4y
>               partial_derivate4z

>hessian4 :: (Closed v, ConjugateSymmetric v)
>  => Dual (Vector4 v) -> (Vector4 :*: Vector4) (Dual (Vector4 v))
>hessian4 f = matrix (\a b -> a (b f)) del4 del4

>grad4 :: (Closed a, ConjugateSymmetric a) => Covector.Dual (Vector4 a) -> LinearMap (Vector4 a) (Vector4 a)
>grad4 f = let f' = bracket f in arr_linear $ \x -> Vector4
>    (partial_derivate4t f' x)
>    (partial_derivate4x f' x)
>    (partial_derivate4y f' x)
>    (partial_derivate4z f' x)

>curl4 :: (Closed a, ConjugateSymmetric a, LinearTransform Vector4 Vector1 a) => Vector4 (Covector.Dual (Vector4 a))
>                                    -> Vector4 a
>                                    -> (Vector4 :*: Vector4) a
>curl4 (Vector4 ft' fx' fy' fz')
>    v@(Vector4 t x y z) = Matrix $ Vector4
>               (Vector4 0
>                        (partial_derivate4x ft v - partial_derivate4t fx v)
>                        (partial_derivate4y ft v - partial_derivate4t fy v)
>                        (partial_derivate4z ft v - partial_derivate4t fz v))
> 
>               (Vector4 (partial_derivate4t fx v - partial_derivate4x ft v)
>                        0
>                        (partial_derivate4y fx v - partial_derivate4x fy v)
>                        (partial_derivate4z fx v - partial_derivate4x fz v))
>               (Vector4 (partial_derivate4t fy v - partial_derivate4y ft v)
>                        (partial_derivate4x fy v - partial_derivate4y fx v)
>                        0
>                        (partial_derivate4z fy v - partial_derivate4y fz v))
>               (Vector4 (partial_derivate4t fz v - partial_derivate4z ft v)
>                        (partial_derivate4x fz v - partial_derivate4z fx v)
>                        (partial_derivate4y fz v - partial_derivate4z fy v)
>                        0)
>       where ft = bracket ft'
>             fx = bracket fx'
>             fy = bracket fy'
>             fz = bracket fz'

>instance (Num a, Closed a, ConjugateSymmetric a, LinearTransform Vector4 Vector1 a) => VectorDerivative (Vector4 a) Dual LinearMap where
>   divergence = divergence4
>   grad = grad4
>   directional_derivative = directional_derivative_impl

>divergence4 :: (Closed a, ConjugateSymmetric a) => (Vector4 a) :-> (Vector4 a) -> Covector.Dual (Vector4 a)
>divergence4 f = covector $ \z -> partial_derivate4t (tcoord4 . (-!<) f) z +
>                                partial_derivate4x (xcoord4 . (-!<) f) z +
>                                partial_derivate4y (ycoord4 . (-!<) f) z +
>                                partial_derivate4z (zcoord4 . (-!<) f) z

>laplace4 :: (Closed a, ConjugateSymmetric a) => Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>laplace4 f = divergence4 (grad4 f)

>instance (Closed a, ConjugateSymmetric a, LinearTransform Vector4 Vector1 a)
> => VectorLaplacian (Vector4 a) LinearMap where
>  vector_laplace = vector_laplace4

>vector_laplace4 :: (VectorDerivative (v a) Dual LinearMap,
>                   Dualizable (v a) Dual,
>                   Diagonalizable v a, 
>                   LinearTransform v Vector4 a,
>                   LinearTransform v Vector1 a,
>                   Linearizable LinearMap (:*:) v Vector4 a,
>                   Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) ->  v a :-> (Vector4 a)
>vector_laplace4 f = arr_linear $ \x -> Vector4
>   ((laplace $ linear_dual_4t f) `bracket` x)
>   ((laplace $ linear_dual_4x f) `bracket` x)
>   ((laplace $ linear_dual_4y f) `bracket` x)
>   ((laplace $ linear_dual_4z f) `bracket` x)

>linear_dual_4t :: (Dualizable (v a) Dual, Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> Vector4 a -> Dual (v a)
>linear_dual_4t f = covector (tcoord4 . (-!<) f)

>linear_dual_4x :: (Dualizable (v a) Dual, Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) -> Dual (v a)
>linear_dual_4x f = covector (xcoord4 . (-!<) f)

>linear_dual_4y :: (Dualizable (v a) Dual,Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) -> Dual (v a)
>linear_dual_4y f = covector (ycoord4 . (-!<) f)

>linear_dual_4z :: (Dualizable (v a) Dual, Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) -> Dual (v a)
>linear_dual_4z f = covector (zcoord4 . (-!<) f)
