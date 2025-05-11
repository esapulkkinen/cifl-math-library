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

>diagonalProjections4 :: (Num a, ConjugateSymmetric a) => Vector4 (Dual (Vector4 a))
>diagonalProjections4 = Vector4 (covector tcoord4)
>                                (covector xcoord4)
>                                (covector ycoord4)
>                                (covector zcoord4)

>index4Dual :: (Num a, ConjugateSymmetric a) => Int -> Dual (Vector4 a)
>index4Dual = covector . index4

>instance (Num a, ConjugateSymmetric a) => ProjectionDual Vector4 Dual a where
>   projectionDual = cov4

>partialDerivate4x :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partialDerivate4x = partialDerivate dx_4

>partialDerivate4y :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partialDerivate4y = partialDerivate dy_4

>partialDerivate4z :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partialDerivate4z = partialDerivate dz_4

>partialDerivate4t :: (Closed a) => (Vector4 a -> a) -> Vector4 a -> a
>partialDerivate4t = partialDerivate dt_4

>derivate4tSquared :: (Closed a, ConjugateSymmetric a)
>     => Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4tSquared = operator_map (partialDerivate4t . partialDerivate4t)

>derivate4t :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4t = operator_map partialDerivate4t

>derivate4x :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4x = operator_map (partialDerivate4x)

>derivate4y :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4y = operator_map (partialDerivate4y)
> 
>derivate4z :: (Closed a, ConjugateSymmetric a) =>
>  Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>derivate4z = operator_map (partialDerivate4z)

>delPartial4 :: (DifferentiallyClosed a) => (Vector4 a -> a) -> Vector4 a -> Vector4 a
>delPartial4 f (Vector4 x y z t) = Vector4 (partial1_4 ff x y z t)
>                                           (partial2_4 ff x y z t)
>                                           (partial3_4 ff x y z t)
>                                           (partial4_4 ff x y z t)
>   where ff a b c d = f (Vector4 a b c d)

>instance DifferentialOperator Vector4 where
>   partial = delPartial4


>del4 :: (Closed a, ConjugateSymmetric a)
>      => Vector4 (Dual (Vector4 a) -> Dual (Vector4 a))
>del4 = Vector4 derivate4t derivate4x derivate4y derivate4z

>del4_ :: (Closed a) => Vector4 ((Vector4 a -> a) -> Vector4 a -> a)
>del4_ = Vector4 partialDerivate4t partialDerivate4x partialDerivate4y
>               partialDerivate4z

>hessian4 :: (Closed v, ConjugateSymmetric v)
>  => Dual (Vector4 v) -> (Vector4 :*: Vector4) (Dual (Vector4 v))
>hessian4 f = matrix (\a b -> a (b f)) del4 del4

>grad4 :: (Closed a, ConjugateSymmetric a) => Covector.Dual (Vector4 a) -> LinearMap (Vector4 a) (Vector4 a)
>grad4 f = let f' = bracket f in arrLinear $ \x -> Vector4
>    (partialDerivate4t f' x)
>    (partialDerivate4x f' x)
>    (partialDerivate4y f' x)
>    (partialDerivate4z f' x)

>curl4 :: (Closed a, ConjugateSymmetric a, LinearTransform Vector4 Vector1 a) => Vector4 (Covector.Dual (Vector4 a))
>                                    -> Vector4 a
>                                    -> (Vector4 :*: Vector4) a
>curl4 (Vector4 ft' fx' fy' fz')
>    v@(Vector4 t x y z) = Matrix $ Vector4
>               (Vector4 0
>                        (partialDerivate4x ft v - partialDerivate4t fx v)
>                        (partialDerivate4y ft v - partialDerivate4t fy v)
>                        (partialDerivate4z ft v - partialDerivate4t fz v))
> 
>               (Vector4 (partialDerivate4t fx v - partialDerivate4x ft v)
>                        0
>                        (partialDerivate4y fx v - partialDerivate4x fy v)
>                        (partialDerivate4z fx v - partialDerivate4x fz v))
>               (Vector4 (partialDerivate4t fy v - partialDerivate4y ft v)
>                        (partialDerivate4x fy v - partialDerivate4y fx v)
>                        0
>                        (partialDerivate4z fy v - partialDerivate4y fz v))
>               (Vector4 (partialDerivate4t fz v - partialDerivate4z ft v)
>                        (partialDerivate4x fz v - partialDerivate4z fx v)
>                        (partialDerivate4y fz v - partialDerivate4z fy v)
>                        0)
>       where ft = bracket ft'
>             fx = bracket fx'
>             fy = bracket fy'
>             fz = bracket fz'

>instance (Num a, Closed a, ConjugateSymmetric a, LinearTransform Vector4 Vector1 a) => VectorDerivative (Vector4 a) Dual LinearMap where
>   divergence = divergence4
>   grad = grad4
>   directionalDerivative = directionalDerivativeImpl

>divergence4 :: (Closed a, ConjugateSymmetric a) => (Vector4 a) :-> (Vector4 a) -> Covector.Dual (Vector4 a)
>divergence4 f = covector $ \z -> partialDerivate4t (tcoord4 . (-!<) f) z +
>                                partialDerivate4x (xcoord4 . (-!<) f) z +
>                                partialDerivate4y (ycoord4 . (-!<) f) z +
>                                partialDerivate4z (zcoord4 . (-!<) f) z

>laplace4 :: (Closed a, ConjugateSymmetric a) => Covector.Dual (Vector4 a) -> Covector.Dual (Vector4 a)
>laplace4 f = divergence4 (grad4 f)

>instance (Closed a, Ord a, ConjugateSymmetric a, LinearTransform Vector4 Vector1 a)
> => VectorLaplacian (Vector4 a) LinearMap where
>  vectorLaplace = vectorLaplace4

>vectorLaplace4 :: (VectorDerivative (v a) Dual LinearMap,
>                   Dualizable (v a) Dual,
>                   Diagonalizable v a, 
>                   LinearTransform v Vector4 a,
>                   LinearTransform v Vector1 a,
>                   Linearizable LinearMap (:*:) v Vector4 a,
>                   Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) ->  v a :-> (Vector4 a)
>vectorLaplace4 f = arrLinear $ \x -> Vector4
>   ((laplace $ linearDual_4t f) `bracket` x)
>   ((laplace $ linearDual_4x f) `bracket` x)
>   ((laplace $ linearDual_4y f) `bracket` x)
>   ((laplace $ linearDual_4z f) `bracket` x)

>linearDual_4t :: (Dualizable (v a) Dual, Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> Vector4 a -> Dual (v a)
>linearDual_4t f = covector (tcoord4 . (-!<) f)

>linearDual_4x :: (Dualizable (v a) Dual, Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) -> Dual (v a)
>linearDual_4x f = covector (xcoord4 . (-!<) f)

>linearDual_4y :: (Dualizable (v a) Dual,Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) -> Dual (v a)
>linearDual_4y f = covector (ycoord4 . (-!<) f)

>linearDual_4z :: (Dualizable (v a) Dual, Diagonalizable v a, LinearTransform v Vector4 a, Linearizable LinearMap (:*:) v Vector1 a)
> => v a :-> (Vector4 a) -> Dual (v a)
>linearDual_4z f = covector (zcoord4 . (-!<) f)
