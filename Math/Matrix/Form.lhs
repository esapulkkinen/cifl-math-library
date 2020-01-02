>{-# LANGUAGE TypeFamilies #-}
>module Math.Matrix.Form where
>import Math.Matrix.Interface
>import Math.Matrix.Covector
>import Math.Tools.Prop

>-- | Notice 'd' is often contravariant.
>class LinearForm d where
>  zero_form :: (VectorSpace v) => d (d v)
>  negate_form :: (VectorSpace v) => d v -> d v
>  plus_form :: (VectorSpace v) => d v -> d (v,v)
>  product_form :: (VectorSpace v) => Scalar v -> d v -> d v

>instance LinearForm Prop where
>   zero_form = Characteristic $ \ (Characteristic f) -> f vzero
>   negate_form (Characteristic f) = Characteristic $ f . vnegate
>   plus_form (Characteristic f) = Characteristic $ \(v,w) -> f (v %+ w)
>   product_form a (Characteristic f) = Characteristic $ \ v -> f (a %* v)

>instance LinearForm Dual where
>   zero_form = Covector $ \ (Covector f) -> f vzero
>   negate_form (Covector f) = Covector $ \w -> f (vnegate w)
>   plus_form (Covector f) = Covector $ \(v,w) -> f (v %+ w)
>   product_form a (Covector f) = Covector $ \v -> f (a %* v)

>data Eigen v = Eigen { eigenvalue :: Scalar v, eigenvector :: v }

>instance (VectorSpace v, Num (Scalar v)) => VectorSpace (Eigen v) where
>   type Scalar (Eigen v) = Scalar v
>   vzero = Eigen 0 vzero
>   vnegate (Eigen x v) = Eigen (negate x) (vnegate v)
>   (Eigen x v) %+ (Eigen x' v') = Eigen (x + x') (v %+ v')
>   k %* (Eigen x v) = Eigen (k*x) (k %* v)
