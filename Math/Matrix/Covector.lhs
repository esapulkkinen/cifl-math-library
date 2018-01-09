>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,GADTs,MultiParamTypeClasses,FlexibleContexts, TypeOperators, UndecidableInstances, TypeFamilies, UnicodeSyntax #-}
>-- | <http://theoreticalminimum.com/courses/advanced-quantum-mechanics/2013/fall/lecture-1>
>-- 
>-- <https://en.wikipedia.org/wiki/Eigenvalue_algorithm>
>module Math.Matrix.Covector where
>import Data.Complex
>import Math.Tools.CoFunctor
>import Math.Tools.CoMonad
>import Math.Tools.Arrow
>import Math.Matrix.Interface
>import Math.Matrix.Matrix

>-- | see K. Chandrasekhara Rao: Functional Analysis

>data Dual v = Covector { bracket :: v -> Scalar v }

>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>
>--   <https://en.wikipedia.org/wiki/Laplace_operator>
>--   <https://en.wikipedia.org/wiki/Divergence>
>--   <https://en.wikipedia.org/wiki/Gradient>
>class (VectorSpace v) => VectorDerivative v where
>  divergence :: (v -> v) -> Dual v  -- (Del %. f)(v)
>  grad       :: Dual v -> v -> v    -- (Del f)(v)
>  curl       :: (v -> v) -> v -> v  -- (Del * f)(v)
>  laplace    :: Dual v -> Dual v    -- (Del^2 f)(v)
>  laplace = divergence . grad

>(∇) :: (VectorDerivative v) => Dual v -> v -> v
>(∇) = grad
> 
>(∇·) :: (VectorDerivative v) => (v -> v) -> Dual v
>(∇·) = divergence
> 
>(∇×) :: (VectorDerivative v) => (v -> v) -> v -> v
>(∇×) = curl
> 
>(∇·∇) :: (VectorDerivative v) => Dual v -> Dual v
>(∇·∇) = laplace

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>

>class (VectorSpace v) => FiniteDimensional v where
>   finite :: (Dual :*: Dual) v -> v

>instance (Monoid (Scalar v)) => Monoid (Dual v) where
>   mempty = Covector $ const mempty
>   mappend (Covector f) (Covector g) = Covector $ \v -> mappend (f v) (g v)

>directional_derivative :: (VectorDerivative v, InnerProductSpace v)
>                       => v -> Dual v -> Dual v
>directional_derivative v f = Covector $ \x -> (∇) f x %. v

>dconst :: Scalar v -> Dual v
>dconst = Covector . const

>outer_vector :: (VectorSpace v, Scalar w ~ Scalar v) => Dual w -> v -> w -> v
>outer_vector (Covector y) x w = y w %* x


conjugate_transpose :: (Functor m, Functor n, RealFloat a,
                        Transposable m n)
                    => (m :*: n) (Complex a) -> (n :*: m) (Complex a)

>-- | <http://en.wikipedia.org/wiki/Conjugate_transpose>
>conjugate_transpose :: (Transposable m n, ConjugateSymmetric a)
>                    => (m :*: n) a -> (n :*: m) a
>conjugate_transpose = transpose . fmap conj

>dvector :: (InnerProductSpace v) => v -> Dual v
>dvector x = Covector $ (%. x)

>kronecker :: (Eq i, Functor n, Functor m, Num c) => m i -> n i -> (m :*: n) c
>kronecker = matrix (\ x y -> if x == y then 1 else 0)

>bilinear :: (v -> v -> Scalar v) -> v -> Dual v
>bilinear bra x = Covector $ \y -> x `bra` y

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>

>natural_double_dual :: v -> (Dual :*: Dual) v
>natural_double_dual v = Matrix $ Covector $ \ dv -> dv `bracket` v

>kernel :: (Num (Scalar v), Eq (Scalar v)) => Dual v -> v -> Bool
>kernel (Covector f) x = f x == 0

>-- | would like to make 'adjoint' as instance of CoFunctor,
>-- but the constraint prevents.
>adjoint :: (Scalar a ~ Scalar b) => (a -> b) -> Dual b -> Dual a
>adjoint f (Covector g) = Covector (g . f)

>data VectorSpaceMap v w where
>   VectorSpaceMap :: (Scalar v ~ Scalar w) => (v -> w) -> VectorSpaceMap v w

>instance CoFunctorArrow Dual VectorSpaceMap where
>   inverseImage (VectorSpaceMap f) = VectorSpaceMap (adjoint f)

>scalar_map :: (Scalar a -> Scalar a) -> Dual a -> Dual a
>scalar_map f (Covector g) = Covector (f . g)

>operator_map :: ((v -> Scalar v) -> w -> Scalar w) -> Dual v -> Dual w
>operator_map f (Covector g) = Covector (f g)

>class (Num (Scalar v), VectorSpace v) => NumSpace v where
>class (Fractional (Scalar v), NumSpace v) => FractionalSpace v where

>instance (StandardBasis v, Show (Scalar v)) => Show (Dual v) where
>   show (Covector f) = "dual[" ++ (show $ map f $ unit_vectors) ++ "]"

>instance (VectorSpace v) => VectorSpace (Dual v) where
>   type Scalar (Dual v) = Scalar v
>   vzero = Covector $ const 0
>   vnegate (Covector x) = Covector (negate . x)
>   (Covector f) %+ (Covector g) = Covector $ \a -> f a + g a
>   a %* (Covector f) = Covector $ \b -> a * f b

>instance (StandardBasis v, Num (Scalar v)) => InnerProductSpace (Dual v) where
>   (Covector f) %. (Covector g) = sum [f x * g x | x <- unit_vectors]

>instance (VectorSpace v) => LieAlgebra (Dual v) where
>   f %<>% g = f*g - g*f

>instance (VectorSpace v) => Num (Dual v) where
>  (Covector f) + (Covector g) = Covector $ \a -> f a + g a
>  (Covector f) - (Covector g) = Covector $ \a -> f a - g a
>  (Covector f) * (Covector g) = Covector $ \a -> f a * g a
>  negate (Covector f) = Covector $ negate . f
>  abs (Covector f) = Covector $ abs . f
>  signum (Covector f) = Covector $ signum . f
>  fromInteger i = Covector $ const (fromInteger i)

>instance (FractionalSpace v) => Fractional (Dual v) where
>  (Covector f) / (Covector g) = Covector $ \a -> f a / g a
>  recip (Covector f) = Covector $ recip . f
>  fromRational r = Covector $ const $ fromRational r
> 
>instance (FractionalSpace v, Floating (Scalar v)) => Floating (Dual v) where
>   pi = Covector $ const pi
>   exp (Covector f) = Covector $ exp . f
>   log (Covector f) = Covector $ log . f
>   sqrt (Covector f) = Covector $ sqrt . f
>   sin (Covector f) = Covector $ sin . f
>   cos (Covector f) = Covector $ cos . f
>   tan (Covector f) = Covector $ tan . f
>   asin (Covector f) = Covector $ asin . f
>   acos (Covector f) = Covector $ acos . f
>   atan (Covector f) = Covector $ atan . f
>   sinh (Covector f) = Covector $ sinh . f
>   cosh (Covector f) = Covector $ cosh . f
>   tanh (Covector f) = Covector $ tanh . f
>   asinh (Covector f) = Covector $ asinh . f
>   acosh (Covector f) = Covector $ acosh . f
>   atanh (Covector f) = Covector $ atanh . f


