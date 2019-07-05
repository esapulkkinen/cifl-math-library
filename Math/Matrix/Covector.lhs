>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,GADTs,MultiParamTypeClasses,FlexibleContexts, TypeOperators, UndecidableInstances, TypeFamilies, UnicodeSyntax, DeriveGeneric, DeriveDataTypeable #-}
>-- |
>-- Module: Math.Matrix.Covector
>-- Description: Covectors
>-- Copyright: (c) Esa Pulkkinen, 2018
>-- Maintainer: esa.pulkkinen@iki.fi
>-- License: LGPL
>--
>-- For background material, see K. Chandrasekhara Rao: Functional Analysis.
>-- 
>-- See also: <http://theoreticalminimum.com/courses/advanced-quantum-mechanics/2013/fall/lecture-1>
>module Math.Matrix.Covector where
>import Data.Complex
>import Data.Monoid hiding (Dual)
>import Prelude hiding (id,(.))
>import Data.Typeable
>import GHC.Generics hiding ((:*:))
>import Control.Category
>import Math.Tools.CoFunctor
>import Math.Tools.CoMonad
>import Math.Tools.Arrow
>import Math.Matrix.Interface
>import Math.Matrix.Matrix

>-- | Data type for dual vectors.
>data Dual v = Covector { bracket :: v -> Scalar v }
>  deriving (Typeable, Generic)

>(*><) :: Dual v -> v -> Scalar v
>(*><) = bracket

>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>
>--   <https://en.wikipedia.org/wiki/Laplace_operator>
>--   <https://en.wikipedia.org/wiki/Divergence>
>--   <https://en.wikipedia.org/wiki/Gradient>
>class (VectorSpace v) => VectorDerivative v where
>  divergence :: (v -> v) -> Dual v  -- (Del %. f)(v)
>  grad       :: Dual v -> v -> v    -- (Del f)(v)
>  laplace    :: Dual v -> Dual v    -- (Del^2 f)(v)
>  laplace = divergence . grad

>class (VectorDerivative v) => VectorCrossProduct v where
>  curl       :: (v -> v) -> v -> v  -- (Del * f)(v)

>-- | unicode support for gradient. Use with parenthesis.
>(∇) :: (VectorDerivative v) => Dual v -> v -> v
>(∇) = grad
> 
>-- | unicode support for divergence. Use with parenthesis.
>(∇·) :: (VectorDerivative v) => (v -> v) -> Dual v
>(∇·) = divergence
> 
>-- | unicode support for curl. Use with parenthesis.
>(∇×) :: (VectorCrossProduct v) => (v -> v) -> v -> v
>(∇×) = curl
> 
>-- | unicode support for laplace. Use with parenthesis.
>(∇·∇) :: (VectorDerivative v) => Dual v -> Dual v
>(∇·∇) = laplace

>differential_commutator :: (VectorDerivative v, Scalar v ~ v)
>                        => Dual v -> Dual v -> v -> v
>differential_commutator g f x = (∇) g (f `bracket` x)
>                              %- g `bracket` ((∇) f x)

>differential_commutator_endo :: (VectorDerivative v, Scalar v ~ v)
>                             => Dual v -> Dual v -> Endo v
>differential_commutator_endo g f = Endo $ differential_commutator g f


>curl_endo :: (VectorCrossProduct v) => Endo v -> Endo v
>curl_endo (Endo f) = Endo (curl f)

>grad_endo :: (VectorDerivative v) => Dual v -> Endo v
>grad_endo f = Endo (grad f)
>
>divergence_endo :: (VectorDerivative v) => Endo v -> Dual v
>divergence_endo (Endo f) = divergence f

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>
>class (VectorSpace v) => FiniteDimensional v where
>   finite :: (Dual :*: Dual) v -> v

>instance (Semigroup (Scalar v)) => Semigroup (Dual v) where
>   (Covector f) <> (Covector g) = Covector $ \v -> f v <> g v

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

>covector_kernel :: (Num (Scalar v), Eq (Scalar v)) => Dual v -> v -> Bool
>covector_kernel (Covector f) x = f x == 0

>-- | would like to make 'adjoint' as instance of CoFunctor,
>-- but the constraint prevents. Instead we declare VectorSpaceMap.
>adjoint :: (Scalar a ~ Scalar b) => (a -> b) -> Dual b -> Dual a
>adjoint f (Covector g) = Covector (g . f)

>data VectorSpaceMap v w where
>   VectorSpaceMap :: (Scalar v ~ Scalar w) => (v -> w) -> VectorSpaceMap v w

>instance Category VectorSpaceMap where
>   id = VectorSpaceMap id
>   (VectorSpaceMap f) . (VectorSpaceMap g) = VectorSpaceMap (f . g)

>instance CoFunctorArrow Dual VectorSpaceMap where
>   inverseImage (VectorSpaceMap f) = VectorSpaceMap (adjoint f)

>scalar_map :: (Scalar a -> Scalar a) -> Dual a -> Dual a
>scalar_map f (Covector g) = Covector (f . g)

>operator_map :: ((v -> Scalar v) -> w -> Scalar w) -> Dual v -> Dual w
>operator_map f (Covector g) = Covector (f g)

>-- | vector space with scalars in Num class
>class (Num (Scalar v), VectorSpace v) => NumSpace v 

>-- | vector space with fractional scalars
>class (Fractional (Scalar v), NumSpace v) => FractionalSpace v 

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
>  f %<>% g = f*g - g*f

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
>  recip = scalar_map recip
>  fromRational r = Covector $ const $ fromRational r
> 
>instance (FractionalSpace v, Floating (Scalar v)) => Floating (Dual v) where
>   pi = Covector $ const pi
>   exp = scalar_map exp
>   log = scalar_map log
>   sqrt = scalar_map sqrt
>   sin = scalar_map sin
>   cos = scalar_map cos
>   tan = scalar_map tan
>   asin = scalar_map asin
>   acos = scalar_map acos
>   atan = scalar_map atan
>   sinh = scalar_map sinh
>   cosh = scalar_map cosh
>   tanh = scalar_map tanh
>   asinh = scalar_map asinh
>   acosh = scalar_map acosh
>   atanh = scalar_map atanh


