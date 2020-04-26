>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,GADTs,MultiParamTypeClasses,FlexibleContexts, TypeOperators, UndecidableInstances, TypeFamilies, UnicodeSyntax, DeriveGeneric, DeriveDataTypeable, RankNTypes, FlexibleInstances, DefaultSignatures #-}
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
>import safe Data.Complex
>import safe Data.Monoid hiding (Dual)
>import safe Prelude hiding (id,(.))
>import safe Data.Typeable
>import safe GHC.Generics hiding ((:*:))
>import safe Control.Category
>import safe Math.Tools.CoFunctor
>import safe Math.Tools.CoMonad
>import safe Math.Tools.Arrow
>import safe Math.Matrix.Interface
>import safe Math.Matrix.Matrix
>import safe Math.Tools.I

>-- | Data type for dual vectors.
>data Dual v = Covector { bracketMap :: v -> (Scalar v) }
>  deriving (Typeable, Generic)

>contract :: (Dual :*: v) a -> (I :*: v) a ->  Scalar (v a)
>contract (Matrix (Covector f)) (Matrix (I w)) = f w

>bracket :: Dual v -> v -> Scalar v
>bracket (Covector f) = f

>covector :: (v -> Scalar v) -> Dual v
>covector = Covector 

>(*><) :: Dual v -> v -> Scalar v
>(*><) = bracket

>-- | <https://en.wikipedia.org/wiki/Laplace_operator>
>--   <https://en.wikipedia.org/wiki/Divergence>
>--   <https://en.wikipedia.org/wiki/Gradient>
>class (VectorSpace v) => VectorDerivative v where
>  divergence :: LinearMap v v -> Dual v  -- (Del %. f)(v)
>  grad       :: Dual v -> LinearMap v v    -- (Del f)(v)
>  laplace    :: Dual v -> Dual v    -- (Del^2 f)(v)
>  laplace = divergence . grad
>  {-# MINIMAL divergence, grad #-}

>-- | <https://en.wikipedia.org/wiki/Curl_(mathematics)>
>class (VectorDerivative v) => VectorCrossProduct v where
>  curl       :: LinearMap v v -> LinearMap v v  -- (Del * f)(v)
>
>-- | <https://en.wikipedia.org/wiki/Vector_Laplacian>
>class VectorLaplacian v where
>  vector_laplace :: LinearMap v v -> LinearMap v v -- (Del^2 A)(v)
>  default vector_laplace :: (VectorCrossProduct v) => LinearMap v v -> LinearMap v v
>  vector_laplace a = LinearMap $ \ x ->
>     grad (divergence a) -!< x
>     %- curl (curl a) -!< x


>class (Functor f) => ProjectionDual f a where
>   projection_dual :: f (Dual (f a))

projection_dual_matrix :: (Scalar ((f :*: g) a) ~ Scalar (f (Scalar (g a))),
 Scalar a ~ Scalar (Scalar a),
 ProjectionDual f (Scalar (g a)), ProjectionDual g a)
  => (f :*: g) (Dual ((f :*: g) a))
projection_dual_matrix = matrix (\ (Covector x) (Covector y) -> Covector (LinearMap $ \m -> m <!> ((-!<) x,(-!<) y))) projection_dual projection_dual

instance (Scalar ((f :*: g) a) ~ Scalar (f (Scalar (g a))),
   ProjectionDual f (Scalar (g a)), ProjectionDual g a)
 => ProjectionDual (f :*: g) a where
   projection_dual = projection_dual_matrix

>-- | unicode support for gradient. Use with parenthesis.
>(∇) :: (VectorDerivative v) => Dual v -> LinearMap v v
>(∇) = grad
> 
>-- | unicode support for divergence. Use with parenthesis.
>(∇·) :: (VectorDerivative v) => LinearMap v v -> Dual v
>(∇·) = divergence

>-- | unicode support for directional derivative. Binary operator.
>(·∇) :: (VectorDerivative v, InnerProductSpace v) => v -> Dual v -> Dual v
>(·∇) = directional_derivative
> 
>-- | unicode support for curl. Use with parenthesis.
>(∇×) :: (VectorCrossProduct v) => LinearMap v v -> LinearMap v v
>(∇×) = curl
> 
>-- | unicode support for laplace. Use with parenthesis.
>(∇·∇) :: (VectorDerivative v) => Dual v -> Dual v
>(∇·∇) = laplace

>differential_commutator :: (VectorDerivative v, Scalar v ~ v)
>                        => Dual v -> Dual v -> LinearMap v v
>differential_commutator g f = LinearMap $ \x -> (∇) g -!< (f `bracket` x)
>                              %- g `bracket` ((∇) f -!< x)

>differential_commutator_endo :: (VectorDerivative v, Scalar v ~ v)
>                             => Dual v -> Dual v -> Endo v
>differential_commutator_endo g f = Endo $ (-!<) $ differential_commutator g f


>curl_endo :: (VectorCrossProduct v) => Endo v -> Endo v
>curl_endo (Endo f) = Endo ((-!<) (curl $ LinearMap f))

>grad_endo :: (VectorDerivative v) => Dual v -> Endo v
>grad_endo f = Endo ((-!<) (grad f))
>
>divergence_endo :: (VectorDerivative v) => Endo v -> Dual v
>divergence_endo (Endo f) = divergence (LinearMap f)

>laplace_endo :: (VectorDerivative v) => Endo (Dual v)
>laplace_endo = Endo laplace

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>
>class (VectorSpace v) => FiniteDimensional v where
>   finite :: (Dual :*: Dual) v -> v

>instance (Semigroup (Scalar v)) => Semigroup (Dual v) where
>   (Covector f) <> (Covector g) = covector $ \v -> f v <> g v

>instance (Monoid (Scalar v)) => Monoid (Dual v) where
>   mempty = Covector $ \_ -> mempty
>   mappend (Covector f) (Covector g)
>     = covector $ \v -> mappend (f v) (g v)

>directional_derivative :: (VectorDerivative v, InnerProductSpace v)
>                       => v -> Dual v -> Dual v
>directional_derivative v f = covector $ \x -> v %. (∇) f -!< x

>directional_derivative_endo :: (VectorDerivative v, InnerProductSpace v)
>  => v -> Endo (Dual v)
>directional_derivative_endo v = Endo $ directional_derivative v

>dconst :: Scalar v -> Dual v
>dconst x = covector (\a -> x)

>outer_vector :: (VectorSpace v, Scalar w ~ Scalar v) => Dual w -> v -> w -> v
>outer_vector (Covector y) x w = y w %* x

>-- | <http://en.wikipedia.org/wiki/Conjugate_transpose>
>conjugate_transpose :: (Transposable m n, ConjugateSymmetric a)
>                    => (m :*: n) a -> (n :*: m) a
>conjugate_transpose = transpose . fmap conj

>-- | unicode alias (unicode HERMITIAN CONJUGATE MATRIX character)
>(⊹) :: (Transposable m n, ConjugateSymmetric a)
>                    => (m :*: n) a -> (n :*: m) a
>(⊹) = conjugate_transpose

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_hermitian :: (Eq a, ConjugateSymmetric a, Transposable m m, Applicative m, Foldable m)
>  => (m :*: m) a -> Bool
>is_hermitian a = a == conjugate_transpose a
>
>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_skew_hermitian :: (Num a, Eq a, ConjugateSymmetric a, Applicative m, Transposable m m, Foldable m) => (m :*: m) a -> Bool
>is_skew_hermitian a = a == (fmap negate $! (conjugate_transpose $! a))
>
>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_normal :: (Num a, Eq (Scalar (m a)), ConjugateSymmetric a, Foldable m, Applicative m, Transposable m m, InnerProductSpace (m a)) => (m :*: m) a -> Bool
>is_normal a = conjugate_transpose a %*% a == a %*% conjugate_transpose a

>-- | partial application of dot product on second argument @(- %. v)@.
>bravector :: (InnerProductSpace v) => v -> Dual v
>bravector x = covector $ \u -> u %. x

>-- | partial application of dot product on first argument: @(v %. -)@.
>ketvector :: (InnerProductSpace v) => v -> Dual v
>ketvector x = covector $ \u -> x %. u

>kronecker :: (Eq i, Functor n, Functor m, Num c) => m i -> n i -> (m :*: n) c
>kronecker = matrix (\ x y -> if x == y then 1 else 0)

>dual_tensor :: 
>   (Scalar a -> Scalar b -> Scalar (a,b))
>  -> Dual a -> Dual b -> Dual (a,b)
>dual_tensor h (Covector f) (Covector g) =
>   Covector $ \(a,b) -> h (f a) (g b) 

>bilinear :: (v -> v -> Scalar v) -> v -> Dual v
>bilinear bra x = covector $ \y -> x `bra` y

>kronecker_dual :: (Eq a, Num (Scalar a)) => a -> Dual a
>kronecker_dual = bilinear (\x y -> if x == y then 1 else 0)

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>

>natural_double_dual :: v -> (Dual :*: Dual) v
>natural_double_dual v = Matrix $ Covector $ \ dv -> dv `bracket` v

>covector_kernel :: (Num (Scalar v), Eq (Scalar v)) => Dual v -> v -> Bool
>covector_kernel (Covector f) x = f x == 0

>-- | would like to make 'adjoint' as instance of CoFunctor,
>-- but the constraint prevents. Instead we declare LinearMap.
>adjoint :: LinearMap a b -> LinearMap (Dual b) (Dual a)
>adjoint (LinearMap f) = LinearMap $ \ (Covector g) -> Covector $ g . f

>adjoint_map :: (Scalar b ~ Scalar a) => (Scalar b -> Scalar a) -> LinearMap a b -> Dual b -> Dual a
>adjoint_map sca f (Covector x) = Covector (sca . x . (-!<) f)

>matrix_arrow :: (LinearTransform f g a) => (f :*: g) a -> LinearMap (f a) (g a)
>matrix_arrow m = LinearMap $ \v -> m <<*> v

>instance CoFunctorArrow Dual LinearMap where
>   inverseImage = adjoint

>scalar_map :: LinearMap (Scalar a) (Scalar a) -> Dual a -> Dual a
>scalar_map (LinearMap f) (Covector g) = Covector (f . g)

>operator_map :: ((v -> Scalar v) -> w -> Scalar w) -> Dual v -> Dual w
>operator_map f (Covector g) = Covector (f g)

>-- | vector space with scalars in Num class
>class (Num (Scalar v), VectorSpace v) => NumSpace v 

>-- | vector space with fractional scalars
>class (Fractional (Scalar v), NumSpace v) => FractionalSpace v 

>instance (StandardBasis v, Show (Scalar v)) => Show (Dual v) where
>   show (Covector f) = "dual" ++ (show $ map f $ unit_vectors)

>instance (VectorSpace v) => VectorSpace (Dual v) where
>   type Scalar (Dual v) = Scalar v
>   vzero = Covector $ \_ -> 0
>   vnegate (Covector x) = Covector (negate . x)
>   (Covector f) %+ (Covector g) = Covector (\x -> f x + g x)
>   a %* (Covector f) = Covector $ \b -> a * f b

>(-!<) :: (Scalar w ~ Scalar v) => LinearMap v w -> v -> w
>(-!<) (LinearMap f) = f

>data LinearMap v w where
>   LinearMap :: (Scalar w ~ Scalar v) => (v -> w) -> LinearMap v w

>instance (Semigroup v) => Semigroup (LinearMap v v) where
>  (LinearMap f) <> (LinearMap g) = LinearMap (f <> g)

>instance (Semigroup v) => Monoid (LinearMap v v) where
>  mempty = LinearMap id
>  mappend (LinearMap f) (LinearMap g) = LinearMap (f . g)

>instance Category LinearMap where
>   id = LinearMap id
>   (LinearMap f) . (LinearMap g) = LinearMap (f . g)


>instance (StandardBasis v, Num (Scalar v)) => InnerProductSpace (Dual v) where
>   (Covector f) %. (Covector g) =
>     sum [f x * g x | x <- unit_vectors]

>instance (VectorSpace v) => LieAlgebra (Dual v) where
>  f %<>% g = f*g - g*f

>instance (VectorSpace v) => Num (Dual v) where
>  (Covector f) + (Covector g) =
>    Covector $ \a -> f a + g a
>  (Covector f) - (Covector g) =
>    Covector $ \a -> f a - g a
>  (Covector f) * (Covector g) =
>    Covector $ \a -> f a * g a
>  negate (Covector f) = Covector $ negate . f
>  abs (Covector f) = Covector $ abs . f
>  signum (Covector f) = Covector $ signum . f
>  fromInteger i = Covector $ \_ -> fromInteger i

>instance (FractionalSpace v) => Fractional (Dual v) where
>  (Covector f) / (Covector g) =
>     Covector $ \a -> f a / g a
>  recip = scalar_map (LinearMap recip)
>  fromRational r = Covector $ \_ -> fromRational r
> 
>instance (FractionalSpace v, Floating (Scalar v)) => Floating (Dual v) where
>   pi = Covector $ const pi
>   exp = scalar_map (LinearMap exp)
>   log = scalar_map (LinearMap log)
>   sqrt = scalar_map (LinearMap sqrt)
>   sin = scalar_map (LinearMap sin)
>   cos = scalar_map (LinearMap cos)
>   tan = scalar_map (LinearMap tan)
>   asin = scalar_map (LinearMap asin)
>   acos = scalar_map (LinearMap acos)
>   atan = scalar_map (LinearMap atan)
>   sinh = scalar_map (LinearMap sinh)
>   cosh = scalar_map (LinearMap cosh)
>   tanh = scalar_map (LinearMap tanh)
>   asinh = scalar_map (LinearMap asinh)
>   acosh = scalar_map (LinearMap acosh)
>   atanh = scalar_map (LinearMap atanh)


>-- | This computes
>-- \(DDM(\otimes,{\bar{x}},{\bar{y}}) = [({\bar{x}} \cdot \nabla) \times ({\bar{y}} \cdot \nabla)]\)
>-- where \(\times\) is the outer product for linear operators derived
>-- from product of dual spaces \(\otimes\). Notice \({\bar{x}} \cdot \nabla\)
>-- is normally called the directional derivative, so this is really a
>-- two dimensional version of directional derivative with some possibility
>-- to choose how two dual vectors are combined.
>dual_derivative ::
> (VectorDerivative v, VectorDerivative w,
> InnerProductSpace v, InnerProductSpace w)
> => (Dual v -> Dual w  -> c) -> v -> w -> (Dual v -> Dual w -> c)
>dual_derivative f x y = cells $ matrix f (x ·∇) (y ·∇)

>dual_differential_outer_product :: (VectorDerivative v, InnerProductSpace v)
> => v -> v -> Dual v -> Dual v  -> Dual v
>dual_differential_outer_product = dual_derivative (*)

>dual_differential_dot_product :: (VectorDerivative v, InnerProductSpace v, StandardBasis v)
>  => v -> v -> Dual v -> Dual v -> Scalar v
>dual_differential_dot_product = dual_derivative (%.)

>norm_covector :: (NormedSpace v) => Dual v
>norm_covector = Covector $ norm
