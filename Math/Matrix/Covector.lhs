>-- -*- coding: utf-8 -*-
>{-# LANGUAGE Safe,GADTs,MultiParamTypeClasses,FlexibleContexts, TypeOperators, UndecidableInstances, TypeFamilies, UnicodeSyntax, DeriveGeneric, DeriveDataTypeable, RankNTypes, FlexibleInstances, DefaultSignatures #-}
>{-# LANGUAGE StandaloneDeriving, ConstraintKinds #-}
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
>import safe Math.Tools.PrettyP
>import safe Math.Matrix.Interface
>import safe Math.Tools.I
>import safe Data.Type.Equality
>import safe Math.Matrix.Linear
>import safe Math.Matrix.Vector2
>import safe Math.Number.StreamInterface

linear_outer_product :: (InnerProductSpace p, VectorSpace v, Scalar v ~ calar p)
   => p -> v -> LinearMap p v
linear_outer_product a b = LinearMap Refl $
   Math.Matrix.Interface.outer a b

>instance (PpShowVerticalF f, LinearTransform f g a, Linearizable LinearMap (:*:) f g a, PpShowF g, PpShow a, Diagonalizable f a) => PpShow (LinearMap (f a) (g a)) where
>  pp x = pp $ fromLinear x

>instance (Diagonalizable f a, Linearizable LinearMap (:*:) f g a, LinearTransform f g a, Show ((f :*: g) a))
> => Show (LinearMap (f a) (g a)) where
>  showsPrec i x = showsPrec i (fromLinear x)

>contract :: (Scalar (v a) ~ a, LinearTransform v Vector1 a)
>  => (Dual :*: v) a -> (Vector1 :*: v) a -> Scalar (v a)
>contract (Matrix (Covector f)) (Matrix (Vector1 w)) = vector_element (f -!< w)

>dual_apply :: (LinearTransform v Vector1 a)
> => Dual (v a) -> v a -> a
>dual_apply (Covector v) x = vector_element (v -!< x)

>(*><) :: Dual (v a) -> v a :-> Vector1 a
>(*><) = bracketMap

>x2_op :: (Num s, ConjugateSymmetric s) => Dual (Vector2 s)
>x2_op = covector xcoord2

>y2_op :: (Num s, ConjugateSymmetric s) => Dual (Vector2 s)
>y2_op = covector ycoord2

>-- | unicode support for gradient. Use with parenthesis.
>(∇) :: (VectorDerivative v d arr) => d v -> arr v v
>(∇) = grad
> 
>-- | unicode support for divergence. Use with parenthesis.
>(∇·) :: (VectorDerivative v d arr) => arr v v -> d v
>(∇·) = divergence

>-- | unicode support for directional derivative. Binary operator.
>(·∇) :: (VectorDerivative v d arr, InnerProductSpace v) => v -> d v -> d v
>(·∇) = directional_derivative
> 
>-- | unicode support for curl. Use with parenthesis.
>(∇×) :: (VectorCrossProduct v arr) => arr v v -> arr v v
>(∇×) = curl
> 
>-- | unicode support for laplace. Use with parenthesis.
>(∇·∇) :: (VectorDerivative v d arr) => d v -> d v
>(∇·∇) = laplace

differential_commutator :: (VectorDerivative v, v ~ Scalar v)
                        => Dual v -> Dual v -> v :-> I (Scalar v)
differential_commutator g f = bracket f . (∇) g
                              %- ((∇) f) . bracket g

differential_commutator_endo :: (VectorDerivative v, Scalar v ~ v)
                             => Dual v -> Dual v -> Endo v
differential_commutator_endo g f = Endo $ (-!!<) $ differential_commutator g f


curl_endo :: (VectorCrossProduct v) => Endo v -> Endo v
curl_endo (Endo f) = Endo ((-!<) (curl $ LinearMap Refl f))

grad_endo :: (VectorDerivative v) => Dual v -> Endo v
grad_endo f = Endo ((-!<) (grad f))

divergence_endo :: (VectorDerivative v) => Endo v -> Dual v
divergence_endo (Endo f) = divergence (LinearMap Refl f)

laplace_endo :: (VectorDerivative v) => Endo (Dual v)
laplace_endo = Endo laplace


instance (Semigroup (I (Scalar v))) => Semigroup (Dual v) where
   (Covector f) <> (Covector g) = Covector $ matrixLin (<>) f g -!<
      where res x = f -!< x <> g -!< x

instance (Monoid (Scalar v)) => Monoid (Dual v) where
   mempty = Covector $ LinearMap Refl $ \_ -> mempty
   mappend (Covector (LinearMap p f)) (Covector (LinearMap q g))
     = Covector $ LinearMap p $ \v -> mappend (f v) (g v)

directional_derivative :: (VectorDerivative v, InnerProductSpace v)
                       => v -> Dual v -> Dual v


 -- Covector $ \x = v %. (∇) f -!< x

 matrixLin (%.) v ((∇) f)

 \x -> v %. ((∇) f -!< x)

>directional_derivative_endo :: (VectorDerivative v Dual LinearMap, InnerProductSpace v)
>  => v -> Endo (Dual v)
>directional_derivative_endo v = Endo $ directional_derivative v

>dconst :: (Linearizable LinearMap (:*:) f Vector1 v, Diagonalizable f v) => v -> Dual (f v)
>dconst x = covector $ \a -> x

>outer_vector :: (Linearizable LinearMap (:*:) f g w, DualNum f w, DualNum g w) => Dual (f w) -> g w -> f w :-> g w
>outer_vector (Covector y) x = arr_linear $ \w -> (y -!!< w) %* x

>-- | <http://en.wikipedia.org/wiki/Conjugate_transpose>
>conjugate_transpose :: (Diagonalizable m a, Linearizable LinearMap (:*:) m m a, Linearizable LinearMap (:*:) m n a, Linearizable LinearMap (:*:) n m a, Diagonalizable n a, LinearIso n m a, Transposable m n a, ConjugateSymmetric (m a))
>                    => m a :-> n a -> n a :-> m a
>conjugate_transpose f = arr_linear conj . transpose f

>-- | unicode alias (unicode HERMITIAN CONJUGATE MATRIX character)
>(⊹) :: (Linearizable LinearMap (:*:) m m a, Linearizable LinearMap (:*:) m n a, Linearizable LinearMap (:*:) n m a, Diagonalizable m a, Diagonalizable n a, LinearIso n m a, Transposable m n a, ConjugateSymmetric (m a))
>                    => m a :-> n a -> n a :-> m a
>(⊹) = conjugate_transpose

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_hermitian :: (Eq (m (m a)), ConjugateSymmetric (m a), Linearizable LinearMap (:*:) m m a,
> Transposable m m a, Diagonalizable m a, LinearTransform m m a,
> Applicative m,
> Foldable m)
>  => m a :-> m a -> Bool
>is_hermitian a = a == conjugate_transpose a
>
>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_skew_hermitian :: (Num (Scalar (m a)), Eq (m (m a)),
> ConjugateSymmetric (m a), Applicative m, Linearizable LinearMap (:*:) m m a,
> VectorSpace (m a), Diagonalizable m a, LinearTransform m m a,
> Transposable m m a, Foldable m) => m a :-> m a -> Bool
>is_skew_hermitian a = a == (negate 1) %* (conjugate_transpose a)
>
>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_normal :: (Num a, Eq (m (m a)), SupportsMatrixMultiplication m m m a, Eq (Scalar (m a)), ConjugateSymmetric (m a), Foldable m, Applicative m, Transposable m m a, Linearizable LinearMap (:*:) m m a, InnerProductSpace (m a)) => m a :-> m a -> Bool
>is_normal a = (conjugate_transpose a) . a == a . (conjugate_transpose a)

>-- | partial application of dot product on second argument @(- %. v)@.
>bravector :: (DualNum f v, InnerProductSpace (f v)) => f v -> Dual (f v)
>bravector x = covector $ \u -> u %. x

>-- | partial application of dot product on first argument: @(v %. -)@.
>ketvector :: (DualNum f v, InnerProductSpace (f v)) => f v -> Dual (f v)
>ketvector x = covector $ \u -> x %. u

>kronecker :: (Eq i, Functor n, Functor m, Num c) => m i -> n i -> (m :*: n) c
>kronecker = matrix (\ x y -> if x == y then 1 else 0)

dual_tensor :: 
   (a -> b -> (a,b))
  -> Dual (f a) -> Dual (f b) -> Dual (f a, f b)
dual_tensor h (Covector f) (Covector g) =
   covector $ \(a,b) -> h (f -!!< a) (g -!!< b) 

>dual_bilinear :: (DualNum f v) => (f v -> f v -> v) -> f v -> Dual (f v)
>dual_bilinear bra x = covector $ \y -> x `bra` y

>kronecker_dual :: (DualNum f a, Eq (f a), Num a) => f a -> Dual (f a)
>kronecker_dual = dual_bilinear (\x y -> if x == y then 1 else 0)

>-- | <https://en.wikipedia.org/wiki/Dual_space#Injection_into_the_double-dual>

natural_double_dual :: f a -> (Dual :*: Dual) (f a)
natural_double_dual v = Matrix $ covector $ \ dv -> dv `bracket` v

>covector_kernel :: (DualNum f v, Num v, Eq v) => Dual (f v) -> f v -> Bool
>covector_kernel (Covector f) x = (f -!!< x) == 0

>-- | would like to make 'adjoint' as instance of CoFunctor,
>-- but the constraint prevents. Instead we declare LinearMap.

adjoint :: LinearMap a b -> LinearMap (Dual b) (Dual a)
adjoint z@(LinearMap p f) = LinearMap (sym p) $
   \ (Covector g) -> Covector $ LinearMap p (castWith (apply Refl p)) . g . z

adjoint_map :: (Scalar b ~ Scalar a) => (Scalar b -> Scalar a) -> a :-> b -> Dual b -> Dual a
adjoint_map sca f (Covector x) = covector ((Vector1 . sca . vector_element) . x . appLinear f)

>adjoint :: f a :-> f a -> Dual (f a) -> Dual (f a)
>adjoint f (Covector d) = Covector (d . f)

instance CoFunctorArrow Dual LinearMap where
   inverseImage = adjoint

scalar_map :: Vector1 (Scalar a) :-> Vector1 (Scalar a) -> Dual a -> Dual a

>scalar_map :: Vector1 a :-> Vector1 a -> Dual (f a) -> Dual (f a)
>scalar_map f (Covector g) = Covector (f . g)

>operator_map :: (Diagonalizable w a, LinearTransform v Vector1 a, Linearizable LinearMap (:*:) w Vector1 a)
>  => ((v a -> a) -> w a -> a) -> Dual (v a) -> Dual (w a)
>operator_map f g = covector $ f (bracket g)

>instance (DualNum f v, StandardBasis (f v), Show v) => Show (Dual (f v)) where
>   show (Covector f) = "dual" ++ (show $ fmap (f -!!<) $ unit_vectors)

>instance (Semigroup v) => Semigroup (LinearMap v v) where
>  f <> g = f . g

>instance (Semigroup v, VectorSpace v) => Monoid (LinearMap v v) where
>  mempty = id
>  mappend f g = f . g



>instance (StandardBasis (f v), Num v, VectorSpace (f v), DualNum f v) => InnerProductSpace (Dual (f v)) where
>   (Covector f) %. (Covector g) =
>     sum [(f -!!< x) * (g -!!< x) | x <- unit_vectors]

>instance (v ~ Scalar (f v), VectorSpace (f v), DualNum f v) => LieAlgebra (Dual (f v)) where
>  f %<>% g = f*g - g*f

>type DualNum f v = (LinearTransform f Vector1 v, ConjugateSymmetric v, Diagonalizable f v, Linearizable LinearMap (:*:) f Vector1 v, VectorSpace (f v), v ~ Scalar (f v))

>instance (DualNum f v) => Num (Dual (f v)) where
>  (Covector f) + (Covector g) =
>    covector $ \a -> (f -!!< a) + (g -!!< a)
>  (Covector f) - (Covector g) =
>    covector $ \a -> (f -!!< a) - (g -!!< a)
>  (Covector f) * (Covector g) =
>    covector $ \a -> (f -!!< a) * (g -!!< a)
>  negate (Covector f) = Covector $ arr_linear negate . f
>  abs (Covector f) = Covector $ arr_linear abs . f
>  signum (Covector f) = Covector $ arr_linear signum . f
>  fromInteger i = covector $ \_ -> fromInteger i

>instance (DualNum f v, FractionalSpace (f v)) => Fractional (Dual (f v)) where
>  (Covector f) / (Covector g) =
>     covector $ \a -> (f -!!< a) / (g -!!< a)
>  recip = scalar_map (arr_linear recip)
>  fromRational r = covector $ \_ -> fromRational r
> 
>instance (LinearTransform f Vector1 v, ConjugateSymmetric v, Linearizable LinearMap (:*:) f Vector1 v, FractionalSpace (f v),
>   v ~ Scalar (f v), Floating v, Diagonalizable f v)
> => Floating (Dual (f v)) where
>   pi = covector $ const pi
>   exp = scalar_map (arr_linear exp)
>   log = scalar_map (arr_linear log)
>   sqrt = scalar_map (arr_linear sqrt)
>   sin = scalar_map (arr_linear sin)
>   cos = scalar_map (arr_linear cos)
>   tan = scalar_map (arr_linear tan)
>   asin = scalar_map (arr_linear asin)
>   acos = scalar_map (arr_linear acos)
>   atan = scalar_map (arr_linear atan)
>   sinh = scalar_map (arr_linear sinh)
>   cosh = scalar_map (arr_linear cosh)
>   tanh = scalar_map (arr_linear tanh)
>   asinh = scalar_map (arr_linear asinh)
>   acosh = scalar_map (arr_linear acosh)
>   atanh = scalar_map (arr_linear atanh)


>-- | This computes
>-- \(DDM(\otimes,{\bar{x}},{\bar{y}}) = [({\bar{x}} \cdot \nabla) \times ({\bar{y}} \cdot \nabla)]\)
>-- where \(\times\) is the outer product for linear operators derived
>-- from product of dual spaces \(\otimes\). Notice \({\bar{x}} \cdot \nabla\)
>-- is normally called the directional derivative, so this is really a
>-- two dimensional version of directional derivative with some possibility
>-- to choose how two dual vectors are combined.
>dual_derivative ::
> (VectorDerivative v Dual LinearMap, VectorDerivative w Dual LinearMap,
> InnerProductSpace v, InnerProductSpace w)
> => (Dual v -> Dual w  -> c) -> v -> w -> (Dual v -> Dual w -> c)
>dual_derivative f x y = cells $ matrix f (x ·∇) (y ·∇)

>dual_differential_outer_product :: (LinearTransform f Vector1 v, ConjugateSymmetric v, Diagonalizable f v, v ~ Scalar (f v), Linearizable LinearMap (:*:) f Vector1 v, VectorDerivative (f v) Dual LinearMap, InnerProductSpace (f v))
> => f v -> f v -> Dual (f v) -> Dual (f v)  -> Dual (f v)
>dual_differential_outer_product = dual_derivative (*)

>dual_differential_dot_product :: (DualNum f v, VectorDerivative (f v) Dual LinearMap, InnerProductSpace (f v), StandardBasis (f v))
>  => f v -> f v -> Dual (f v) -> Dual (f v) -> Scalar (Dual (f v))
>dual_differential_dot_product = dual_derivative (%.)

>norm_covector :: (v ~ Scalar (f v), Diagonalizable f v, Linearizable LinearMap (:*:) f Vector1 v,
> NormedSpace (f v)) => Dual (f v)
>norm_covector = covector norm

>del2 :: (ConjugateSymmetric v, Closed v) => Vector2 (Dual (Vector2 v) -> Dual (Vector2 v))
>del2 = Vector2 partial_derivate2x partial_derivate2y

>hessian2 :: (ConjugateSymmetric a, Closed a) => Dual (Vector2 a) -> (Vector2 :*: Vector2) (Dual (Vector2 a))
>hessian2 f = matrix (\a b -> a (b f)) del2 del2

>-- | <https://en.wikipedia.org/wiki/Dual_space>
>instance (ConjugateSymmetric a, Num a) => StandardBasis ((Dual :*: Vector2) a) where
>  unit_vectors = [Matrix (covector xcoord2), Matrix (covector ycoord2)]
