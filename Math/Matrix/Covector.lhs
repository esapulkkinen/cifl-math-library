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
>import safe Math.Matrix.Interface
>import safe Math.Tools.I
>import safe Data.Type.Equality
>import safe Math.Matrix.Linear
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Vector2
>import safe Math.Number.StreamInterface

linear_outer_product :: (InnerProductSpace p, VectorSpace v, Scalar v ~ calar p)
   => p -> v -> LinearMap p v
linear_outer_product a b = LinearMap Refl $
   Math.Matrix.Interface.outer a b

>contract :: (Scalar (v a) ~ a, LinearTransform v Vector1 a)
>  => (Dual :*: v) a -> (Vector1 :*: v) a -> Scalar (v a)
>contract (Matrix (Covector f)) (Matrix (Vector1 w)) = vectorElement (f -!< w)

>dual_apply :: (LinearTransform v Vector1 a)
> => Dual (v a) -> v a -> a
>dual_apply (Covector v) x = vectorElement (v -!< x)

>(*><) :: (a ~ Scalar (v a)) => Dual (v a) -> v a :-> Vector1 a
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
>(·∇) = directionalDerivative
> 
>-- | unicode support for curl. Use with parenthesis.
>(∇×) :: (VectorCrossProduct v arr) => arr v v -> arr v v
>(∇×) = curl
> 
>-- | unicode support for laplace. Use with parenthesis.
>(∇·∇) :: (VectorDerivative v d arr) => d v -> d v
>(∇·∇) = laplace

>-- doubly applied curl
>(∇×∇×) :: (VectorCrossProduct v arr) => arr v v -> arr v v
>(∇×∇×) = curl . curl

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

>directionalDerivativeEndo :: (VectorDerivative v Dual LinearMap, InnerProductSpace v)
>  => v -> Endo (Dual v)
>directionalDerivativeEndo v = Endo $ directionalDerivative v


>dconst :: (Scalar (f v) ~ v, Dualizable (f v) d, Linearizable LinearMap (:*:) f Vector1 v, Diagonalizable f v) => v -> d (f v)
>dconst x = covector $ \a -> x

>outerVector :: (Linearizable LinearMap (:*:) f g w, DualNum f w, DualNum g w) => Dual (f w) -> g w -> f w :-> g w
>outerVector (Covector y) x = arrLinear $ \w -> (y -!!< w) %* x

>-- | <http://en.wikipedia.org/wiki/Conjugate_transpose>
>conjugateTranspose :: (Diagonalizable m a, Linearizable LinearMap (:*:) m m a, Linearizable LinearMap (:*:) m n a, Linearizable LinearMap (:*:) n m a, Diagonalizable n a, LinearIso n m a, Transposable m n a, ConjugateSymmetric (m a))
>                    => m a :-> n a -> n a :-> m a
>conjugateTranspose f = arrLinear conj . transpose f

>-- | unicode alias (unicode HERMITIAN CONJUGATE MATRIX character)
>(⊹) :: (Linearizable LinearMap (:*:) m m a, Linearizable LinearMap (:*:) m n a, Linearizable LinearMap (:*:) n m a, Diagonalizable m a, Diagonalizable n a, LinearIso n m a, Transposable m n a, ConjugateSymmetric (m a))
>                    => m a :-> n a -> n a :-> m a
>(⊹) = conjugateTranspose

>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_hermitian :: (Eq (m (m a)), ConjugateSymmetric (m a), Linearizable LinearMap (:*:) m m a,
> Transposable m m a, Diagonalizable m a, LinearTransform m m a,
> Applicative m,
> Foldable m)
>  => m a :-> m a -> Bool
>is_hermitian a = a == conjugateTranspose a
>
>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_skew_hermitian :: (Num (Scalar (m a)), Eq (m (m a)),
> ConjugateSymmetric (m a), Applicative m, Linearizable LinearMap (:*:) m m a,
> VectorSpace (m a), Diagonalizable m a, LinearTransform m m a,
> Transposable m m a, Foldable m) => m a :-> m a -> Bool
>is_skew_hermitian a = a == (negate 1) %* (conjugateTranspose a)
>
>-- | <https://en.wikipedia.org/wiki/Conjugate_transpose>
>is_normal :: (Eq (m (m a)), Diagonalizable m a, LinearTransform m m a,
>            Linearizable LinearMap (:*:) m m a,
>            ConjugateSymmetric (m a)) => m a :-> m a -> Bool
>is_normal a = (conjugateTranspose a) . a == a . (conjugateTranspose a)

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

scalarMap :: Vector1 (Scalar a) :-> Vector1 (Scalar a) -> Dual a -> Dual a

>scalarMap :: (a ~ Scalar (f a)) => Vector1 a :-> Vector1 a -> Dual (f a) -> Dual (f a)
>scalarMap f (Covector g) = Covector (f . g)

>operator_map :: (Scalar (w a) ~ a, Dualizable (w a) Dual, Dualizable (v a) Dual, Diagonalizable w a, LinearTransform v Vector1 a, Linearizable LinearMap (:*:) w Vector1 a)
>  => ((v a -> a) -> w a -> a) -> Dual (v a) -> Dual (w a)
>operator_map f g = covector $ f (bracket g)

>instance (DualNum f v, StandardBasis (f v), Show v) => Show (Dual (f v)) where
>   show (Covector f) = "dual" ++ (show $ fmap (f -!!<) $ unitVectors)

>instance (Semigroup v) => Semigroup (LinearMap v v) where
>  f <> g = f . g

>instance (Semigroup v, VectorSpace v) => Monoid (LinearMap v v) where
>  mempty = id
>  mappend f g = f . g



>instance (StandardBasis (f v), Num v, VectorSpace (f v), DualNum f v) => InnerProductSpace (Dual (f v)) where
>   (Covector f) %. (Covector g) =
>     sum [(f -!!< x) * (g -!!< x) | x <- unitVectors]

>instance (v ~ Scalar (f v), VectorSpace (f v), DualNum f v) => LieAlgebra (Dual (f v)) where
>  f %<>% g = f*g - g*f

>type DualNum f v = (LinearTransform f Vector1 v, Dualizable (f v) Dual, ConjugateSymmetric v, Diagonalizable f v, Linearizable LinearMap (:*:) f Vector1 v, VectorSpace (f v))

>instance (DualNum f v) => Num (Dual (f v)) where
>  (Covector f) + (Covector g) =
>    covector $ \a -> (f -!!< a) + (g -!!< a)
>  (Covector f) - (Covector g) =
>    covector $ \a -> (f -!!< a) - (g -!!< a)
>  (Covector f) * (Covector g) =
>    covector $ \a -> (f -!!< a) * (g -!!< a)
>  negate (Covector f) = Covector $ arrLinear negate . f
>  abs (Covector f) = Covector $ arrLinear abs . f
>  signum (Covector f) = Covector $ arrLinear signum . f
>  fromInteger i = covector $ \_ -> fromInteger i

>instance (DualNum f v, FractionalSpace (f v)) => Fractional (Dual (f v)) where
>  (Covector f) / (Covector g) =
>     covector $ \a -> (f -!!< a) / (g -!!< a)
>  recip = scalarMap (arrLinear recip)
>  fromRational r = covector $ \_ -> fromRational r
> 
>instance (Dualizable (f v) Dual, LinearTransform f Vector1 v, ConjugateSymmetric v, Linearizable LinearMap (:*:) f Vector1 v, FractionalSpace (f v),
>   v ~ Scalar (f v), Floating v, Diagonalizable f v)
> => Floating (Dual (f v)) where
>   pi = covector $ const pi
>   exp = scalarMap (arrLinear exp)
>   log = scalarMap (arrLinear log)
>   sqrt = scalarMap (arrLinear sqrt)
>   sin = scalarMap (arrLinear sin)
>   cos = scalarMap (arrLinear cos)
>   tan = scalarMap (arrLinear tan)
>   asin = scalarMap (arrLinear asin)
>   acos = scalarMap (arrLinear acos)
>   atan = scalarMap (arrLinear atan)
>   sinh = scalarMap (arrLinear sinh)
>   cosh = scalarMap (arrLinear cosh)
>   tanh = scalarMap (arrLinear tanh)
>   asinh = scalarMap (arrLinear asinh)
>   acosh = scalarMap (arrLinear acosh)
>   atanh = scalarMap (arrLinear atanh)


>-- | This computes
>-- \(DDM(\otimes,{\bar{x}},{\bar{y}}) = [({\bar{x}} \cdot \nabla) \times ({\bar{y}} \cdot \nabla)]\)
>-- where \(\times\) is the outer product for linear operators derived
>-- from product of dual spaces \(\otimes\). Notice \({\bar{x}} \cdot \nabla\)
>-- is normally called the directional derivative, so this is really a
>-- two dimensional version of directional derivative with some possibility
>-- to choose how two dual vectors are combined.
>dualDerivative ::
> (VectorDerivative v Dual LinearMap, VectorDerivative w Dual LinearMap,
> InnerProductSpace v, InnerProductSpace w)
> => (Dual v -> Dual w  -> c) -> v -> w -> (Dual v -> Dual w -> c)
>dualDerivative f x y = cells $ matrix f (x ·∇) (y ·∇)

>dualDifferentialOuterProduct :: (Dualizable (f v) Dual, LinearTransform f Vector1 v, ConjugateSymmetric v, Diagonalizable f v, Linearizable LinearMap (:*:) f Vector1 v, VectorDerivative (f v) Dual LinearMap, InnerProductSpace (f v))
> => f v -> f v -> Dual (f v) -> Dual (f v)  -> Dual (f v)
>dualDifferentialOuterProduct = dualDerivative (*)

>dualDifferentialDotProduct :: (DualNum f v, VectorDerivative (f v) Dual LinearMap, InnerProductSpace (f v), StandardBasis (f v))
>  => f v -> f v -> Dual (f v) -> Dual (f v) -> Scalar (Dual (f v))
>dualDifferentialDotProduct = dualDerivative (%.)

>normCovector :: (Diagonalizable f v, Linearizable LinearMap (:*:) f Vector1 v, Dualizable (f v) d,
> NormedSpace (f v)) => d (f v)
>normCovector = covector norm

>del2 :: (ConjugateSymmetric v, Closed v) => Vector2 (Dual (Vector2 v) -> Dual (Vector2 v))
>del2 = Vector2 partialDerivate2x partialDerivate2y

>hessian2 :: (ConjugateSymmetric a, Closed a) => Dual (Vector2 a) -> (Vector2 :*: Vector2) (Dual (Vector2 a))
>hessian2 f = matrix (\a b -> a (b f)) del2 del2

>-- | <https://en.wikipedia.org/wiki/Dual_space>
>instance (ConjugateSymmetric a, Num a) => StandardBasis ((Dual :*: Vector2) a) where
>  unitVectors = [Matrix (covector xcoord2), Matrix (covector ycoord2)]
