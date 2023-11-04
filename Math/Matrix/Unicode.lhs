>-- -*- coding: utf-8 -*-
>{-# LANGUAGE UnicodeSyntax, FlexibleContexts, TypeOperators, GADTs #-}
>-- |
>-- To use these, use emacs "Options/Select input method" and choose 'TeX'.
>-- Then enter the symbol by using backslash and the indicated name.
>-- for prefix operators, it's necessary to use parenthesis in applications.
>-- ctrl-\\ changes whether expansions occur.
>--
>-- Alternatively it's possible to use M-x insert-char to insert unicode characters
>module Math.Matrix.Unicode where
>
>import Math.Matrix.Interface
>import Math.Matrix.Vector3
>import Math.Matrix.Linear
>import Math.Number.Real
>import Math.Tools.CoFunctor
>import Math.Matrix.Covector
>
>-- | in
>(∈) ∷ (BinaryLogic p) ⇒ a → p a → Bool
>(∈) = isIn
> 
>-- | notin
>(∉) ∷ (BinaryLogic p) ⇒ a → p a → Bool
>(∉) = isNotIn

>-- | sum
>(∑) ∷ (VectorSpace a) ⇒ [a] → a
>(∑) = vsum



>-- | ast
>(∗) ∷ (VectorSpace v) ⇒ Scalar v → v → v
>(∗) = (%*)

>-- | cdot
>(·) ∷ (InnerProductSpace v) ⇒ v → v → Scalar v
>(·) = (%.)

>-- | neg
>(¬) ∷ (VectorSpace a) ⇒ a → a
>(¬) = vnegate

>-- | times
>(×) ∷ (LieAlgebra a) ⇒ a → a → a
>(×) = (%<>%)

>-- | prod
>(∏) ∷ (Num a) ⇒ [a] → a
>(∏) = product 

>-- | ddots
>(⋱) ∷ (Diagonalizable m a) ⇒ (m :*: m) a -> m a
>(⋱) = diagonal_impl
>
>-- | oplus
>(⊕) ∷ (VectorSpace v) ⇒ v → v → v
>(⊕) = (%+)

>-- | ominus
>(⊖) ∷ (VectorSpace v) ⇒ v → v → v
>(⊖) = (%-)
>

>-- | Unicode for tensor product. ("CIRCLED TIMES" character)
>(⊗) :: (Num a, Functor m, Functor n) => m a -> n a -> (m :*: n) a
>(⊗) = tensor_product

(⊗) :: (LinearTransform f g a, InnerProductSpace (v a),
   Linearizable LinearMap (:*:) f g a, Scalar (v a) ~ a)
 => f (v a) -> g (v a) -> f a :-> g a
(⊗) = linear_outer_product

>-- | otimes
>(⊗⊗) ∷ (SupportsMatrixMultiplication g h f a) ⇒ (g :*: h) a → (h :*: f) a → (g :*: f) a
>(⊗⊗) = (%*%)

>-- | boxtimes
>(⊠) ∷ (LieAlgebra m) ⇒ m → m → m
>(⊠) = (%<>%)

