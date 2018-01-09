>-- -*- coding: utf-8 -*-
>{-# LANGUAGE UnicodeSyntax, FlexibleContexts, TypeOperators #-}
>module Math.Matrix.Unicode where
>-- ^
>-- To use these, use emacs "Options/Select input method" and choose 'TeX'.
>-- then enter the symbol by using backslash and the indicated name.
>-- for prefix operators, it's necessary to use parenthesis in applications.
>-- ctrl-\ changes whether expansions occur.
>import Math.Matrix.Interface
>import Math.Matrix.Vector3
>import Model.Real
>import Math.Tools.CoFunctor
>

>(∈) ∷ (BinaryLogic p) ⇒ a → p a → Bool
>(∈) = isIn
> 
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
>(⋱) ∷ (SquareMatrix m a) ⇒ (m :*: m) a -> m a
>(⋱) = diagonal
>
>-- | oplus
>(⊕) ∷ (VectorSpace v) ⇒ v → v → v
>(⊕) = (%+)

>-- | ominus
>(⊖) ∷ (VectorSpace v) ⇒ v → v → v
>(⊖) = (%-)
>

>-- | otimes
>(⊗) ∷ (Functor g, Transposable h f, InnerProductSpace (h a))
>     ⇒ (g :*: h) a → (h :*: f) a → (g :*: f)(Scalar (h a))
>(⊗) = (%*%)

>-- | boxtimes
>(⊠) ∷ (LieAlgebra m) ⇒ m → m → m
>(⊠) = (%<>%)

