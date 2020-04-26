>{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell, QuasiQuotes, GADTs #-}
>{-# LANGUAGE UnicodeSyntax #-}
>{-# OPTIONS_HADDOCK hide #-}
>module Math.Matrix.QuantumMechanics where
>import Control.Applicative
>import Data.Complex
>import Math.Number.DimensionalAnalysis
>import Math.Tools.Complex
>import Math.Number.Stream
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Matrix.Interface
>import Math.Matrix.Covector
>import Math.Number.Real
>import Math.Matrix.QuasiQuoter

>-- | <https://en.m.wikipedia.org/wiki/Stone%E2%80%93von_Neumann_theorem?wprov=slfa1

position :: (VectorSpace v) => (Scalar v -> v) -> Scalar v -> v

>position :: (Num a) => (a -> a) -> a -> a
>position psi x0 = x0 * psi x0
>

momentum :: (Infinitesimal a, Closed a, RealFloat a)
         => (Complex a -> Complex a) -> Complex a -> Complex a

>momentum :: (Show a, RealFloat a, Closed a, Infinitesimal a) 
>         =>  (Complex (Quantity a) -> Complex (Quantity a))
>         -> Complex (Quantity a) -> Complex (Quantity a)
>momentum psi x0 = - i * (ħ :+ 0) * complex_derivate psi x0

>-- <http://en.wikipedia.org/wiki/Planck_Constant>
>hbar :: (Show a, Floating a) => Quantity a
>hbar = 6.62607004081e-34 * joule * second

>ħ :: (Show a, Floating a) => Quantity a
>ħ = hbar


>-- <http://en.wikipedia.org/wiki/Matrix_representation_of_Maxwell's_equations>
>riemann_silberstein_plus ::
>   (Scalar b ~ Complex v, RealFloat v, VectorSpace b)
> => Dual (Vector4 (Complex v))
> -> Dual (Vector4 (Complex v))
> -> (Vector4 (Complex v) -> b)
> -> (Vector4 (Complex v) -> b)
> -> Vector4 (Complex v) -> b
>riemann_silberstein_plus permit permea e b rt = (1 / sqrt 2) %* (sqrt(permit `bracket` rt) %* e rt)
>     %+ (i / (sqrt(permea `bracket` rt))) %* b rt

>-- <http://en.wikipedia.org/wiki/Matrix_representation_of_Maxwell's_equations>

>riemann_silberstein_minus ::
>   (Scalar b ~ Complex v, RealFloat v, VectorSpace b)
> => Dual (Vector4 (Complex v))
> -> Dual (Vector4 (Complex v))
> -> (Vector4 (Complex v) -> b)
> -> (Vector4 (Complex v) -> b)
> -> Vector4 (Complex v) -> b
>riemann_silberstein_minus permit permea e b rt = 
>   (1 / sqrt 2) %* (sqrt(permit `bracket` rt) %* e rt)
>     %- (i / (sqrt(permea `bracket` rt))) %* (b rt)

>theta_plus :: (RealFloat v) =>
> (Dual (Vector4 (Complex v))) -> (Dual (Vector4 (Complex v)))
> -> (Vector4 (Complex v) -> Vector3 (Complex v))
> -> (Vector4 (Complex v) -> Vector3 (Complex v))
> -> Vector4 (Complex v) -> Vector4 (Complex v)
>theta_plus permit permea e b rt = Vector4 (vnegate fx + i * fy)
>                                          fz
>                                          fz
>                                          (fx + i * fy)
>   where Vector3 fx fy fz = riemann_silberstein_plus permit permea e b rt

>theta_minus :: (RealFloat v) =>
> (Dual (Vector4 (Complex v))) -> (Dual (Vector4 (Complex v)))
> -> (Vector4 (Complex v) -> Vector3 (Complex v))
> -> (Vector4 (Complex v) -> Vector3 (Complex v))
> -> Vector4 (Complex v) -> Vector4 (Complex v)

>theta_minus permit permea e b rt = Vector4 (negate fx - i * fy)
>                                           fz
>                                           fz
>                                           (fx - i * fy)
>   where Vector3 fx fy fz = riemann_silberstein_minus permit permea e b rt

>velocity :: Floating a => (b -> a) -> (b -> a) -> b -> a
>velocity permit permea rt = 1 / sqrt (permit rt * permea rt)

>resistance :: (Floating a) => (b -> a) -> (b -> a) -> b -> a
>resistance permit permea rt = sqrt ( permea rt / permit rt)

>u_func :: (Floating (Scalar v), VectorDerivative v) => Dual v -> Dual v -> v -> v
>u_func (Covector permit) (Covector permea) rt =
>   (1 / (2 * velocity permit permea rt))
>   %* (∇) (Covector $ velocity permit permea) -!< rt

>w_func :: (Floating (Scalar v), VectorDerivative v) => Dual v -> Dual v -> v -> v
>w_func (Covector permit) (Covector permea) rt = (1 / (2 * resistance permit permea rt))
> %* (∇) (Covector $ resistance permit permea) -!< rt


>-- <http://en.wikipedia.org/wiki/Matrix_representation_of_Maxwell's_equations>
>-- These are the M-matrices.
>maxwell_x = (1 :+ 0) %* [double4x4|0 0 1 0
>                                  0 0 0 1
>                                  1 0 0 0
>                                  0 1 0 0|]
>maxwell_y = i %* [double4x4|0 0 -1 0
>                           0 0 0 -1
>                           1 0 0 0
>                           0 1 0 0|]
>maxwell_z = (1 :+ 0) %* [double4x4|1 0 0 0
>                                  0 1 0 0
>                                  0 0 -1 0
>                                  0 0 0 -1|]
>maxwell = Vector3 maxwell_x maxwell_y maxwell_z

>heisenberg_matrix_for_harmonic_oscillator
>   :: (Floating a) => (Stream :*: Stream) a
>heisenberg_matrix_for_harmonic_oscillator =
>     remove_row $ remove_column $ liftA2 (+)
>                 (add_column (constant 0)
>                        (diagonal_matrix (fmap sqrt naturals)))
>                 (add_row (constant 0)
>                        (diagonal_matrix (fmap sqrt naturals)))

>heisenberg_matrix_for_harmonic_oscillator_without_sqrt
>   :: (Floating a) => (Stream :*: Stream) a
>heisenberg_matrix_for_harmonic_oscillator_without_sqrt =
>     remove_row $ remove_column $ liftA2 (+)
>                 (add_column (constant 0)
>                        (diagonal_matrix naturals))
>                 (add_row (constant 0)
>                        (diagonal_matrix naturals))


>c :: (Floating a) => Quantity a
>c = speed_of_light

>klein_gordon :: (Show a, Floating a, Closed a, Infinitesimal a)
>  => Quantity a -> Dual (Vector4 (Quantity a)) -> Dual (Vector4 (Quantity a))
>klein_gordon m ϕ = (1 / (c*c)) %* (derivate4t_squared ϕ)
>   %- ((∇·∇) ϕ) %+ (((m ^ 2* c ^ 2)/(ħ*ħ)) %* ϕ)

>i :: (Num a) => Complex a
>i= 0:+1

>-- | <https://en.wikipedia.org/wiki/Schr%C3%B6dinger_equation>
>schrodinger :: (Show a, RealFloat a, Closed a, Infinitesimal a)
>            => Quantity (Complex a)
>            -> Dual (Vector4 (Quantity (Complex a)))
>            -> Dual (Vector4 (Quantity (Complex a)))
>            -> Dual (Vector4 (Quantity (Complex a)))
>schrodinger μ ϕ v = Covector $ \ rt ->
>   (i %* ħ) * (derivate4t ϕ *>< rt)
>  %+ (1 / (2 * μ)) * ħ^2 * ((∇·∇) ϕ *>< rt)
>  %- (v `bracket` rt) * (ϕ *>< rt)
