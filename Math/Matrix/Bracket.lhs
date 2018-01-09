>{-# LANGUAGE Safe,FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances, TypeFamilies #-}
>module Math.Matrix.Bracket where
>import Data.Complex
>import Math.Tools.CoFunctor
>import Math.Matrix.Matrix
>import Math.Matrix.Interface
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Vector3

Tools.Covector might be better than this module.

>-- | <http://en.wikipedia.org/wiki/Conjugate_transpose Conjugate transpose>
>--   <http://en.wikipedia.org/wiki/Bra%E2%80%93ket_notation bracket notation>

>conjugate_transpose :: (Functor m, Functor n, RealFloat a,Transposable m n,
>                        LinearTransform m n (Complex a))
>                    => (m :*: n) (Complex a) -> (n :*: m) (Complex a)
>conjugate_transpose = fmap conjugate . transpose

(%<|>%) :: (LinearTransform n Vector1 (Complex a), Functor n,
            RealFloat a, CoordinateSpace n)
        => n (Complex a) -> n (Complex a) -> Complex a

>-- | <http://en.wikipedia.org/wiki/Riesz_representation_theorem Riesz representation theorem>
>--   <http://en.wikipedia.org/wiki/Dual_space dual space>
>--   <http://en.wikipedia.org/wiki/Banach_space banach space>
>--    K. Chandrasekhara Rao: Functional analysis.

>data Dual v = Dual { bracket :: v -> Scalar v }

>instance (StandardBasis v, Show (Scalar v)) => Show (Dual v) where
>   show (Dual f) = show $ map f $ unit_vectors

>dual_map :: (Scalar f ~ Scalar g) => (f -> g) -> Dual g -> Dual f
>dual_map f (Dual g) = Dual (g . f)

>dual :: (InnerProductSpace v) => v -> Dual v
>dual x = Dual $ \y -> y %. x

>dual_basis :: (InnerProductSpace v) => [v] -> [Dual v]
>dual_basis lst = fmap (\i -> Dual $ \v -> i %. v) lst

>instance (InnerProductSpace v, StandardBasis v) 
>  => StandardBasis (Dual v) where
>   unit_vectors = dual_basis unit_vectors

>instance (Num (Scalar v)) => Num (Dual v) where
>  (Dual f) + (Dual g) = Dual (\x -> f x + g x)
>  (Dual f) - (Dual g) = Dual (\x -> f x - g x)
>  (Dual f) * (Dual g) = Dual (\x -> f x * g x)
>  negate (Dual f) = Dual (negate . f)
>  abs (Dual f) = Dual (abs . f)
>  signum (Dual f) = Dual (signum . f)
>  fromInteger i   = Dual (const (fromInteger i))

>instance (Num (Scalar v)) => VectorSpace (Dual v) where
>  type Scalar (Dual v) = Scalar v
>  vzero = Dual $ const 0
>  vnegate (Dual f) = Dual $ \x -> negate (f x)
>  x %* (Dual f) = Dual (\v -> x * f v)
>  (Dual f) %+ (Dual g) = Dual $ \x -> f x + g x

