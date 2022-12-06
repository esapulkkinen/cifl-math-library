>{-# LANGUAGE Safe, TypeFamilies, GADTs #-}
>module Math.Matrix.FreeVectorSpace where
>import safe Math.Matrix.Interface
>import safe Math.Tools.Universe
>import safe Math.Tools.Functor (interleave)
>import safe Control.Applicative

>-- FreeVectorSpace is left adjoint to Scalar from VectorSpace class.
>data FreeVectorSpace a where
>   Fzero :: FreeVectorSpace a
>   Fnegate :: FreeVectorSpace a -> FreeVectorSpace a
>   Fplus :: FreeVectorSpace a -> FreeVectorSpace a -> FreeVectorSpace a
>   Fproduct :: a -> FreeVectorSpace a -> FreeVectorSpace a
>  deriving (Show, Read)

>instance Functor FreeVectorSpace where
>   fmap f Fzero = Fzero
>   fmap f (Fnegate m) = Fnegate (fmap f m)
>   fmap f (Fplus a b) = Fplus (fmap f a) (fmap f b)
>   fmap f (Fproduct s v) = Fproduct (f s) (fmap f v)

>instance (Num a) => VectorSpace (FreeVectorSpace a) where
>   type Scalar (FreeVectorSpace a) = a
>   vzero = Fzero
>   vnegate = Fnegate
>   (%+) = Fplus
>   (%*) = Fproduct

>freeVectorSpace :: (VectorSpace v) => (a -> Scalar v) -> FreeVectorSpace a -> v
>freeVectorSpace _ Fzero = vzero
>freeVectorSpace f (Fnegate v) = vnegate (freeVectorSpace f v)
>freeVectorSpace f (Fplus a b) = freeVectorSpace f a %+ freeVectorSpace f b
>freeVectorSpace f (Fproduct s v) = f s %* freeVectorSpace f v

>-- free :: (a -> Scalar b) -> (Free a -> b)
>-- cofree :: (Scalar a -> b) -> (a -> CoFree b)
>-- cofree id :: a -> CoFree (Scalar a)
>-- free id :: Free (Scalar b) -> b

