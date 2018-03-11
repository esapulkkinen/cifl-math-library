>{-# LANGUAGE Safe,DataKinds, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators, KindSignatures, TypeFamilies, ExistentialQuantification, ScopedTypeVariables #-}
>module Math.Matrix.FiniteVector where
>import Control.Applicative
>import Math.Matrix.Interface
>import GHC.TypeLits

>data Vec :: Nat -> * -> * where
>  Empty :: Vec 0 a 
>  Cons :: a -> Vec n a -> Vec (1 + n) a 

>instance Functor (Vec n) where
>   fmap f Empty = Empty
>   fmap f (Cons x xr) = Cons (f x) (fmap f xr)

>vlength :: Vec n a -> Integer
>vlength Empty = 0
>vlength (Cons _ r) = 1 + vlength r

>vhead :: Vec (1+n) a -> a
>vhead (Cons x _) = x

vtail :: (KnownNat n) => Vec (1+n) a -> Vec n a
vtail (Cons _ r) = r

Doesn't typecheck in GHC 7.10.3, "Could not deduce (n1 ~ n) from the context
((1+n1) ~ (1+n)).

>foldv :: b -> (a -> b -> b) -> Vec n a -> b
>foldv e _ Empty = e
>foldv e f (Cons x xr) = f x (foldv e f xr)

>sum_vcoordinates :: (Num a) => Vec n a -> a
>sum_vcoordinates = foldv 0 (+)

>sum_vsquares :: (Num a) => Vec n a -> a
>sum_vsquares = foldv 0 (\x y -> x*x + y)

>vtrace :: (Num a, Diagonalizable n) => (Vec n :*: Vec n) a -> a
>vtrace = sum_vcoordinates . vdiagonal . cells

>toList :: Vec n a -> [a]
>toList = foldv [] (:)

>toListEnc :: EncVec a -> [a]
>toListEnc (EncVec x) = toList x

>data EncVec a = forall n. EncVec (Vec n a)

>instance (Show a) => Show (EncVec a) where
>  show (EncVec Empty) = ""
>  show (EncVec (Cons x xr)) = show x ++ " " ++ show (EncVec xr)

>fromList :: [a] -> EncVec a
>fromList [] = EncVec Empty
>fromList (x:xr) = case fromList xr of { (EncVec v) -> EncVec $ Cons x v }

>instance (Diagonalizable n , VectorSpace (Vec n a), Transposable (Vec n) (Vec n), Num a) => SquareMatrix (Vec n) a where
>   identity = Matrix videntity
>   diagonal_matrix x = Matrix (vdiagonal_matrix x)
>   diagonal (Matrix x) = vdiagonal x

>class Diagonalizable n where
>   vnull :: (Num a) => Vec n a
>   vdiagonal :: Vec n (Vec n a) -> Vec n a
>   videntity :: (Num a) => Vec n (Vec n a)
>   vdiagonal_matrix :: (Num a) => Vec n a -> Vec n (Vec n a)

>instance Diagonalizable 0 where
>   vnull = Empty
>   vdiagonal Empty = Empty
>   videntity = Empty
>   vdiagonal_matrix Empty = Empty

>instance (Show a) => Show (Vec n a) where
>  show Empty = ""
>  show (Cons x xr) = show x ++ " " ++ show xr

>instance Show (((Vec 0) :*: (Vec n)) a) where
>  show (Matrix Empty) = ""


>bindv :: (Diagonalizable n, Functor (Vec n)) => Vec n a -> (a -> Vec n b) -> Vec n b
>bindv z f = vdiagonal $ fmap f z
