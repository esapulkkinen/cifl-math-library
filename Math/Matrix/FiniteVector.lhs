>{-# LANGUAGE Safe,DataKinds, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators, KindSignatures, TypeFamilies, ExistentialQuantification, ScopedTypeVariables, TypeOperators, AllowAmbiguousTypes #-}
>{-# LANGUAGE OverloadedStrings #-}
>module Math.Matrix.FiniteVector where
>import qualified Text.PrettyPrint as Pretty
>import Control.Applicative
>import Math.Matrix.Interface
>import Math.Tools.PrettyP
>import GHC.TypeLits

>-- | See <https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html>
>-- for another approach on finite vectors.
>data Vec :: Nat -> * -> * where
>  Empty :: Vec 0 a 
>  Cons :: a -> Vec n a -> Vec (1 + n) a
>  Assoc :: Vec (n + (m + p)) a -> Vec ((n + m) + p) a

>vzero_vec = Empty
>zero = Empty
>one = Cons () zero
>two = Cons () one
>three = Cons () two
>four = Cons () three
>five = Cons () four
>six = Cons () five
>seven = Cons () six
>
>one_index = Cons 0 zero
>two_index = succ_index one_index
>three_index = succ_index two_index
>four_index = succ_index three_index
>five_index = succ_index four_index

>succ_index :: (Enum a) => Vec n a -> Vec (1+n) a
>succ_index z@(Cons i x) = Cons (toEnum 0) (fmap succ z)

>constant_vec :: a -> Vec n b -> Vec n a
>constant_vec x v = fmap (const x) v

>vnegate_vec :: (Num a) => Vec n a -> Vec n a
>vnegate_vec = fmap negate

>class Appendable n m where
>   vplus :: Vec n a -> Vec m a -> Vec (n + m) a

>instance Appendable 0 0 where
>   vplus Empty Empty = Empty

>instance Appendable n 0 where
>   vplus v Empty = v

>instance Appendable 0 n where
>   vplus Empty v = v

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

>vtrace :: (Num a, DiagonalizableVector n) => (Vec n :*: Vec n) a -> a
>vtrace = sum_vcoordinates . vdiagonal . cells

>toList :: Vec n a -> [a]
>toList = foldv [] (:)

>toListEnc :: EncVec a -> [a]
>toListEnc (EncVec x) = toList x

>data EncVec a = forall n. EncVec (Vec n a)

>instance (Show a) => Show (EncVec a) where
>  show (EncVec Empty) = ""
>  show (EncVec (Cons x xr)) = show x ++ " " ++ show (EncVec xr)

>encvecLength :: EncVec a -> Integer
>encvecLength (EncVec Empty) = 0
>encvecLength (EncVec (Cons x xr)) = 1 + encvecLength (EncVec xr)
>encvecLength (EncVec (Assoc f)) = encvecLength (EncVec f)

>fromList :: [a] -> EncVec a
>fromList [] = EncVec Empty
>fromList (x:xr) = case fromList xr of { (EncVec v) -> EncVec $ Cons x v }

>instance (DiagonalizableVector n , VectorSpace (Vec n a), Transposable (Vec n) (Vec n), Num a) => Diagonalizable (Vec n) a where
>   vector_dimension m = vdimension m
> --  identity = Matrix . videntity
>   diagonal_matrix x = Matrix (vdiagonal_matrix x)
>   diagonal (Matrix x) = vdiagonal x

>class DiagonalizableVector n where
>   vnull :: (Num a) => Vec n a
>   vdimension :: Vec n a -> Vec n Integer
>   vdiagonal :: Vec n (Vec n a) -> Vec n a
>   videntity :: (Num a) => Vec n () -> Vec n (Vec n a)
>   vdiagonal_matrix :: (Num a) => Vec n a -> Vec n (Vec n a)
>   vlist :: Vec n a -> [a]

>instance DiagonalizableVector 0 where
>   vnull = Empty
>   vdimension Empty = Empty
>   vdiagonal Empty = Empty
>   videntity _ = Empty
>   vdiagonal_matrix Empty = Empty
>   vlist _ = []

>instance PpShowVerticalF (Vec n) where
>   ppf_vertical Empty = ""
>   ppf_vertical (Cons x xr) = Pretty.nest 4 (pp x) Pretty.$$ (ppf xr)

>instance PpShowF (Vec n) where
>   ppf Empty = ""
>   ppf (Cons x xr) = pp x Pretty.<+> ppf xr

>instance (Show a) => Show (Vec n a) where
>  show Empty = ""
>  show (Cons x xr) = show x ++ " " ++ show xr

>instance Show (((Vec 0) :*: (Vec n)) a) where
>  show (Matrix Empty) = ""


>bindv :: (DiagonalizableVector n, Functor (Vec n)) => Vec n a -> (a -> Vec n b) -> Vec n b
>bindv z f = vdiagonal $ fmap f z
