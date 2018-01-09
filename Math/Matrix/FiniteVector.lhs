>{-# LANGUAGE Safe,DataKinds, MultiParamTypeClasses, GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators, KindSignatures, TypeFamilies, ExistentialQuantification #-}
>module Math.Matrix.FiniteVector where
>import Control.Applicative
>import Math.Matrix.Interface
>import GHC.TypeLits

data Nat = Zero | Succ Nat
type OneT = 'Succ 'Zero
type TwoT = 'Succ OneT
type ThreeT = 'Succ TwoT
type FourT = 'Succ ThreeT
type FiveT = 'Succ FourT
type SixT = 'Succ FiveT

>data Vec :: Nat -> * -> * where
>  Empty :: Vec 0 a 
>  Cons :: a -> Vec n a -> Vec (1 + n) a 

>instance Functor (Vec n) where
>   fmap f Empty = Empty
>   fmap f (Cons x xr) = Cons (f x) (fmap f xr)

instance Applicative (Vec 0) where
  pure x = Empty
  Empty <*> Empty = Empty

type family Plus (a :: Nat) (b :: Nat) :: Nat
type instance Plus 'Zero 'Zero = 'Zero
type instance Plus ('Succ n) 'Zero = 'Succ n
type instance Plus 'Zero ('Succ n) = 'Succ n
type instance Plus ('Succ n) ('Succ m) = 'Succ (Plus n ('Succ m))

pureVec :: a -> Vec n a
pureVec x = if natVal (undefined :: Proxy (n :: Nat)) == 0 then Empty
                                                   else Cons x (pureVec x)

pureVec :: (KnownNat n) => a -> Vec n a
pureVec x = if natVal (undefined :: Proxy (n :: Nat)) == 0 then Empty :: Vec n a else Cons x (pureVec x) :: Vec n a


instance Applicative (Vec n) where
  pure x = if natVal (Proxy :: Proxy n) == 0 then Empty else Cons x (pure x)
  (Cons f fr) <*> (Cons x xr) = Cons (f x) (fr <*> xr)
  Empty <*> Empty = Empty

>vlength :: Vec n a -> Integer
>vlength Empty = 0
>vlength (Cons _ r) = 1 + vlength r

>vhead :: Vec (1+n) a -> a
>vhead (Cons x _) = x

Doesn't typecheck in GHC 7.10.3, "Could not deduce (n1 ~ n) from the context
((1+n1) ~ (1+n)).

vtail :: Vec (1+n) a -> Vec n a
vtail (Cons _ r) = r

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

instance (Num a, Functor (Vec n), VectorSpace (Vec n a),Diagonalizable n) => FiniteSquareMatrix (Vec n) a where
    trace = vtrace

instance (Functor (Vec n)) => Transposable (Vec n) (Vec n) where
   transpose (Matrix v) = Matrix $ vtranspose v

class TransposableVector n m where
   vtranspose :: Vec n (Vec m a) -> Vec m (Vec n a)


vtranspose Empty = Empty
vtranspose (Cons Empty r) = Empty
vtranspose (Cons (Cons x r) r') = Cons (Cons x (fmap velem r'))
                                  (liftA2 Cons r $ fmap vtail r')

instance TransposableVector n m where

instance TransposableVector (n+1) 0 where
   vtranspose (Cons Empty r) = Empty

instance (TransposableVector 0 n) => TransposableVector 0 (n+1) where
   vtranspose Empty = Cons Empty (vtranspose Empty)

instance (Applicative (Vec n)) => TransposableVector (n+1) (n+1) where

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

instance (Show (Vec m a), Show (((Vec n) :*: (Vec m)) a)) => Show (((Vec (n+1)) :*: (Vec m)) a) where
   show (Matrix (Cons x xr)) = show x ++ "\n" ++ show (Matrix xr)

instance (Diagonalizable n, Functor (Vec n)) => Diagonalizable (n+1) where
   vnull = Cons 0 vnull
   vdiagonal (Cons (Cons x _) dr) = Cons x $ vdiagonal $ fmap vtail dr   
   videntity = Cons (Cons 1 vnull) (fmap (Cons 0) videntity)
   vdiagonal_matrix (Cons x xr) = Cons (Cons x vnull) (fmap (Cons 0) (vdiagonal_matrix xr))

instance (Num a) => VectorSpace (Vec n a) where
   type Scalar (Vec 0 a ) = a
   vzero = Empty
   vnegate Empty = Empty
   Empty %+ Empty = Empty
   a %* Empty = Empty

instance (Num a, a ~ Scalar (Vec n a), VectorSpace (Vec n a))
 => VectorSpace (Vec (n+1) a) where
   type Scalar (Vec (n+1) a) = a
   vzero = Cons 0 vzero
   vnegate (Cons x r) = Cons (negate x) (vnegate r)
   (Cons a r) %+ (Cons a' r') = Cons (a+a') (r %+ r')
   a %* (Cons b r) = Cons (a * b) (a %* r)

instance (Num a) => InnerProductSpace (Vec 0 a) where
   Empty %. Empty = 0

instance (a ~ Scalar (Vec n a), ConjugateSymmetric a, Num a, InnerProductSpace (Vec n a))
 => InnerProductSpace (Vec (n+1) a) where
   (Cons a r) %. (Cons b r') = a*conj b + r %. r'

instance (a ~ Scalar (Vec n a), VectorSpace (Vec n a), InnerProductSpace (Vec n a), Floating a, ConjugateSymmetric a) => NormedSpace (Vec (n+1) a) where
   norm x = sqrt (x %. x)

instance Functor (Vec 0) where
   fmap f Empty = Empty

instance (Functor (Vec n)) => Functor (Vec (n+1)) where
   fmap f (Cons a r) = Cons (f a) (fmap f r)

>bindv :: (Diagonalizable n, Functor (Vec n)) => Vec n a -> (a -> Vec n b) -> Vec n b
>bindv z f = vdiagonal $ fmap f z

instance Monad (Vec 0) where
   (>>=) = bindv
   return x = Empty

instance (Monad (Vec n), Diagonalizable n) => Monad (Vec (n+1)) where
   (>>=) = bindv
   return x = Cons x (return x)
