>{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies  #-}
>{-# LANGUAGE FlexibleInstances, DefaultSignatures, FlexibleContexts #-}
>module Math.Number.Symbolic where
>import Math.Number.R
>import Math.Number.StreamInterface
>import Math.Number.Stream
>import Data.List (intersperse)
>import qualified Math.Number.Stream as S
>import Math.Matrix.Interface
>import Math.Tools.PrettyP
>import GHC.Base (stimes)
> 
>data Expr a where
>  EFromRational :: Expr Rational -> Expr R
>  EIntegral :: (Expr R, Expr R) -> Expr (R -> R) -> Expr R
>  ELambda :: (Expr a -> Expr b) -> Expr (a -> b)
>  EApp    :: Expr (a -> b) -> Expr a -> Expr b
>  EAdd    :: Expr a -> Expr a -> Expr a
>  ENegate :: Expr a -> Expr a
>  ELess :: Expr R -> Expr R -> Expr Bool

>data Coeff a = Coeff { pcoeff :: a, pindex :: Integer }

>instance (Show a, Eq a, Num a) => Show (Coeff a) where
>  show (Coeff c 0) = show c
>  show (Coeff 1 1) = show "x"
>  show (Coeff c 1) = show c ++ "x"
>  show (Coeff 1 i) = "x^" ++ show i
>  show (Coeff c i) = show c ++ "x^" ++ show i

>instance (Show a, Eq a, Num a) => Show (Poly a) where
>  show (Poly lst) = concat $ intersperse " + " [ show c | c <- reverse lst ]

>instance (Num a) => Monoid (Coeff a) where
>   mempty = Coeff 1 0
> 
>instance (Num a) => Semigroup (Coeff a) where
>   (Coeff a i) <> (Coeff b j) = Coeff (a * b) (i + j)
> 
>newtype Poly a = Poly { coefficients :: [Coeff a] }

>instance (Num a) => Monoid (Poly a) where
>   mempty = Poly []

>instance (Num a) => Semigroup (Poly a) where
>   (Poly lst) <> (Poly lst') = polySum (lst ++ lst')

>insertCoeff :: (a -> a -> a) -> Coeff a -> [Coeff a] -> [Coeff a]
>insertCoeff _ z [] = [z]
>insertCoeff f z1@(Coeff c i) (z2@(Coeff d j): r)
>   | i < j = z1 : insertCoeff f z2 r
>   | i > j = z2 : insertCoeff f z1 r
>   | otherwise = insertCoeff f (Coeff (f c d) i) r

>reduceCoefficients :: (Num a) => [Coeff a] -> [Coeff a]
>reduceCoefficients (cr@(Coeff c i) : r1@(dr@(Coeff d j) : r))
>  | i < j = insertCoeff (+) cr (reduceCoefficients r1)
>  | i > j = insertCoeff (+) dr (reduceCoefficients (cr : r))
>  | i == j = insertCoeff (+) (Coeff (c + d) i) (reduceCoefficients r)
>reduceCoefficients [x] = [x]
>reduceCoefficients [] = []

>polySum :: (Num a) => [Coeff a] -> Poly a
>polySum = Poly . reduceCoefficients

>polyTimes :: (Num a) => Poly a -> Poly a -> Poly a
>polyTimes (Poly lst) (Poly lst') = polySum [ x <> y | x <- lst, y <- lst' ]

>evalCoeff :: (Semigroup v, VectorSpace v) => Coeff (Scalar v) -> v -> v
>evalCoeff (Coeff c i) x = c %* stimes i x

>evalPoly :: (Semigroup v, VectorSpace v) => Poly (Scalar v) -> v -> v
>evalPoly (Poly lst) x = vsum [ evalCoeff c x | c <- lst]

>instance (VectorSpace a, Num a) => Num (Poly a) where
>  a + b = a <> b
>  a - b = a <> vnegate b
>  (*) = polyTimes
>  negate = vnegate
>  abs (Poly lst) = Poly [ Coeff (abs x) i | Coeff x i <- lst ]
>  signum (Poly lst) = Poly [ Coeff (signum x) i | Coeff x i <- lst ]
>  fromInteger = pconstant . fromInteger

>class Polynomial p a where
>  pconstant :: a -> p a
>  pXPower :: Integer -> p a

>pX :: (Num a) => Poly a
>pX = pXPower 1

>instance (Num a) => VectorSpace (Poly a) where
>  type Scalar (Poly a) = a
>  vzero = Poly []
>  vnegate (Poly lst) = Poly [ Coeff (negate x) i | Coeff x i <- lst ]
>  (Poly x) %+ (Poly y) = polySum (x ++ y)
>  k %* (Poly lst) = Poly [ Coeff (k * x) i | Coeff x i <- lst ]

>instance (Num a) => Polynomial Coeff a where
>   pconstant x = Coeff x 0
>   pXPower i = Coeff 1 i

>instance (Num a) => Polynomial Poly a where
>   pconstant x = Poly [pconstant x]
>   pXPower i = Poly [pXPower i]


