>{-# LANGUAGE GADTs, PolyKinds, FlexibleInstances,
>    MultiParamTypeClasses, TypeFamilies, DeriveFunctor, TypeOperators,
>    StandaloneDeriving
> #-}
> 
>module Math.Number.Cyclotomic where
>import Math.Matrix.Interface
>import Math.Number.Units
>import Data.Ratio
>import Math.Number.Stream
>import Math.Number.StreamInterface
>import Data.Complex

The field cyclotomic polynomials.

>data CPoly q

>data Cyclotomic a where
>  CyclotomicRootOfUnity :: Integer -> Cyclotomic Rational -- ROU(n)
>  CyclotomicPower :: Integer -> Cyclotomic a -> Cyclotomic a -- (ROU(n)^k)
>  CyclotomicPoly :: [Cyclotomic Rational] -> Cyclotomic (CPoly Rational) -- (x - ROU(n)^k_1)*...*(x - ROU(n)^k_n)

>instance Show (Cyclotomic a) where
>  show (CyclotomicRootOfUnity n) = "exp(2πi/" ++ show n ++ ")"
>  show (CyclotomicPower n x) = show x ++ "^" ++ show n
>  show (CyclotomicPoly []) = show "1"
>  show (CyclotomicPoly [c]) = show "(x - " ++ show c ++ ")"
>  show (CyclotomicPoly (c:cr)) = show "(x - " ++ show c ++ ")" ++ show (CyclotomicPoly cr)


>cosFractionalAngle :: Rational -> Cyclotomic Rational
>cosFractionalAngle q = CyclotomicPower (numerator q) (CyclotomicRootOfUnity (denominator q))

>sinFractionalAngle :: Rational -> Cyclotomic Rational
>sinFractionalAngle q = cosFractionalAngle (q - 1%4)

>-- TODO: does this make any sense? Cyclotomic is already subset of complex numbers.
>expFractionalAngle :: Rational -> Complex (Cyclotomic Rational)
>expFractionalAngle bi = (cosFractionalAngle bi :+ sinFractionalAngle bi)


>data FractionalAngle a = FractionOfCircle { fractionalAngle :: a } -- represents angle of 2*pi*q
>                       | CyclotomicAngle { cyclotomicAngle :: Cyclotomic a }
>  deriving (Show)

instance DimensionalFloating FractionalAngle Rational where
  fractionOfRotation = FractionOfCircle

>instance (Fractional r) => VectorSpace (FractionalAngle r) where
>  type Scalar (FractionalAngle r) = r
>  vzero = FractionOfCircle 0
>  vnegate (FractionOfCircle a) = FractionOfCircle (negate a)
>  (FractionOfCircle q) %+ (FractionOfCircle q') = FractionOfCircle (q + q')
>  k %* (FractionOfCircle q) = FractionOfCircle (k * q)

>fractionalHalfRotation :: FractionalAngle Rational
>fractionalHalfRotation = FractionOfCircle (1 % 2)

>-- | Codiagonal version of chebyshev polynomials means the polynomials
>-- should be read from codiagonals. That is, 'codiagonals chebyshevPolynomialsCodiag'
>-- is a stream of lists of coefficients of each polynomial.
>chebyshevPolynomialsCodiag :: (Stream :*: Stream) Integer
>chebyshevPolynomialsCodiag = lowerTriangleImpl chebyshevPolynomials

>-- | Codiagonal version of chebyshev polynomials means the polynomials
>-- should be read from codiagonals. That is, 'codiagonals chebyshevPolynomialsCodiag'
>-- is a stream of lists of coefficients of each polynomial.
>chebyshevPolynomialsSecondKindCodiag :: (Stream :*: Stream) Integer
>chebyshevPolynomialsSecondKindCodiag = lowerTriangleImpl chebyshevPolynomialsSecondKind

>-- | <https://en.wikipedia.org/wiki/Chebyshev_polynomials>
>-- Chebyshev polynomials are polynomial functions T_n that are
>-- solutions to equation: T_n(cos(theta)) = cos (n*theta)
>-- and also to equation: T_n(cosh(theta)) = cosh (n*theta)
>chebyshevPolynomials :: (Stream :*: Stream) Integer
>chebyshevPolynomials = Matrix $ (1 - s_z2 * s_z) `div` (1 - 2*s_z*s_z2  + s_z*s_z)

>chebyshevPolynomialsSecondKind :: (Stream :*: Stream) Integer
>chebyshevPolynomialsSecondKind = Matrix $ inversion $ 1 - 2*s_z*s_z2 + s_z*s_z
