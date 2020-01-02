>-- -*- coding: utf-8 -*-
>{-# LANGUAGE TypeFamilies, PostfixOperators, UnicodeSyntax #-}
>module Math.Matrix.LogPolar where
>import Math.Matrix.Interface
>import Math.Matrix.Vector3
>import Data.Complex

>-- | This type is basically a logarithm of a complex number.
>--   <https://en.wikipedia.org/wiki/Log-polar_coordinates>
>data LogPolar2 a = LogPolar2 {
>   log_norm2 :: a,
>   polar_angle2 :: a
> }

>complexToPolar :: (RealFloat a) => Complex a -> LogPolar2 a
>complexToPolar c = let (a :+ b) = log c in LogPolar2 a b

>polarToComplex :: (RealFloat a) => LogPolar2 a -> Complex a
>polarToComplex (LogPolar2 log_norm ang) = exp (log_norm :+ ang)

>instance (Floating a) => NormedSpace (LogPolar2 a) where
>   norm = exp . log_norm2

>instance (Show a, RealFloat a) => Show (LogPolar2 a) where
>   show (LogPolar2 n a) = "exp(" ++ show n ++ ")∠ " ++ show (degrees a) ++ "°"

>instance (Floating a) => VectorSpace (LogPolar2 a) where
>   type Scalar (LogPolar2 a) = a
>   vzero = LogPolar2 0 0
>   vnegate (LogPolar2 r a) = LogPolar2 r (a + pi)
>   (LogPolar2 r a) %+ (LogPolar2 r' a') = LogPolar2 (r + r') (a + a')
>   a %* (LogPolar2 r b) = LogPolar2 (a * r) b

>-- | <https://en.wikipedia.org/wiki/Log-polar_coordinates>
>      
>data LogPolar3 a = LogPolar3 {
>  log_norm3 ∷ a,
>  polar_angle3 ∷ a,
>  azimuth_angle3 ∷ a }

>instance (Show a, RealFloat a) ⇒ Show (LogPolar3 a) where
>  show (LogPolar3 n a b) = "exp(" ++ show n ++ ")∠ " ++ show (degrees a) ++ "°∠ "
>                           ++ show (degrees b) ++ "°"

>instance (Floating a) ⇒ VectorSpace (LogPolar3 a) where
>  type Scalar (LogPolar3 a) = a
>  vzero = LogPolar3 0 0 0
>  vnegate (LogPolar3 x y z) = LogPolar3 x (y + pi) (z + pi)
>  (LogPolar3 x y z) %+ (LogPolar3 x' y' z') = LogPolar3 (x + x') (y + y') (z + z')
>  a %* (LogPolar3 x y z) = LogPolar3 (a * x) y z

>instance (Floating a) ⇒ NormedSpace (LogPolar3 a) where
>  norm (LogPolar3 r _ _) = exp r

>-- | <https://math.stackexchange.com/questions/243142/what-is-the-general-formula-for-calculating-dot-and-cross-products-in-spherical>
>instance (Floating a) ⇒ InnerProductSpace (LogPolar3 a) where
>  (LogPolar3 r a b) %. (LogPolar3 r' a' b')
>    = exp (r + r') * (cos a * cos a' + cos (b-b')*sin a * sin a')

>-- | degree to radian conversion
  
>fromDegree ∷ (Floating a) => a → a
>fromDegree x = x * pi / 180
  
>-- | radian to degrees conversion
>degrees∷(Floating a) ⇒ a → a
>degrees x = x * 180 / pi
  
>unitSphere ∷ (Floating a) ⇒ a → a → LogPolar3 a
>unitSphere a b = LogPolar3 0 (fromDegree a) (fromDegree b)
  
>isInsideUnitSphere ∷ (Num a, Ord a) ⇒ LogPolar3 a -> Bool
>isInsideUnitSphere x = log_norm3 x < 0
  
>-- | <https://en.wikipedia.org/wiki/List_of_common_coordinate_transformations>
>fromCartesian :: (RealFloat a) ⇒ Vector3 a → LogPolar3 a
>fromCartesian (Vector3 x y z) = LogPolar3 (log r) (acos $ z / r) (atan2 y x)
>  where r = sqrt (x*x+y*y+z*z)
  
>-- | <https://en.wikipedia.org/wiki/List_of_common_coordinate_transformations>
>toCartesian :: (RealFloat a) ⇒ LogPolar3 a → Vector3 a
>toCartesian (LogPolar3 logn a b) = Vector3 (r * sin a * cos b)
>                                  (r * sin a * sin b)
>                                   (r * cos a)
>  where r = exp logn

