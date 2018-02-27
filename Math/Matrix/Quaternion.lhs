>{-# LANGUAGE TypeFamilies, TypeOperators #-}
>module Math.Matrix.Quaternion where
>import Data.Complex
>import Math.Matrix.Interface hiding (inverse)
>import Math.Matrix.Vector2
>import Math.Matrix.Vector4
>
>-- | Quaternion interpretation of four-component vectors.
>newtype Quaternion a = Quaternion { quaternion_vector :: Vector4 a }

>instance (Show a) => Show (Quaternion a) where
>  show (Quaternion (Vector4 a b c d)) = show a ++ " + " ++ show b ++ "i + " ++ show c ++ "j + " ++ show d ++ "k"

>instance Functor Quaternion where
>   fmap f (Quaternion x) = Quaternion (fmap f x)

>instance Applicative Quaternion where
>   pure x = Quaternion $ pure x
>   (Quaternion f) <*> (Quaternion x) = Quaternion $ f <*> x

>instance (Num a) => VectorSpace (Quaternion a) where
>   type Scalar (Quaternion a) = a
>   vzero = Quaternion vzero
>   vnegate (Quaternion x) = Quaternion $ vnegate x
>   (Quaternion x) %+ (Quaternion y) = Quaternion (x %+ y)
>   x %* (Quaternion v) = Quaternion (x %* v)

>instance (ConjugateSymmetric a, Floating a) => NormedSpace (Quaternion a) where
>   norm (Quaternion v) = sqrt(v %. v)


>-- | <https://en.wikipedia.org/wiki/Quaternion>
>qproduct :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
>qproduct (Quaternion (Vector4 a1 b1 c1 d1))
>         (Quaternion (Vector4 a2 b2 c2 d2)) =
>     Quaternion $ Vector4 (a1*a2 - b1*b2 - c1*c2 - d1*d2)
>                          (a1*b2 + b1*a2 + c1*d2 - d1*c2)
>                          (a1*c2 - b1*d2 + c1*a2 + d1*b2)
>                          (a1*d2 + b1*c2 - c1*b2 + d1*a2)

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>qconjugate :: (Num a) => (Quaternion a) -> (Quaternion a)
>qconjugate (Quaternion (Vector4 a b c d)) = Quaternion $ Vector4 a (negate b) (negate c) (negate d)

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>inverse :: (ConjugateSymmetric a, Floating a) => Quaternion a -> Quaternion a
>inverse q = (norm q * norm q) %* qconjugate q
>
>-- | <https://en.wikipedia.org/wiki/Quaternion>
>left_divide :: (ConjugateSymmetric a, Floating a) => Quaternion a -> Quaternion a -> Quaternion a
>left_divide q p = inverse q * p

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>right_divide :: (ConjugateSymmetric a, Floating a) => Quaternion a -> Quaternion a -> Quaternion a
>right_divide q p = q * inverse p

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>split :: (Num a) => Quaternion a -> (Quaternion a, Quaternion a)
>split (Quaternion (Vector4 t x y z)) = (Quaternion (Vector4 t 0 0 0),
>                                        Quaternion (Vector4 0 x y z))

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>qexp :: (ConjugateSymmetric a,Floating a) => Quaternion a -> Quaternion a
>qexp q = Quaternion $ exp (tcoord4 t) %* (cos nv %* t4 %+ (sin nv / nv) %* v)
>    where (Quaternion t,Quaternion v) = split q
>          nv = norm v

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>qlog q = Quaternion $ log nq %* t4 %+ (acos (tcoord4 t / nq) / norm v) %* v
>   where (Quaternion t,Quaternion v) = split q
>         nq = norm q

>instance (Floating a, ConjugateSymmetric a) => Fractional (Quaternion a) where
>   (/) = left_divide
>   recip = inverse
>   fromRational q = Quaternion $ Vector4 (fromRational q) 0 0 0

>instance (Floating a, ConjugateSymmetric a) => Floating (Quaternion a) where
>   exp = qexp
>   log = qlog

>instance (Num a) => ConjugateSymmetric (Quaternion a) where
>   conj = qconjugate

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>unit :: (Num a) => Quaternion a
>unit = Quaternion $ Vector4 1 0 0 0

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>i :: (Num a) => Quaternion a
>i    = Quaternion $ Vector4 0 1 0 0

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>j :: (Num a) => Quaternion a
>j    = Quaternion $ Vector4 0 0 1 0

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>k :: (Num a) => Quaternion a
>k    = Quaternion $ Vector4 0 0 0 1

>instance (Num a) => Num (Quaternion a) where
>   v1 + v2 = pure (+) <*> v1 <*> v2
>   v1 - v2 = pure (-) <*> v1 <*> v2
>   (*) = qproduct
>   negate = fmap negate
>   abs = fmap abs
>   signum = fmap signum
>   fromInteger i = Quaternion $ Vector4 (fromInteger i) (fromInteger 0) (fromInteger 0) (fromInteger 0)

>-- | <https://en.wikipedia.org/wiki/Quaternion>
>quaternion_matrix :: (Num a) => Quaternion a -> (Vector2 :*: Vector2) (Complex a)
>quaternion_matrix (Quaternion (Vector4 a b c d)) = Matrix $ Vector2 
>               (Vector2 (a :+ b) (c :+ d))
>               (Vector2 (negate c :+ d) (a :+ negate b))
