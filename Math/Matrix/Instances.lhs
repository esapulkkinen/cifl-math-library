>{-# LANGUAGE Safe,TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
>module Math.Matrix.Instances where
>import Control.Applicative
>import Math.Matrix.Interface

>-- | <https://en.wikipedia.org/wiki/Matrix_mechanics>

>commutator :: (Transposable f f, Num (Scalar (f a)), InnerProductSpace (f a),
>               VectorSpaceOver (f a) a, VectorSpace ((f :*: f) a),
>           Applicative f) => (f :*: f) a -> (f :*: f) a -> (f :*: f) a
>commutator x y = (x %*% y) %- (y %*% x)

>-- | <https://en.wikipedia.org/wiki/Characteristic_polynomial>
>characteristicPolynomial :: (FiniteSquareMatrix m a, Num a, Applicative m,
>                    VectorSpaceOver ((m :*: m) a) a)
>    => (m :*: m) a -> a -> a
>characteristicPolynomial m v = determinant (v %* identity %- m)


instance (Num a, Applicative f, Scalar (f a) ~ a, Transposable f f, InnerProductSpace (f a))
 => LieAlgebra ((f :*: f) a) where
   (%<>%) = commutator


instance {-# OVERLAPPABLE #-}
     (Num a, Applicative f, Applicative g) => VectorSpace ((f :*: g) a) where
  type Scalar ((f :*: g) a) = a
  vzero = Matrix $ pure (pure 0)
  vnegate (Matrix x) = Matrix $ fmap (fmap negate) x
  (Matrix x) %+ (Matrix y) = Matrix $ liftA2 (liftA2 (+)) x y
  s %* (Matrix x) = Matrix $ fmap (fmap (s *)) x

instance {-# OVERLAPPABLE #-}
   (a ~ Scalar (g a), FiniteSquareMatrix f a, Transposable f g,
     Transposable g f, InnerProductSpace (g a)) => InnerProductSpace ((f :*: g) a) where
  a %. b = trace (a %*% transpose b)

instance {-# OVERLAPPABLE #-}
   (Num a, Num (f a), a ~ Scalar (f a), InnerProductSpace (f a), SquareMatrix f a, Applicative f) => Num ((f :*: f) a) where
  (Matrix m) + (Matrix n) = Matrix $ liftA2 (liftA2 (+)) m n
  (Matrix m) - (Matrix n) = Matrix $ liftA2 (liftA2 (-)) m n
  m * n = m %*% n
  negate m = Matrix $ fmap negate $ cells m
  abs m = Matrix $ fmap abs $ cells m
  signum m = Matrix $ fmap signum $ cells m
  fromInteger a = (fromInteger a) %* identity
