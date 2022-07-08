>{-# LANGUAGE Safe,TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
>{-# LANGUAGE ScopedTypeVariables, QuantifiedConstraints #-}
>module Math.Matrix.Instances where
>import Control.Applicative
>import Math.Matrix.Interface
>import Math.Matrix.Linear

>-- | <https://en.wikipedia.org/wiki/Matrix_mechanics>

commutator :: (SupportsMatrixMultiplication f f f a) => f a :-> f a -> f a :-> f a -> f a :-> f a

>commutator x y = (x %*% y) %- (y %*% x)

>-- | <https://en.wikipedia.org/wiki/Characteristic_polynomial>
>characteristicPolynomial :: (LinearTraceable LinearMap m a, VectorSpace (m a),
> Linearizable LinearMap (:*:) m m a, LinearTransform m m a, Scalar (m a) ~ a)
>    => m a :-> m a -> a -> a
>characteristicPolynomial (m' :: m a :-> m a) v = determinant ((v %* MatIdentity) %- m')


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
