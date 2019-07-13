>{-# LANGUAGE Safe,GADTs, TypeFamilies #-}
>module Math.Tools.Median where
>import Data.Ratio
>import Math.Tools.Adjunction (swap)
>import Math.Matrix.Interface

>-- | Rules expected of MedianAlgebra.
>--
>-- From "Knuth: The Art of Computer Programming" section 7.1.1 <http://www-cs-faculty.stanford.edu/~uno/taocp.html>
>-- 
>-- === "median/majority"
>--
>-- prop>     med x x y == x
>--
>-- === "median/commutative"
>--
>-- prop> med x y z == med x z y
>-- prop> med x y z == med y z x
>-- prop> med x y z == med z x y
>-- prop> med x y z == med z y x
>--  
>-- === "median/associative"
>--
>-- prop> med x w (med y w z) == med (med x w y) w z
>--
>-- === "median/distributive"
>--
>-- prop> med (med x y z) u v == med x (med y u v) (med z u v)
>--
>class (Ord m) => MedianAlgebra m where
>   med :: m -> m -> m -> m

>median5 :: (MedianAlgebra a) => a -> a -> a -> a -> a -> a
>median5 v w x y z = med v (med x y z) (med w x (med w y z))

>median :: (Ord a) => a -> a -> a -> a
>median x y z = (x `min` y) `max` (y `min` z) `max` (x `min` z)

>instance (MedianAlgebra a) => MedianAlgebra [a] where
>   med a  [] []= []
>   med [] a  []= []
>   med [] [] a = []
>   med (c:cr) (d:dr) []  = (min c d:zipWith min cr dr)
>   med [] (c:cr) (d:dr)  = (min c d:zipWith min cr dr)
>   med (c:cr) [] (d:dr)  = (min c d:zipWith min cr dr)
>   med (c:cr) (d:dr) (e:er) = (med c d e : med cr dr er)

>instance MedianAlgebra Bool where { med = median }
>instance MedianAlgebra Integer where { med = median }
>instance MedianAlgebra Float where { med = median }
>instance MedianAlgebra Double where { med = median }
>instance MedianAlgebra Int where { med = median }
>instance MedianAlgebra Char where { med = median }
>instance MedianAlgebra Ordering where { med = median }
>instance (Integral a) => MedianAlgebra (Ratio a) where { med = median }
>instance (MedianAlgebra a, MedianAlgebra b) => MedianAlgebra (a,b) where
>  med (x1,x2) (y1,y2) (z1,z2) = (med x1 y1 z1,med x2 y2 z2) 
>instance (MedianAlgebra a, MedianAlgebra b, MedianAlgebra c) => MedianAlgebra (a,b,c) where
>  med (x1,x2,x3) (y1,y2,y3) (z1,z2,z3) = (med x1 y1 z1, med x2 y2 z2, med x3 y3 z3)
>instance (MedianAlgebra a, MedianAlgebra b, MedianAlgebra c, MedianAlgebra d) => MedianAlgebra (a,b,c,d) where
>  med (x1,x2,x3,x4) (y1,y2,y3,y4) (z1,z2,z3,z4) = (med x1 y1 z1, med x2 y2 z2, med x3 y3 z3, med x4 y4 z4)
>instance (MedianAlgebra a, MedianAlgebra b, MedianAlgebra c, MedianAlgebra d, MedianAlgebra e) => MedianAlgebra (a,b,c,d,e) where
>  med (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) (z1,z2,z3,z4,z5) = (med x1 y1 z1, med x2 y2 z2, med x3 y3 z3, med x4 y4 z4, med x5 y5 z5)

>data Interval a = Interval { interval_startpoint :: a,
>                             interval_endpoint   :: a }

>instance (Show a) => Show (Interval a) where
>  show (Interval a b) = show (a,b)

>instance Functor Interval where
>  fmap f (Interval a b) = Interval (f a) (f b)

>instance Applicative Interval where
>   pure x = Interval x x
>   (Interval fa fb) <*> (Interval xa xb) = Interval (fa xa) (fb xb)

>instance Monad Interval where
>   return x = Interval x x
>   (Interval a b) >>= f = Interval a' b''
>     where Interval a' _   = f a
>           Interval _  b'' = f b
>   fail msg = Interval (error msg) (error msg)

>instance (Num a) => VectorSpace (Interval a) where
>  type Scalar (Interval a) = a
>  vzero = Interval 0 0
>  vnegate (Interval x y) = Interval y x
>  (Interval x y) %+ (Interval x' y') = Interval (x + x') (y + y')
>  x %* (Interval a b) = Interval (x * a) (x * b)

>instance (Num a) => NormedSpace (Interval a) where
>  norm (Interval a b) = abs (b - a)

>instance (Num a) => InnerProductSpace (Interval a) where
>  (Interval a b) %. (Interval a' b') = (b-a)*(b'-a')

>in_interval :: (Eq a, MedianAlgebra a) => a -> Interval a -> Bool
>in_interval x (Interval u v) = x == med x u v

>interval :: (Monad m, MedianAlgebra a) => Interval a -> m a -> m a
>interval (Interval u v) m = m >>= \x -> return (med x u v)

>median_homomorphism :: (MedianAlgebra a) => Interval a -> a -> a
>median_homomorphism (Interval u v) x = med x u v

>instance (Num a, Ord a) => Num (Interval a) where
>  (Interval a b) + (Interval a' b') = Interval (a+a') (b+b')
>  (Interval a b) * (Interval a' b') = Interval (a*a') (b*b')
>  (Interval a b) - (Interval a' b') = Interval (a-a') (b-b')
>  negate (Interval a b) = Interval b a
>  abs (Interval a b) | a >= b    = Interval b a
>                     | otherwise = Interval a b
>  signum (Interval a b) = Interval (signum a) (signum b)
>  fromInteger i = Interval (fromInteger i) (fromInteger i)


>-- | andor is same as \ (x,y) -> (x && y, x || y), thus the name, except
>-- that the type 'b' is more general. Thus if both 'and' and 'or' are combined,
>-- sorting of the pair occurs.

>andor :: (Ord a) => (a,a) -> (a,a)
>andor (x,y) = (if x <= y then id else swap) (x,y)

>andor' :: (Ord a) => (a,a) -> (a,a)
>andor' (x,y) = (min x y, max x y)

>andor3 :: (Ord a) => (a,a,a) -> (a,a,a)
>andor3 (x,y,z) = (x'',y''',z')
>  where (x',y') = andor (x,y)
>        (y'',z') = andor (y',z)
>        (x'',y''') = andor (x',y'')
