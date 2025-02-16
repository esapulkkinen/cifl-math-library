>{-# LANGUAGE Safe #-}
>module Math.Tools.Integer where


>squareRootInteger :: Integer -> Integer
>squareRootInteger i = dropUntil (newtonSquareRootList i)
>  where dropUntil (a:b:r) | a == b  = a
>                          | a + 1 == b = a 
>                          | a == b + 1 = b
>                          | otherwise = dropUntil r
>        dropUntil _ = 0

>-- | Some numbers may cause newton_square_root_list to produce an infinite
>-- sequence such as newton_square_root_list 48 == [1,24,13,8,7,8,7,...]
>-- We guard against this by terminating if the difference between two
>-- adjacent results is less than 2.
>newtonSquareRootList :: (Integral a) => a -> [a]
>newtonSquareRootList i = 1 : map (\x -> (x + (i `div` x)) `div` 2) (newtonSquareRootList i)

>gcd_ :: (Integral a) => a -> a -> a
>gcd_ 0 b = b
>gcd_ a 0 = a
>gcd_ a b | a > b     = gcd_ (a - b) b
>         | otherwise = gcd_ (b - a) a

>-- | See Knuth: TAOCP, or
>-- <http://www.math.harvard.edu/~mazur/preprints/when_is_one.pdf>
>risingFactorialPower :: (Num a, Eq a) => a -> a -> a
>risingFactorialPower 0 _ = 0
>risingFactorialPower x 0 = 1
>risingFactorialPower x n = x * risingFactorialPower (x+1) (n-1)

>-- | See Knuth: TAOCP
>-- or <http://www.math.harvard.edu/~mazur/preprints/when_is_one.pdf>
>fallingFactorialPower :: (Num a,Eq a) => a -> a -> a
>fallingFactorialPower x 0 = 1
>fallingFactorialPower 0 _ = 0
>fallingFactorialPower x n = x * (fallingFactorialPower (x-1) (n-1))

using product form

>-- | <http://en.wikipedia.org/wiki/Binomial_coefficient>
>binomial :: (Enum a, Fractional a) => a -> a -> a
>binomial n k = product [(n - (k - i)) / i | i <- [1..k]]
