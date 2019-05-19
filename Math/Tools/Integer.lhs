>{-# LANGUAGE Safe #-}
>module Math.Tools.Integer where


>square_root_integer :: Integer -> Integer
>square_root_integer i = dropUntil (newton_square_root_list i)
>  where dropUntil (a:b:r) | a == b  = a
>                          | a + 1 == b = a 
>                          | a == b + 1 = b
>                          | otherwise = dropUntil r
>        dropUntil _ = 0

>-- | Some numbers may cause newton_square_root_list to produce an infinite
>-- sequence such as newton_square_root_list 48 == [1,24,13,8,7,8,7,...]
>-- We guard against this by terminating if the difference between two
>-- adjacent results is less than 2.
>newton_square_root_list :: (Integral a) => a -> [a]
>newton_square_root_list i = 1 : map (\x -> (x + (i `div` x)) `div` 2) (newton_square_root_list i)

>gcd_ :: (Integral a) => a -> a -> a
>gcd_ 0 b = b
>gcd_ a 0 = a
>gcd_ a b | a > b     = gcd_ (a - b) b
>         | otherwise = gcd_ (b - a) a

>-- | See Knuth: TAOCP, or
>-- <http://www.math.harvard.edu/~mazur/preprints/when_is_one.pdf>
>rising_factorial_power :: (Num a, Eq a) => a -> a -> a
>rising_factorial_power 0 _ = 0
>rising_factorial_power x 0 = 1
>rising_factorial_power x n = x * rising_factorial_power (x+1) (n-1)

>-- | See Knuth: TAOCP
>-- or <http://www.math.harvard.edu/~mazur/preprints/when_is_one.pdf>
>falling_factorial_power :: (Num a,Eq a) => a -> a -> a
>falling_factorial_power x 0 = 1
>falling_factorial_power 0 _ = 0
>falling_factorial_power x n = x * (falling_factorial_power (x-1) (n-1))

using product form

>-- | <http://en.wikipedia.org/wiki/Binomial_coefficient>
>binomial :: (Enum a, Fractional a) => a -> a -> a
>binomial n k = product [(n - (k - i)) / i | i <- [1..k]]
