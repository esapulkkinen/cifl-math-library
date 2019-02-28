>{-# LANGUAGE Safe #-}
>module Math.Number.Interface where
>import Data.Ratio
>nthroot :: (Floating a) => a -> a -> a
>nthroot x k = exp (log x / k)

>logroot :: (Floating a) => a -> a -> a
>logroot x k = exp (log x / log k)

>class RationalRoots a where
>   rational_power :: a -> Rational -> a

>instance RationalRoots Float where
>   rational_power x r = x ** (fromRational r)

>instance RationalRoots Double where
>   rational_power x r = x ** (fromRational r)
