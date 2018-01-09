>{-# LANGUAGE ExistentialQuantification #-}
>module Math.Tools.Complex where
>import Data.Complex

>imaginary_unit :: (Num a) => Complex a
>imaginary_unit = (0 :+ 1)

>mapComplex :: (a -> b) -> Complex a -> Complex b
>mapComplex f (a :+ b) = f a :+ f b

>returnComplex :: (Num a) => a -> Complex a
>returnComplex x = x :+ 0

>joinComplex :: (Num a) => Complex (Complex a) -> Complex a
>joinComplex ((a :+ b) :+ (c :+ d)) = (a - d) :+ (b + c)
