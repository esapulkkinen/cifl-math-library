>{-# LANGUAGE TypeOperators #-}
>module Math.Matrix.HyperReal where
>import Math.Tools.Orthogonal
>import Control.Applicative
>import Math.Matrix.Interface
>import Math.Matrix.Stream
>
>data HR a = HR { differentials :: Stream a, st :: a, infinities :: Stream a }

>lessThan :: (Ord a) => HR a -> HR a -> HR Bool
>lessThan (HR x a x') (HR y b y') = HR (liftA2 (<) x y) (a < b) (liftA2 (<) x' y')
>
>lessEq :: (Ord a) => HR a -> HR a -> HR Bool
>lessEq (HR x a x') (HR y b y') = HR (liftA2 (<=) x y) (a <= b) (liftA2 (<=) x' y')

>equal :: (Eq a) => HR a -> HR a -> HR Bool
>equal (HR x a x') (HR y b y') = HR (liftA2 (==) x y) (a == b) (liftA2 (==) x' y')

>instance (Eq a) => Eq (HR a) where
>   x == y = st (equal x y)

>instance (Ord a) => Ord (HR a) where
>  x < y = st (x `lessThan` y)
>  x <= y = st (x `lessEq` y)

>derivate :: (Num a, Fractional b) => (HR a -> HR b) -> HR a -> b
>derivate f x = st $ divZero $ f (x + dx) - f x

>instance (Real a) => Real (HR a) where
>   toRational x = toRational (st x)

>outerHR :: (a -> b -> c) -> HR a -> HR b -> (HR :*: HR) c
>outerHR f aa@(HR x a x') bb@(HR y b y') = Matrix $ HR (outer f x bb)
>                                                      (fmap (f a) bb)
>                                                      (outer f x' bb)

>instance Functor HR where
>   fmap f (HR x a x') = HR (fmap f x) (f a) (fmap f x') 

>liftHR2 :: (a -> b -> c) -> HR a -> HR b -> HR c
>liftHR2 f (HR x a x') (HR y b y') = HR (liftA2 f x y) (f a b) (liftA2 f x' y')

>instance (Num a) => Num (HR a) where
>   x + y = liftHR2 (+) x y
>   x - y = liftHR2 (-) x y
>   (HR x a x') * (HR y b y') = HR (Pre a x*y) (a*b) (Pre b x'*y')
>   negate = fmap negate
>   abs = fmap abs
>   fromInteger i = HR 0 (fromInteger i) 0
>   signum = fmap signum

>instance (Eq a,Fractional a) => Fractional (HR a) where
>   t@(HR x a x') / s@(HR y b y') | b == 0 = divZero t / divZero s
>                                 | otherwise = HR (x / y) (a / b) (x'/y')
>   recip (HR x a x') = HR (recip x) (recip a) (recip x')
>   fromRational x = HR 0 (fromRational x) 0

>divZero :: HR a -> HR a
>divZero (HR x a x') = HR (stail x) (shead x) (Pre a x')

>dx :: (Num a) => HR a
>dx = HR (Pre 1 0) 0 0
>
>infinity :: (Num a) => HR a
>infinity = HR 0 0 (Pre 1 0)

>instance (Show a) => Show (HR a) where
>   show (HR (Pre x _) a (Pre y _)) = "... dx*" ++ show x ++ " + " ++ show a ++ " + " ++ show y ++ "/dx ..."
