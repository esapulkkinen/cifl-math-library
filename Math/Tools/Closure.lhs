>{-# OPTIONS_HADDOCK hide,prune #-}
>module Math.Tools.Closure where
>import Data.Monoid
>import Math.Tools.FixedPoint
>import Math.Number.Stream hiding (either)
>
>fix2 :: (b -> a) -> (a -> b) -> Either t t' -> Either a b
>fix2 f1 g1 x = either (Left . f) (Right . g) x
>   where f = f1 . g
>         g = g1 . f

>data Either3 a b c = Left3 a | Mid3 b | Right3 c

>either3 f g h (Left3 x) = f x
>either3 f g h (Mid3 y)  = g y
>either3 f g h (Right3 z) = h z


>fix3 :: (b -> a) -> (c -> b) -> (a -> c) -> Either3 t t' t'' -> Either3 a b c
>fix3 f1 g1 h1 x = either3 (Left3 . f) (Mid3 . g) (Right3 . h) x
>   where f = f1 . g
>         g = g1 . h
>         h = h1 . f

>fixN :: Stream (Either a a -> a)
>     -> Stream (b -> Either a b)
>     ->    b          -> a
>fixN (Pre f1 fr) (Pre f2 fr') = f1 . either Left (Right . fixN' fr fr') . f2

>fixN' :: Stream (Either a a -> a) -> Stream (b -> Either a b) -> b -> a
>fixN' (Pre f1 fr) (Pre f2 fr') = f1 . either Right (Left . fixN fr fr') . f2
