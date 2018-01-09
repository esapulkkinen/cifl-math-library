>module Math.Number.BiStream where
>import Math.Number.Stream

>data BiStream a = BiStream { bifst :: Stream a, bisnd :: Stream a }

>instance (Show a) => Show (BiStream a) where
>   show (BiStream (Pre x xr) (Pre y yr)) = "(" ++ show x ++ "," ++ show y ++ ")" ++ show (BiStream xr yr)

>integers_bistream :: (Num a) => BiStream a
>integers_bistream = BiStream nonzero_naturals naturals

>instance Functor BiStream where
>   fmap f (BiStream a b) = BiStream (fmap f a) (fmap f b)

