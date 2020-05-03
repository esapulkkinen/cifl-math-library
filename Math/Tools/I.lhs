>{-# LANGUAGE Safe #-}
>module Math.Tools.I where
>data I a = I { unI :: !a }

>instance Applicative I where
>   pure x = I x
>   (I f) <*> (I x) = I (f x)

>instance Monad I where
>   return x = I x
>   (I x) >>= f = f x
>   fail _ = error "I failure"

>instance (Show a) => Show (I a) where
>  show (I x) = "I" ++ show x

>instance Functor I where
>   fmap f (I x) = I (f x)

>unfmapI :: (I a -> I b) -> a -> b
>unfmapI f = unI . f . I

>data V a = V

>instance Show (V a) where
>  show V = "V"
