>{-# LANGUAGE RankNTypes #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Equality where
>import Prelude hiding (EQ)

>data EQ a = EQ (a -> a -> Bool)

>data EQMap a = EQMap (a -> a -> Bool) a

>instance Eq (EQMap a) where
>   (EQMap f x) == (EQMap _ y) = f x y

by :: (Functor f) => EQ a -> (forall b. (Eq b) => f b) -> EQMap a
by (EQ f) h = fmap (EQMap f) h

