>{-# OPTIONS_HADDOCK prune #-}
>module Math.Tools.Group where

>class (Monad g) => Group g where
>      greturn :: top -> g top
>      gjoin   :: g (g top) -> g top
>      glift   :: (a -> b) -> a -> g b
>      gbind   :: (a -> g b) -> g a -> g b
>      greturn = return
>      gjoin = (>>= id)
>      glift f = fmap f . greturn
>      gbind f = gjoin  . fmap f

>gjoin3 :: (Group g) => g (g (g top)) -> g top
>gjoin3 = gjoin . gjoin

>gmap :: (Group g) => (a -> b) -> g (g a) -> g b
>gmap f = gjoin . fmap (fmap f)

>join_nested :: (Group g1, Group g) => (g1 a -> g (g b)) -> a -> g b
>join_nested f = gjoin . f . greturn

>group_leftAdjunct :: (Group g, Group g1) => (g1 a -> c) -> a -> g c
>group_leftAdjunct x = greturn . x . greturn

>group_flip :: (Group f, Group g) => (f a -> g (g b)) -> f (f a) -> g b
>group_flip x = gjoin . x . gjoin

>group_join_self :: (Group g) => g a -> g a
>group_join_self = gjoin . greturn

>instance Group Maybe
>instance Group IO
>instance Group []
