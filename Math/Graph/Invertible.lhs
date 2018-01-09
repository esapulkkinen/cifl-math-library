>{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
>module Math.Graph.Invertible where

>class Convertible a b where
>   identity :: a -> b

>class (Convertible a b) => Invertible a b where
>   domain   :: b -> a
>   -- domain . identity   == id

>class (Invertible a b) => Graph a b where
>   codomain :: b -> a
>   -- codomain . identity == id

>class (Graph a b) => NestedGraph a b where
>   difference   :: a -> a -> b

>newtype N = N (Maybe N)

>instance Convertible (Maybe N) N where { identity = N }
>instance Invertible (Maybe N) N where  { domain (N x) = x }
>instance Graph (Maybe N) N where { codomain (N y) = y }
>instance NestedGraph (Maybe N) N where 
>   difference (Just (N x)) (Just (N y)) = difference x y
>   difference (Just x) Nothing  = N (Just x)
>   difference Nothing (Just y)  = N (Just y)
>   difference Nothing Nothing = N Nothing

>data Z = Z N N

>instance Convertible N Z where { identity x = Z x x }
>instance Invertible  N Z where { domain (Z x _) = x }
>instance Graph N Z where { codomain (Z _ y) = y }
>instance NestedGraph N Z where
>   difference (N (Just x)) (N (Just y)) = difference x y
>   difference a b = Z a b

>data Q = Q Z Z

>instance Convertible Z Q where { identity x = Q x x }
>instance Invertible Z Q where { domain (Q x _) = x }
>instance Graph Z Q where { codomain (Q _ y) = y }
>instance NestedGraph Z Q where 
>   difference (Z x x') (Z y y') = Q (difference x y') (difference x' y)
