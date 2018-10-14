>{-# OPTIONS_HADDOCK hide,prune #-}
>module Math.Tools.Triangle where
>import Control.Arrow
>import Math.Number.Stream
>
>data Triangle a b = Triangle {
>     triangle_diagonal :: b,
>     triangle_rest :: Triangle a (a,b)
>   }

>instance Functor (Triangle a) where
>   fmap f (Triangle x r) = Triangle (f x) (fmap (second f) r)

>trimap :: (a -> b) -> (c -> d) -> Triangle a c -> Triangle b d
>trimap f g (Triangle x t) = Triangle (g x) (trimap f (f *** g) t)

>tdiagonal :: Triangle a b -> Stream b
>tdiagonal (Triangle b r) = b `Pre` tdiagonal (fmap snd r)

>trow :: Triangle a b -> Stream a
>trow (Triangle b (Triangle (a,b') r)) = Pre a $ trow $ Triangle b'
>     $ fmap (\ ~(a,~(a',b)) -> (a,b)) r

>tfirst :: Triangle a b -> Triangle a a
>tfirst = fmap fst . triangle_rest

>trest :: Triangle a b -> Triangle a b
>trest = fmap snd . triangle_rest

>identity_triangle :: a -> b -> Triangle a b
>identity_triangle a b = Triangle b (identity_triangle a (a,b))

>natural_triangle :: Triangle Integer Integer
>natural_triangle = Triangle 0 $ trimap succ (\x -> (x,x)) natural_triangle

>instance (Show a, Show b) => Show (Triangle a b) where
>   show z = triangle_limit_show 10 z

>triangle_limit_show :: (Show a, Show b) => Integer -> Triangle a b -> String
>triangle_limit_show 0 _ = "..."
>triangle_limit_show i (Triangle b r) = show b ++ "\n" ++ triangle_limit_show (pred i) r
