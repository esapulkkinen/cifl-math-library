>{-# LANGUAGE TypeFamilies, LambdaCase #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Either3 where
>import Math.Tools.Visitor

>data Either3 a b c = Left3 a
>                   | Middle3 b
>                   | Right3 c

>instance Functor (Either3 a b) where
>   fmap f = \case
>     (Left3 x) -> Left3 x
>     (Middle3 y) -> Middle3 y
>     (Right3 z) -> Right3 (f z)

>instance Visitor (Either3 a b c) where
>   data Fold (Either3 a b c) d = FoldEither3 (a -> d) (b -> d) (c -> d)
>   visit (FoldEither3 f _ _) (Left3 x)   = f x
>   visit (FoldEither3 _ g _) (Middle3 y) = g y
>   visit (FoldEither3 _ _ h) (Right3 z)  = h z

>instance Builder (Either3 a b c) where
>   data Unfold (Either3 a b c) d = UnfoldEither3 (d -> Either a (Either b c))
>   build (UnfoldEither3 f) ctx = case f ctx of
>                 (Left x) -> Left3 x
>                 (Right (Left x)) -> Middle3 x
>                 (Right (Right y)) -> Right3 y
