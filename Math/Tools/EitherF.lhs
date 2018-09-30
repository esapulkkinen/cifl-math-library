>{-# LANGUAGE GADTs #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.EitherF where

>data PairF f g a = PairF (f a) (g a)
>data EitherF f g a = LeftF (f a) | RightF (g a)

>eitherF :: (f a -> b) -> (g a -> b) -> EitherF f g a -> b
>eitherF f _ (LeftF x)   = f x
>eitherF _ g (RightF y)  = g y

>join_alts :: (a -> f c) -> (b -> g c) -> Either a b -> EitherF f g c
>join_alts f g (Left x) = LeftF (f x)
>join_alts f g (Right y) = RightF (g y)

>data MixF p a where
>  MixF :: EitherF f g a -> MixF (PairF f g) a
