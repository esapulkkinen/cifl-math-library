>{-# LANGUAGE GADTs #-}
>module Math.Tools.ExpressionMonad where
>import Control.Monad

>data EM m a where
>  Prim   :: m a -> EM m a
>  Return :: a -> EM m a
>  Bind :: EM m a -> (a -> EM m b) -> EM m b
>  Appl :: EM m (a -> b) -> EM m a -> EM m b

>runEM :: (Monad m) => EM m a -> m a
>runEM (Prim m)   = m
>runEM (Return x) = return x
>runEM (Bind m f) = runEM m >>= (runEM . f)

>instance Applicative (EM m) where
>   pure = Return
>   f <*> g = Appl f g

>instance Monad (EM m) where
>  return = Return
>  (>>=)  = Bind

>instance Functor (EM m) where
>  fmap = liftM
