>{-# LANGUAGE Safe, RankNTypes, LinearTypes #-}
>module Math.Tools.I where
>import Math.Tools.PrettyP
> 
>newtype I a = I { unI :: a }

>data Void a = Void

>instance (Num a) => Num (I a) where
>  (I x) + (I y) = I (x + y)
>  (I x) - (I y) = I (x - y)
>  (I x) * (I y) = I (x * y)
>  negate (I x) = I (negate x)
>  signum (I x) = I (signum x)
>  fromInteger i = I (fromInteger i)
>  abs (I x) = I (abs x)

>instance (Fractional a) => Fractional (I a) where
>  (I x) / (I y) = I (x/y)
>  recip (I x) = I (recip x)
>  fromRational x = I (fromRational x)

>instance (Floating a) => Floating (I a) where
>  pi = I pi
>  exp (I x) = I (exp x)
>  log (I x) = I (log x)
>  sqrt (I x) = I (sqrt x)
>  (I x) ** (I y) = I (x ** y)
>  logBase (I x) (I y) = I (logBase x y)
>  sin (I x) = I (sin x)
>  cos (I x) = I (cos x)
>  tan (I x) = I (tan x)
>  asin (I x) = I (asin x)
>  acos (I x) = I (acos x)
>  atan (I x) = I (atan x)
>  sinh (I x) = I (sinh x)
>  cosh (I x) = I (cosh x)
>  tanh (I x) = I (tanh x)
>  asinh (I x) = I (asinh x)
>  acosh (I x) = I (acosh x)
>  atanh (I x) = I (atanh x)

>instance Applicative I where
>   pure x = I x
>   (I f) <*> (I x) = I (f x)

>instance Monad I where
>   return = pure 
>   (I x) >>= f = f x

>instance MonadFail I where
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

>instance (PpShow a) => PpShow (I a) where
>	  pp (I x) = pp x
