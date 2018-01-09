>{-# LANGUAGE TypeOperators #-}
>module Math.Tools.Equation where
>import Control.Applicative
>import Math.Tools.Functor
>import Math.Tools.Arrow
>import Math.Tools.Isomorphism

>data Equation a = Equal { equation_left :: a, equation_right :: a }

>isoEqual :: a :==: b -> Equation a :==: Equation b
>isoEqual iso =  (\ (Equal x y) -> Equal (iso =< x) (iso =< y))
>            <-> (\ (Equal x y) -> Equal (invertA iso =< x) (invertA iso =< y))

>instance (Show a) => Show (Equation a) where
>   show (x `Equal` y) = show x ++ " == " ++ show y

>instance Functor Equation where
>   fmap f (x `Equal` y) = (f x) `Equal` (f y)

>instance Applicative Equation where
>   pure = reflexivity
>   (f `Equal` g) <*> (x `Equal` y) = (f x) `Equal` (g y)

>instance Monad Equation where
>   return = reflexivity
>   (x `Equal` y) >>= f = (equation_left  $ f x) `Equal` (equation_right $ f y)

>reflexivity :: a -> Equation a
>reflexivity x = x `Equal` x

>transitivity :: (Eq a, Monad m) => Equation a -> Equation a -> m (Equation a)
>transitivity (a `Equal` b) (b' `Equal` c)
>   | b == b'   = return (a `Equal` c)
>   | otherwise = fail "Equations are not part of transitive chain"
>
>symmetry :: Equation a -> Equation a
>symmetry (a `Equal` b) = b `Equal` a

>embedEquation :: (Applicative f) => Equation (f a) -> f (Equation a)
>embedEquation (x `Equal` y) = liftA2 Equal x y
>
>unembedEquation :: (Functor f) => f (Equation a) -> Equation (f a)
>unembedEquation eqt = a `Equal` b
>   where (a,b) = funzip $ fmap (\ (x `Equal` y) -> (x,y)) eqt
