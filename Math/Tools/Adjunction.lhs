>{-# LANGUAGE Trustworthy,MultiParamTypeClasses,FunctionalDependencies #-}
>module Math.Tools.Adjunction where

import Control.Monad.Reader hiding (fix) -- instances of Functor ((->) a) and Monad ((->) a)

>import Control.Arrow ((&&&),(|||))
>import Control.Applicative

>-- | The problem with this type class is that categories are not represented
>-- at all. This assumes all functors are F : C -> C.
>-- This is a standard definition of adjunction from category theory.
>class (Functor f, Functor g) =>
>		Adjunction f g | f -> g, g -> f where
>      leftAdjunct :: (f a -> b) -> a -> g b
>      unit :: a -> g (f a) 
>      rightAdjunct :: (a -> g b) -> f a -> b
>      counit :: f (g b) -> b 
>      -- minimum required impl: unit xor leftAdjunct
>      -- minimum required impl: counit xor rightAdjunct
>      unit = leftAdjunct id
>      leftAdjunct f = fmap f . unit
>      counit = rightAdjunct id
>      rightAdjunct g = counit . fmap g

>      -- rule : fmap (counit x) o unit (fmap f) == id

>embed_adjunction :: (Adjunction f g, Adjunction g f) => (f a -> f b) -> a -> b
>embed_adjunction f x = rightAdjunct f (unit x)

>-- | zipR and cozipL are from Edward Kmett's Adjunction package
>-- <http://hackage.haskell.org/packages/archive/adjunctions/0.9.0.4/doc/html/src/Data-Functor-Adjunction.html>

>zipR :: (Adjunction f u) => (u a, u b) -> u (a,b)
>zipR = leftAdjunct (rightAdjunct fst &&& rightAdjunct snd)

>-- | zipR and cozipL are from Edward Kmett's Adjunction package
>-- <http://hackage.haskell.org/packages/archive/adjunctions/0.9.0.4/doc/html/src/Data-Functor-Adjunction.html>

>cozipL :: (Adjunction f u) => f (Either a b) -> Either (f a) (f b)
>cozipL = rightAdjunct (leftAdjunct Left ||| leftAdjunct Right)

>swap :: (b,a) -> (a,b)
>swap = rightAdjunct (,)

>instance Adjunction ((,) a) ((->) a) where
>	  unit t = \arg -> (arg,t)
>	  counit (x,f) = f x

>data AdjM f g a = AdjM { runAdjM :: g (f a) }

>instance (Functor f, Functor g) => Functor (AdjM f g) where
>   fmap f (AdjM m) = AdjM $ fmap (fmap f) m
>
>instance (Applicative f, Applicative g) => Applicative (AdjM f g) where
>   pure x = AdjM $ pure (pure x)
>   (AdjM f) <*> (AdjM x) = AdjM $ liftA2 (<*>) f x

>instance (Adjunction f g, Applicative f, Applicative g) => Monad (AdjM f g) where
>   return = AdjM . unit
>   (AdjM m) >>= g = AdjM $ fmap (counit . fmap (runAdjM . g)) m
