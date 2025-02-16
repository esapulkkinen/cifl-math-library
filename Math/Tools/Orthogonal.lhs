>{-# LANGUAGE Safe #-}
>module Math.Tools.Orthogonal where 
>import Control.Applicative
>import Control.Monad (join)
>import Math.Tools.Functor

>newFunction :: (Functor f) => (a -> f a) -> (f c -> c) -> a -> c
>newFunction dtr ctr = ctr . fmap (newFunction dtr ctr) . dtr

>foldFunc :: (Functor f) => (a -> f a) -> (f c -> f c -> c) -> a -> a -> c
>foldFunc f g x y = newFunction f (\a -> newFunction f (g a) y) x


cross_function :: (Functor f) => (c -> a -> f a) -> (f c -> c) -> ((a -> c1) -> c1) -> c -> c1

cross_function :: (Functor f) => (a -> a1 -> f a1) -> ((a1 -> c) -> c) -> (f a -> a) -> a -> c
cross_function f x y = new_function (\a -> new_function (f a) y) x

cfunc
cfunction f x y = (new_function y . f) `new_function` x

cffunc f y x = new_function (\a -> new_function (f a) y) x
dfunction f x y = x `fbind` \a -> y `fbind` (f a)

>fbind :: (Functor f) => (f c -> c) -> (a -> f a) -> a -> c
>fbind = flip newFunction

----

>outer'' f x y = fmap (\a -> fmap (f a) y) x

>functorOuter :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
>functorOuter f x y = x `mapf` \a -> y `mapf` \b -> f a b

>outerLeft :: (Functor f, Functor g) => f (a -> b) -> g a -> f (g b)
>outerLeft = functorOuter id

>outerRight :: (Functor f, Functor g) => f a -> g (a -> b) -> f (g b)
>outerRight = functorOuter (flip id)

>listDotproduct :: (Num a) => [a] -> [a] -> a
>listDotproduct x y = sum $ liftA2 (*) x y  

  sum == foldl (+) 0
  (*) == \x y -> fold 0 (+ x) y

----

>reduce2 :: (Applicative f) => (f a -> a) -> f (f a) -> a
>reduce2 f = (pure f <*>) (pure f <*>)

>outer' :: (Functor f) => (a -> f b -> b) -> f a -> f (f b)
>outer' f x = x `mapf` \a -> outer' f x `mapf` \b -> f a b

>fullOuter :: (Functor f, Functor g) => f a -> (a -> g b) -> (a -> b -> c) -> f (g c)
>fullOuter x f1 f2 = x `mapf` \a -> f1 a `mapf` \b -> f2 a b

>self :: (Functor f) => (a -> a -> b) -> f a -> f (f b)
>self f x = functorOuter f x x

>mapf :: (Functor f) => f a -> (a -> b) -> f b
>mapf = flip fmap

>outerProduct :: (Functor f, Functor g) => f a -> g b -> f (g (a,b))
>outerProduct = functorOuter (,)

>outerApply :: (Functor f, Functor g) => f (b -> c) -> g b -> f (g c)
>outerApply = functorOuter id

>(|**|) :: (Functor f, Monad f) => f a -> f b -> f (a,b)
>x |**| y = join $ functorOuter (,) x y


>outer3Functor :: (Functor f, Functor g, Functor h)
>	=> (a -> b -> c -> d) -> f a -> g b -> h c -> f (g (h d))
>outer3Functor f x x' y = functorOuter (\a b -> fmap (f a b) y) x x'



>outer4Functor :: (Functor f, Functor g, Functor h, Functor i) =>
>	(a -> b -> c -> d -> e) -> f a -> g b -> h c -> i d -> f (g (h (i e)))
>outer4Functor f x x' y y' = functorOuter (\a b -> functorOuter (f a b) y y') x x'

data LayerSeq f a = LayerSeq a (f (LayerSeq f a))

instance (Monad f, Functor f) => Monad (LayerSeq f) where
	  return x = LayerSeq x (fmap return (return x))
	  (LayerSeq x f) >>= g = let LayerSeq x r = g x in 
		    LayerSeq (g x) 

data Pullback f g a where
     Pullback :: ((a,b) -> (b,c) -> Maybe (a,c))


pullback :: f (a,b) -> g (b,c) -> Pullback f g (a,b,c)

