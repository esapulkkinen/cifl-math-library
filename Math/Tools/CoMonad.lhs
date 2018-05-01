>{-# LANGUAGE Safe #-}
>module Math.Tools.CoMonad where
>import Prelude hiding ((.),id)
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow
>import Math.Tools.Adjunction

>class (Functor w) => Coapplicative w where
>   coeval   :: w a -> a
>   colambda :: (w a -> w b) -> w (a -> b)

>-- | From youtube video "Category theory II 7.2: Comonads categorically and examples"
>-- by Bartosz Milewski, comonad has a "focal point" defined by 'extract' operation,
>-- and a mechanism for applying a cokleisli arrow to the whole environment
>-- [e.g. if you give average to 'extend', you'd smooth the whole data structure].

>class (Functor w) => Comonad w where
>      extract :: w a -> a 
>      duplicate :: w a -> w (w a)
>      extend :: (w a -> b) -> (w a -> w b)
>      (=>>) :: w a -> (w a -> b) -> w b 
>      (.>>) :: w a -> b -> w b 
>      x .>> v = x =>> (\y -> extract y `seq` v)
>      (=>>) = flip extend
>      duplicate = extend id
>      extend f = fmap f . duplicate

>class (Comonad w) => CircularComonad w where
>   rotate :: w a -> w a

>class (CircularComonad w) => FiniteComonad w where
>   inverseRotate :: w a -> w a

>class (Comonad w) => BinaryTreeComonad w where
>   nextLevel :: w a -> (w a, w a)

>class (Comonad w) => TernaryTreeComonad w where
>   nextLevel3 :: w a -> (w a, w a, w a)

>class (Comonad w) => ChainComonad w where
>   coMonadChain :: w a -> Maybe (w a)

>class (Comonad w) => TreeComonad w where
>   children :: w a -> [w a]

>class (Comonad w) => DenseComonad w where
>   split :: w a -> w (a,a)

>class (Comonad w) => NonemptyComonad w where
>   nonzero :: w a -> Either a (w a)

>class (CircularComonad w) => InfiniteComonad w where
>   pre :: a -> w a -> w a

>class UncertainComonad w where
>   perhaps :: w a -> w (Maybe a)


>preList :: (InfiniteComonad w, Foldable t) => t a -> w a -> w a
>preList = flip (foldr pre)

>indexComonad :: (InfiniteComonad w) => Integer -> w a -> a
>indexComonad 0 = extract
>indexComonad i = indexComonad (i-1) . rotate

>permute :: (Comonad w) => w a -> w a
>permute = extract . duplicate

>wmap :: Comonad w => (a -> b) -> w a -> w b
>wmap f = (=>> (f . extract))

>class Reference w where
>      dereference   :: w a -> a
>      invoke        :: w a -> (w a -> b) -> w b
>      new_reference :: w a -> b -> w b

>class Unique w where
>      copy :: a -> (w a, a)


>class Continuation m where
>      escape :: ((a -> m b) -> m a) -> m a



>data CoState s a = CoState (s -> a) s


>instance Functor (CoState s) where
>	  fmap g (CoState f x) = CoState (g . f) x
>
>instance Comonad (CoState s) where
>	  extract (CoState g x) = g x
>	  duplicate (CoState f c) = CoState (CoState f) c

	  (CoState g s) =>> f = CoState (\s' -> f (CoState g s')) s
	  (CoState g s) .>> b = CoState (\s' -> seq (g s') b) s

>instance Comonad ((,) a) where
>	  extract (c,x) = x
>	  duplicate (c,x) = (c,(c,x))
>         extend f z@(a,b) = (a,f z)

>newtype CoKleisli w a b = CoKleisli { unCoKleisli :: w a -> b }

>instance Functor (CoKleisli w a) where
>	  fmap g (CoKleisli f) = CoKleisli (g . f)

>instance (Comonad w) => Category (CoKleisli w) where
>         id = CoKleisli extract
>	  (CoKleisli b) . (CoKleisli a) = CoKleisli (b . fmap a . duplicate)


>instance (Comonad w) => Arrow (CoKleisli w) where
>	  arr f = CoKleisli (f . extract)
>	  (CoKleisli a) &&& (CoKleisli b) = CoKleisli (a &&& b)
>	  (CoKleisli a) *** (CoKleisli b) = CoKleisli (a . fmap fst &&& b . fmap snd)
>	  first a = a *** arr id
>	  second a = arr id *** a

putTwoChars :: OI (Char, Char) -> ()
putTwoChars ctx = ctx .>> stdPutChar (fst $ coeval x)
putTwoChars ctx = let (a,b) = eval ctx in  ctx .>> coPutChar a stdOI .>> coPutChar b stdOI


>data Neg o a = Neg ((a -> o) -> o)

>instance Functor (Neg o) where
>  fmap f (Neg h) = Neg $ \c -> h (\a -> c (f a))

>instance Applicative (Neg o) where
>  pure x = Neg (\c -> c x)
>  (Neg f) <*> (Neg x) = Neg $ \c -> f (\f' -> x (\x' -> c (f' x')))

c (f id (x id))

>instance Monad (Neg o) where
>	  return x = Neg (\c -> c x)
>	  (Neg x) >>= f = Neg (\c -> x (\v -> let Neg g = f v in g c))

>instance Continuation (Neg o) where
>	  escape h = Neg (\c -> let Neg g = h (\v -> Neg (\c' -> c v)) in g c)

>evalC :: Neg o o -> o
>evalC (Neg f) = f id

>-- | shift and reset are from Wadler: Monads and composable continuations 
>-- <http://citeseer.nj.nec.com/wadler93monads.html>

>shiftC :: ((a -> Neg o o) -> Neg o o) -> Neg o a
>shiftC h = Neg (\c -> let Neg g = h (\v -> Neg (\c' -> c' (c v))) in g id)

>-- | <http://citeseer.nj.nec.com/wadler93monads.html>
>resetC :: Neg o o -> Neg o o
>resetC (Neg m) = Neg (\c -> c (m id))

>liftC :: (a -> o) -> Neg o a -> Neg o o -- is this useful? variation on resetC.
>liftC f (Neg m) = Neg (\c -> c (m f))




             g :: a -> o
             f :: w a -> b
             w :: w a
    w =>> f :: w a -> (w a -> b) -> w b



data STObject o w = NewST { this_st :: IORef o, input_st :: IO w }

instance Comonad (STObject o) where
	  extract (NewST _ x) = unsafePerformIO x
	  z@(NewST this i) =>> method = NewST this 
		 (return (method $! z))

data Object o w = New { this :: o, input :: w }

instance Comonad (Object o) where
	  extract (New _ x) = x
	  (New s i) =>> method = New s (method (New s i))

instance Functor (Object o) where
	  fmap = wmap

o1 :: Object Int ()
o1 = New 10 ()

new :: o -> Object o ()
new s = New s ()

class Comonad w => Private s w where
      use :: w a -> w s
      set :: w (s,a) -> w a

instance Private Object where
	  use (New o _) = New o o

type Method a b = Comonad w => w a -> b


data CoObject o w = L o | R w

instance Monad (CoObject o) where
	  return x = R x
	  (L o) >>= f = L o
	  (R p) >>= f = f p

instance Functor (CoObject o) where
	  fmap f w = w >>= (return . f)


data IORep m w a i o = IORep (a (w i) (m o))

instance (Monad m, Comonad w, Arrow a) => Arrow (IORep m w a) where
	  arr f = IORep (arr (\c -> return $ f $ extract c))
	  (IORep x) >>> (IORep y) = IORep (\c -> x c >>= \v ->
						 y (c =>> (\_ -> v)))
	  first (IORep x) = IORep (arr (\c -> let (a,b) = extract c
					  in x a >>= \v -> return (v,b)))
