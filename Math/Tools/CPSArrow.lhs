>{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses, Arrows #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.CPSArrow where
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow

Continuation passing arrow transformer:

>newtype CPSArrowT a b c = CPSArrowT { 
>     runCPSArrowT :: forall ans. (a c ans) -> (a b ans)
>   }

>liftCPSArrowT :: (Arrow a) => a b c -> CPSArrowT a b c
>liftCPSArrowT f = CPSArrowT (\k -> f >>> k)

>unliftCPSArrowT :: (Arrow a) => CPSArrowT a b c -> a b c
>unliftCPSArrowT f = f `runCPSArrowT` returnA

>instance (FunctorArrow f a, ArrowApply a) => FunctorArrow f (CPSArrowT a) where
>   amap (CPSArrowT f) = CPSArrowT (\cont -> amap (f returnA) >>> cont)

>instance (ArrowChoice a, ArrowApply a) => ArrowChoice (CPSArrowT a) where
>   left (CPSArrowT f) = CPSArrowT (\k -> (f (arr Left >>> k)) ||| (arr Right >>> k)  )
>   right (CPSArrowT f) = CPSArrowT (\k -> (arr Left >>> k) ||| (f (arr Right >>> k)))
>   (CPSArrowT f) +++ (CPSArrowT g) = CPSArrowT (\k -> f (arr Left >>> k) ||| g (arr Right >>> k))
>   (CPSArrowT f) ||| (CPSArrowT g) = CPSArrowT (\k -> f k ||| g k)

>instance Category (CPSArrowT a) where
>       id = CPSArrowT id
>	(CPSArrowT g) . (CPSArrowT f) = CPSArrowT (f . g)


>instance (ArrowApply a) => Arrow (CPSArrowT a) where
>	  arr f = liftCPSArrowT (arr f)
>	  first (CPSArrowT f) = CPSArrowT (\k -> arr (\ (b,d) -> 
>		(f (arr (\c -> (c,d)) >>> k),b)) >>> app)

>instance (ArrowApply a) => ArrowApply (CPSArrowT a) where
>   app = CPSArrowT (\k -> proc (CPSArrowT f,x) -> do f k -<< x)


>callccCPSArrowT :: (Arrow a) => 
>                   (forall ans. CPSArrowT a c ans -> CPSArrowT a b c)
>                                                  -> CPSArrowT a b c
>callccCPSArrowT f = CPSArrowT (\k -> f (CPSArrowT (\r -> k >>> r)) 
>                                     `runCPSArrowT` k)

>instance (ArrowApply a) => CPSArrow (CPSArrowT a) where
>   callcc = callccCPSArrowT

