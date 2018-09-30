>{-# LANGUAGE TypeOperators #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Action where
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow
>import Math.Tools.CoFunctor
>import Math.Tools.NaturalTransformation

>data LAct x a b = LAct { runLAct :: (x -> a) -> x -> b }
>data RAct x a b = RAct { runRAct :: (a -> x) -> b -> x }
>data Op x a  = Op { runOp :: a -> x }
>data LeftASets arr a b = LeftASets { runLeftASet :: arr a :~> arr b }

>instance Category (LeftASets arr) where
>  id = LeftASets (NatTrans id)
>  (LeftASets f) . (LeftASets g) = LeftASets (f `vert` g)

>instance Category (LAct x) where
>  id = LAct id
>  (LAct f) . (LAct g) = LAct $ \h -> f (g h)

>instance Category (RAct x) where
>  id = RAct id
>  (RAct f) . (RAct g) = RAct $ \h -> f (g h)

>instance Arrow (LAct x) where
>  arr f = LAct $ \a -> f . a
>  first (LAct f) = LAct $ \g x -> (f (fst . g) x, snd (g x))
  
>natural :: (x -> y) -> ((->) y) :~> ((->) x)
>natural f = NatTrans (\ g -> g . f)

>smooth :: (x -> y) -> (Op x) :~> (Op y)
>smooth f = NatTrans (\ (Op ax) -> Op (f . ax))

>coarr :: (b -> a) -> RAct x a b
>coarr f = RAct $ \g -> g . f

>instance CoFunctor (Op x) where
>   inverse_image f (Op g) = Op (g . f)
