>{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
>{-# OPTIONS_HADDOCK hide #-}
>module Math.Tools.StateArrow where
>import Prelude hiding (id,(.))
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow

STArrow is based on the state functor from John Hughes' "Generalising
monads to Arrows" paper.

type STArrow s b c = Kleisli (ST s) b c

>newtype STArrow s a b c = SF { runSTArrow :: a (b,s) (c,s) }

>liftState :: Arrow a => a b c -> STArrow s a b c
>liftState f = SF (first f)

>class StateAspect sa s where
>      fetch :: sa b s
>      store :: sa s ()

>instance Arrow a => StateAspect (STArrow s a) s where
>	  fetch = SF (arr (\ (_,s) -> (s,s)))
>	  store = SF (arr (\ (x,_) -> ((),x)))

>instance (Arrow a) => Category (STArrow s a) where
>   id = liftState id
>   SF f . SF g = SF (f . g)

>instance Arrow a => Arrow (STArrow s a) where
>       arr f = liftState (arr f)
>       first (SF f) = SF (arr (\ ((b,d),s) -> ((b,s),d)) >>> first f
>                         >>> arr (\ ((c,s),d) -> ((c,d),s)))

fetch :: Arrow a => STArrow s a b s
fetch = SF (arr (\ (_,s) -> (s,s)))

store :: Arrow a => STArrow s a s ()
store = SF (arr (\ (x,s) -> ((),x)))

>instance ArrowChoice a => ArrowChoice (STArrow s a) where
>	  left (SF f) = SF ( arr (\ (x,s) -> case x of
>                              Left b -> Left (b,s)
>			       Right c -> Right (c,s)) >>>
>			((f >>> first (arr Left)) ||| first (arr Right)))

>instance ArrowZero a => ArrowZero (STArrow s a) where
>	  zeroArrow = SF zeroArrow

>instance ArrowPlus a => ArrowPlus (STArrow s a) where
>	  (SF f) <+> (SF g) = SF (f Control.Arrow.<+> g)

>instance ArrowApply a => ArrowApply (STArrow s a) where
>	  app = SF (arr (\ ((SF f,b),s) -> (f,(b,s))) >>> app)
