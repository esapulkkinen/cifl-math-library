>{-# LANGUAGE Arrows, MultiParamTypeClasses #-}
>module Math.Tools.Assert where
>import Control.Arrow
>import Math.Tools.Arrow

>data Assertion arr a b = AEqual (arr a b)
>                       | ATerminates 
>                       | ANot (Assertion arr a b)
>                       | AAnd (Assertion arr a b) (Assertion arr a b)
>                       | AOr  (Assertion arr a b) (Assertion arr a b)
>                       | AImply (Assertion arr a b) (Assertion arr a b)
>                       | AForall (arr a (Assertion arr a b))
>                       | AExist  (arr a (Assertion arr a b))

>class (Arrow arr) => UniverseA arr a where
>    allA :: arr () [a]

>eval :: (UniverseA arr a, ArrowChoice arr, ArrowApply arr, Eq b) => Assertion arr a b -> arr (arr a b) Bool
>eval (AEqual e1) = proc e2 -> do lst <- allA -< ()
>                                 lst1 <- amap e1 -< lst
>                                 lst2 <- amap e2 -<< lst
>                                 returnA -< and $ zipWith (==) lst1 lst2
>eval ATerminates = proc f -> do v <- allA -< ()
>                                _ <- amap f -<< v
>                                returnA -< True
>eval (ANot f) = eval f >>> arr not
>eval (AAnd f g) = (eval f &&& eval g) >>> arr (uncurry (&&))
>eval (AOr f g)  = (eval f &&& eval g) >>> arr (uncurry (||))
>eval (AImply f g) = (eval f &&& eval g) >>> arr (\ (v1,v2) -> not v1 || v2)
>eval (AForall e) = proc f -> do
>                       lst <- allA -< ()
>                       let p = proc v -> do v' <- e -< v
>                                            eval v' -<< f
>                       lst2 <- amap p -<< lst
>                       returnA -< and lst2
>eval (AExist e) = proc f -> do
>                       lst <- allA -< ()
>                       let p = proc v -> do v' <- e -< v
>                                            eval v' -<< f
>                       lst2 <- amap p -<< lst
>                       returnA -< or lst2


