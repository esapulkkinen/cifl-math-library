>{-# LANGUAGE TypeOperators, FlexibleContexts #-}
>module Math.Number.Derivate where
>import Math.Tools.Arrow
>import Math.Tools.Isomorphism
>import Math.Matrix.Interface
>import Data.Monoid
>import Data.Foldable
>import Math.Number.R hiding (derivate)

>derivate_gen :: (Fractional b) => Endo (Endo c -> a -> b)
>derivate_gen = Endo $ \ d f x -> d f x / d (Endo id) x

>real_delta :: Endo Rational -> Rational -> R
>real_delta (Endo f) x = real $ \eps -> f (x + eps) - f (x - eps)

>real2_delta :: Endo Rational -> R -> R
>real2_delta (Endo f) (Limit x) = real $ \eps -> let x' = x `appEndo` eps
>                                  in f (x' + eps) - f (x' - eps)
>
>real1_delta f (x,y) = real $ \eps -> f (x + eps) y - f (x - eps) y
> 

>deriv :: Endo Rational -> Rational -> R
>deriv = derivate_gen `appEndo` real_delta

>deriv_real :: Endo Rational -> Endo R
>deriv_real f = Endo $ (derivate_gen `appEndo` real2_delta) f

deriv1 f x = derivate_gen real1_delta f x


>derivate (f,g) (x1,x2) = (f x1 - f x2) / (g x1 - g x2)

>derivate_real f (Limit x) = real $ \eps -> derivate (f,(+eps)) (x `appEndo` eps,x `appEndo` eps + eps)



>integrate :: (Num b, Ord a) => a :==: a -> (a,a) -> (a -> b) -> b
>integrate (Iso d dinv) (x,y) f = sum $ map f $ takeWhile (< y) $ iterate d x
