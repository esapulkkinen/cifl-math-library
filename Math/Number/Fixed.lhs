>{-# LANGUAGE DataKinds, KindSignatures, GADTs, FlexibleInstances #-}
>{-# LANGUAGE ScopedTypeVariables, TypeOperators, TypeFamilies #-}
>module Math.Number.Fixed where
>-- ^ Fixed point numeric type

>import Data.Kind
>import GHC.TypeLits
>import Data.Proxy
>import Math.Number.Interface
>import Data.Ratio

>data Fixed :: Nat -> Nat -> * -> * where
>  Fixed :: a -> Proxy multiplier -> Proxy divisor -> Fixed multiplier divisor a

>unit_proxy :: Proxy 1
>unit_proxy = Proxy :: Proxy 1

>type Kilo a = Fixed 1000 1 a
>type Mega a = Fixed 1000000 1 a
>type Giga a = Fixed 1000000000 1 a
>type Centi a = Fixed 1 100 a
>type Milli a = Fixed 1 1000 a
>type Micro a = Fixed 1 1000000 a
>type Nano a  = Fixed 1 1000000000 a

>type family ReduceFixed a
>type instance ReduceFixed (Fixed fmult fdiv a) = Fixed (Div fmult (GCD fmult fdiv)) (Div fdiv (GCD fmult fdiv)) a

>fixed_kilo :: a -> Kilo a
>fixed_kilo a = Fixed a (Proxy :: Proxy 1000) unit_proxy
>
>fixed_mega :: a -> Mega a
>fixed_mega a = Fixed a (Proxy :: Proxy 1000000) unit_proxy
>
>fixed_giga :: a -> Giga a
>fixed_giga a = Fixed a (Proxy :: Proxy 1000000000) unit_proxy

>fixed_centi :: a -> Centi a
>fixed_centi a = Fixed a unit_proxy (Proxy :: Proxy 100)

>fixed_milli :: a -> Milli a
>fixed_milli a = Fixed a unit_proxy (Proxy :: Proxy 1000)
>
>fixed_micro :: a -> Micro a
>fixed_micro a = Fixed a unit_proxy (Proxy :: Proxy 1000000)
>
>fixed_nano :: a -> Nano a
>fixed_nano a = Fixed a unit_proxy (Proxy :: Proxy 1000000000)

>join_fixed :: Fixed mult1 div1 (Fixed mult2 div2 a)
> -> ReduceFixed (Fixed (mult1 GHC.TypeLits.* mult2) (div1 GHC.TypeLits.* div2) a)
>join_fixed (Fixed (Fixed a (Proxy :: Proxy m2t) (Proxy :: Proxy d2t))
>    (Proxy :: Proxy m1t) (Proxy :: Proxy d1t)) =
> Fixed a (Proxy :: Proxy (Div (m1t GHC.TypeLits.* m2t) (d1t GHC.TypeLits.* d2t)))
>         (Proxy :: Proxy (Mod (m1t GHC.TypeLits.* m2t) (d1t GHC.TypeLits.* d2t)))

>fromFixed :: (Integral a, KnownNat mult, KnownNat divisor) => Fixed mult divisor a -> Ratio a
>fromFixed (Fixed t mult d) = ((t * fromInteger (natVal mult)) % fromInteger (natVal d))

>instance (KnownNat mult, KnownNat divisor, Integral a, Show a) => Show (Fixed mult divisor a) where
>  show f = show (fromFixed f)
