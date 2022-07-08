>{-# LANGUAGE LambdaCase #-}
>module Math.Matrix.Points where

>data OneD = OneD0
>  deriving (Eq,Ord,Enum)
>data TwoD = TwoD0 | TwoD1
>  deriving (Eq,Ord,Enum)
>data ThreeD = ThreeD0 | ThreeD1 | ThreeD2
>  deriving (Eq,Ord,Enum)
>data FourD = FourD0 | FourD1 | FourD2 | FourD3
>  deriving (Eq,Ord,Enum)
>data FiveD = FiveD0 | FiveD1 | FiveD2 | FiveD3 | FiveD4
>  deriving (Eq,Ord,Enum)
>data SixD = SixD0 | SixD1 | SixD2 | SixD3 | SixD4 | SixD5
>  deriving (Eq,Ord, Enum)
>data SevenD = SevenD0 | SevenD1 | SevenD2 | SevenD3 | SevenD4 | SevenD5 | SevenD6
>  deriving (Eq,Ord,Enum)

>svec1 :: a -> OneD -> a
>svec1 x = \case { OneD0 -> x }

>svec2 :: a -> a -> TwoD -> a
>svec2 x y = \case { TwoD0 -> x ; TwoD1 -> y }

>svec3 :: a -> a -> a -> ThreeD -> a
>svec3 x y z = \case { ThreeD0 -> x; ThreeD1 -> y ; ThreeD2 -> z }

>svec4 :: a -> a -> a -> a -> FourD -> a
>svec4 x y z t = \case { FourD0 -> x; FourD1 -> y ; FourD2 -> z ; FourD3 -> t }
>
>svec5 :: a -> a -> a -> a -> a -> FiveD -> a
>svec5 x y z t u = \case { FiveD0 -> x ; FiveD1 -> y ; FiveD2 -> z ;
>                          FiveD3 -> t ; FiveD4 -> u }
>svec6 :: a -> a -> a -> a -> a -> a -> SixD -> a
>svec6 x y z t u w = \case { SixD0 -> x ; SixD1 -> y ; SixD2 -> z ;
>                          SixD3 -> t ; SixD4 -> u ; SixD5 -> w }
>svec7 :: a -> a -> a -> a -> a -> a -> a -> SevenD -> a
>svec7 x y z t u w ww = \case { SevenD0 -> x ; SevenD1 -> y ; SevenD2 -> z ;
>                          SevenD3 -> t ; SevenD4 -> u ; SevenD5 -> w ;
>                          SevenD6 -> ww }
