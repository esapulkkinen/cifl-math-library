>{-# LANGUAGE Safe, LambdaCase, FlexibleInstances #-}
>module Math.Matrix.Points where
>import Math.Tools.PrettyP
>import Math.Tools.Universe

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

The remove operations are useful with inverse_image operations
to map indices.

>remove41 :: ThreeD -> FourD
>remove41 = svec3 FourD1 FourD2 FourD3

>remove42 :: ThreeD -> FourD
>remove42 = svec3 FourD0 FourD2 FourD3

>remove43 :: ThreeD -> FourD
>remove43 = svec3 FourD0 FourD1 FourD3

>remove44 :: ThreeD -> FourD
>remove44 = svec3 FourD0 FourD1 FourD2

>remove31 :: TwoD -> ThreeD
>remove31 = svec2 ThreeD1 ThreeD2
>
>remove32 :: TwoD -> ThreeD
>remove32 = svec2 ThreeD0 ThreeD2
>
>remove33 :: TwoD -> ThreeD
>remove33 = svec2 ThreeD0 ThreeD1

>remove21 :: OneD -> TwoD
>remove21 = svec1 TwoD1

>remove22 :: OneD -> TwoD
>remove22 = svec1 TwoD0

>toInt7 :: SevenD -> Int
>toInt7 SevenD0 = 0
>toInt7 SevenD1 = 1
>toInt7 SevenD2 = 2
>toInt7 SevenD3 = 3
>toInt7 SevenD4 = 4
>toInt7 SevenD5 = 5
>toInt7 SevenD6 = 6

>toInt6 :: SixD -> Int
>toInt6 SixD0 = 0
>toInt6 SixD1 = 1
>toInt6 SixD2 = 2
>toInt6 SixD3 = 3
>toInt6 SixD4 = 4
>toInt6 SixD5 = 5

>toInt5 :: FiveD -> Int
>toInt5 FiveD0 = 0
>toInt5 FiveD1 = 1
>toInt5 FiveD2 = 2
>toInt5 FiveD3 = 3
>toInt5 FiveD4 = 4

>toInt4 :: FourD -> Int
>toInt4 FourD0 = 0
>toInt4 FourD1 = 1
>toInt4 FourD2 = 2
>toInt4 FourD3 = 3
>toInt3 :: ThreeD -> Int
>toInt3 ThreeD0 = 0
>toInt3 ThreeD1 = 1
>toInt3 ThreeD2 = 2
>toInt2 :: TwoD -> Int
>toInt2 TwoD0 = 0
>toInt2 TwoD1 = 1
>toInt1 :: OneD -> Int
>toInt1 _ = 0
 

>fromInt7 :: Int -> SevenD
>fromInt7 0 = SevenD0
>fromInt7 1 = SevenD1
>fromInt7 2 = SevenD2
>fromInt7 3 = SevenD3
>fromInt7 4 = SevenD4
>fromInt7 5 = SevenD5
>fromInt7 6 = SevenD6
>fromInt7 i = fromInt7 (i `mod` 7)

>fromInt6 :: Int -> SixD
>fromInt6 0 = SixD0
>fromInt6 1 = SixD1
>fromInt6 2 = SixD2
>fromInt6 3 = SixD3
>fromInt6 4 = SixD4
>fromInt6 5 = SixD5
>fromInt6 i = fromInt6 (i `mod` 6)

>fromInt5 :: Int -> FiveD
>fromInt5 0 = FiveD0
>fromInt5 1 = FiveD1
>fromInt5 2 = FiveD2
>fromInt5 3 = FiveD3
>fromInt5 4 = FiveD4
>fromInt5 i = fromInt5 (i `mod` 5)
> 
>fromInt4 :: Int -> FourD
>fromInt4 0 = FourD0
>fromInt4 1 = FourD1
>fromInt4 2 = FourD2
>fromInt4 3 = FourD3
>fromInt4 i = fromInt4 (i `mod` 4)
> 
>fromInt3 :: Int -> ThreeD
>fromInt3 0 = ThreeD0
>fromInt3 1 = ThreeD1
>fromInt3 2 = ThreeD2
>fromInt3 i = fromInt3 (i `mod` 3)
> 
>fromInt2 :: Int -> TwoD
>fromInt2 0 = TwoD0
>fromInt2 1 = TwoD1
>fromInt2 i = fromInt2 (i `mod` 2)
> 
>fromInt1 :: Int -> OneD
>fromInt1 _ = OneD0

>-- Being careful here not to have Show (a -> b) instance, which would
>-- violate Rice's theorem when 'a' is infinite. It's not nice to use
>-- the Universe constraint on Show instances, as it made things like
>-- ((+) :: Integer -> Integer -> Integer) showable, which is scary.
>instance (Show b) => Show (OneD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (TwoD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (ThreeD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (FourD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (FiveD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (SixD -> b) where
>   show f = show [f i | i <- all_elements]
>instance (Show b) => Show (SevenD -> b) where
>   show f = show [f i | i <- all_elements]

>instance Show SevenD where { show x = show (toInt7 x) }
>instance Show SixD where { show x = show (toInt6 x) }
>instance Show FiveD where { show x = show (toInt5 x) }
>instance Show FourD where { show x = show (toInt4 x) }
>instance Show ThreeD where { show x = show (toInt3 x) }
>instance Show TwoD where { show x = show (toInt2 x) }
>instance Show OneD where { show x = show (toInt1 x) }

>instance PpShow SevenD where { pp x = pp (toInt7 x) }
>instance PpShow SixD  where { pp x = pp (toInt6 x) }
>instance PpShow FiveD where { pp x = pp (toInt5 x) }
>instance PpShow FourD where { pp x = pp (toInt4 x) }
>instance PpShow ThreeD where { pp x = pp (toInt3 x) }
>instance PpShow TwoD where { pp x = pp (toInt2 x) }
>instance PpShow OneD where { pp x = pp (toInt1 x) }

>instance Universe OneD where
>   all_elements=[OneD0]
>instance Universe TwoD where
>   all_elements=[TwoD0,TwoD1]
>instance Universe ThreeD where
>   all_elements=[ThreeD0,ThreeD1,ThreeD2]
>instance Universe FourD where
>   all_elements=[FourD0,FourD1,FourD2,FourD3]
>instance Universe FiveD where
>   all_elements=[FiveD0, FiveD1, FiveD2, FiveD3, FiveD4]
>instance Universe SixD where
>   all_elements=[SixD0, SixD1, SixD2, SixD3, SixD4, SixD5]
>instance Universe SevenD where
>   all_elements=[SevenD0, SevenD1, SevenD2, SevenD3, SevenD4, SevenD5, SevenD6]

>instance Real OneD where
>   toRational = toRational . toInt1
>instance Real TwoD where
>   toRational = toRational . toInt2
>instance Real ThreeD where
>   toRational = toRational . toInt3
>instance Real FourD where
>   toRational = toRational . toInt4
>instance Real FiveD where
>   toRational = toRational . toInt5
>instance Real SixD where
>   toRational = toRational . toInt6
>instance Real SevenD where
>   toRational = toRational . toInt7

>instance Num SevenD where
>   x + y = fromInt7 (toInt7 x + toInt7 y)
>   x - y = fromInt7 (toInt7 x - toInt7 y)
>   x * y = fromInt7 (toInt7 x * toInt7 y)
>   negate = fromInt7 . negate . toInt7
>   abs = id
>   signum SevenD0 = 0
>   signum _ = 1
>   fromInteger = fromInt7 . fromIntegral
>
>instance Num SixD where
>   x + y = fromInt6 (toInt6 x + toInt6 y)
>   x - y = fromInt6 (toInt6 x - toInt6 y)
>   x * y = fromInt6 (toInt6 x * toInt6 y)
>   negate = fromInt6 . negate . toInt6
>   abs = id
>   signum SixD0 = 0
>   signum _ = 1
>   fromInteger = fromInt6 . fromIntegral
> 
>instance Num FiveD where
>   x + y = fromInt5 (toInt5 x + toInt5 y)
>   x - y = fromInt5 (toInt5 x - toInt5 y)
>   x * y = fromInt5 (toInt5 x * toInt5 y)
>   negate = fromInt5 . negate . toInt5 
>   abs x = x
>   signum FiveD0 = 0
>   signum _ = 1
>   fromInteger = fromInt5 . fromIntegral

>instance Num FourD where
>   x + y = fromInt4 (toInt4 x + toInt4 y)
>   x - y = fromInt4 (toInt4 x - toInt4 y)
>   x * y = fromInt4 (toInt4 x * toInt4 y)
>   negate x = fromInt4 (negate (toInt4 x))
>   abs x = x
>   signum FourD0 = 0
>   signum _ = 1
>   fromInteger = fromInt4 . fromIntegral
>
>instance Integral ThreeD where
>   quot x y = fromInt3 (toInt3 x `quot` toInt3 y)
>   rem x y = fromInt3 (toInt3 x `rem` toInt3 y)
>   div x y = fromInt3 (toInt3 x `div` toInt3 y)
>   quotRem x y = let (a,b) = quotRem (toInt3 x) (toInt3 y) in (fromInt3 a,fromInt3 b)
>   divMod x y = let (a,b) = divMod (toInt3 x) (toInt3 y) in (fromInt3 a, fromInt3 b)
>   toInteger = toInteger . toInt3
>instance Integral FourD where
>   quot x y = fromInt4 (toInt4 x `quot` toInt4 y)
>   rem x y = fromInt4 (toInt4 x `rem` toInt4 y)
>   div x y = fromInt4 (toInt4 x `div` toInt4 y)
>   quotRem x y = let (a,b) = quotRem (toInt4 x) (toInt4 y) in (fromInt4 a,fromInt4 b)
>   divMod x y = let (a,b) = divMod (toInt4 x) (toInt4 y) in (fromInt4 a, fromInt4 b)
>   toInteger = toInteger . toInt4
>instance Integral FiveD where
>   quot x y = fromInt5 (toInt5 x `quot` toInt5 y)
>   rem x y = fromInt5 (toInt5 x `rem` toInt5 y)
>   div x y = fromInt5 (toInt5 x `div` toInt5 y)
>   quotRem x y = let (a,b) = quotRem (toInt5 x) (toInt5 y) in (fromInt5 a,fromInt5 b)
>   divMod x y = let (a,b) = divMod (toInt5 x) (toInt5 y) in (fromInt5 a, fromInt5 b)
>   toInteger = toInteger . toInt5
>instance Integral SixD where
>   quot x y = fromInt6 (toInt6 x `quot` toInt6 y)
>   rem x y = fromInt6 (toInt6 x `rem` toInt6 y)
>   div x y = fromInt6 (toInt6 x `div` toInt6 y)
>   quotRem x y = let (a,b) = quotRem (toInt6 x) (toInt6 y) in (fromInt6 a,fromInt6 b)
>   divMod x y = let (a,b) = divMod (toInt6 x) (toInt6 y) in (fromInt6 a, fromInt6 b)
>   toInteger = toInteger . toInt6
>instance Integral SevenD where
>   quot x y = fromInt7 (toInt7 x `quot` toInt7 y)
>   rem x y = fromInt7 (toInt7 x `rem` toInt7 y)
>   div x y = fromInt7 (toInt7 x `div` toInt7 y)
>   quotRem x y = let (a,b) = quotRem (toInt7 x) (toInt7 y) in (fromInt7 a,fromInt7 b)
>   divMod x y = let (a,b) = divMod (toInt7 x) (toInt7 y) in (fromInt7 a, fromInt7 b)
>   toInteger = toInteger . toInt7

>instance Integral OneD where
>   quot x y = fromInt1 (toInt1 x `quot` toInt1 y)
>   rem x y = fromInt1 (toInt1 x `rem` toInt1 y)
>   div x y = fromInt1 (toInt1 x `div` toInt1 y)
>   quotRem x y = let (a,b) = quotRem (toInt1 x) (toInt1 y) in (fromInt1 a,fromInt1 b)
>   divMod x y = let (a,b) = divMod (toInt1 x) (toInt1 y) in (fromInt1 a, fromInt1 b)
>   toInteger = toInteger . toInt1


>instance Integral TwoD where
>   quot x y = fromInt2 (toInt2 x `quot` toInt2 y)
>   rem x y = fromInt2 (toInt2 x `rem` toInt2 y)
>   div x y = fromInt2 (toInt2 x `div` toInt2 y)
>   quotRem x y = let (a,b) = quotRem (toInt2 x) (toInt2 y) in (fromInt2 a,fromInt2 b)
>   divMod x y = let (a,b) = divMod (toInt2 x) (toInt2 y) in (fromInt2 a, fromInt2 b)
>   toInteger = toInteger . toInt2

>   
>instance Num ThreeD where
>   x + y = fromInt3 (toInt3 x + toInt3 y)
>   x - y = fromInt3 (toInt3 x - toInt3 y)
>   x * y = fromInt3 (toInt3 x * toInt3 y)
>   negate x = fromInt3 (negate (toInt3 x))
>   abs x = x
>   signum ThreeD0 = 0
>   signum _ = 1
>   fromInteger = fromInt3 . fromIntegral
>   
>instance Num TwoD where
>   x + y = fromInt2 (toInt2 x + toInt2 y)
>   x - y = fromInt2 (toInt2 x - toInt2 y)
>   x * y = fromInt2 (toInt2 x * toInt2 y)
>   negate x = fromInt2 (negate (toInt2 x))
>   abs x = x
>   signum TwoD0 = 0
>   signum _ = 1
>   fromInteger = fromInt2 . fromIntegral
>   
>instance Num OneD where
>   _ + _ = OneD0
>   _ - _ = OneD0
>   _ * _ = OneD0
>   negate _ = OneD0
>   abs _ = OneD0
>   signum _ = OneD0
>   fromInteger _ = OneD0
