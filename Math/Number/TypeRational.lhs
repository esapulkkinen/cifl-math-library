>{-# LANGUAGE CPP #-}
>{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, GADTs, TypeFamilies #-}
>{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, PolyKinds #-}
>{-# LANGUAGE FlexibleInstances, TypeFamilyDependencies #-}
>module Math.Number.TypeRational where
>import Data.Kind
>import Math.Number.Stream

>data SNat = Ze | Su SNat | SNegate SNat

>data SBin = SBZe | SBOne SBin | SBZero SBin 

>data Sing :: SNat -> * where
>  SNatZ :: Sing 'Ze
>  SNatS :: Sing s -> Sing ('Su s)

>type family Half (n :: SNat) = s | s -> n where
>   Half 'Ze = 'Ze
>   Half ('Su ('Su z)) = 'Su (Half z)

>instance SingInteger (Sing 'Ze) where { singToInteger SNatZ = 0 }
>instance (SingInteger (Sing x)) => SingInteger (Sing ('Su x)) where { singToInteger (SNatS s) = succ (singToInteger s) }

>type family SPlus (n :: SNat) (n' :: SNat) where
>   SPlus 'Ze 'Ze = 'Ze
>   SPlus 'Ze a   = a
>   SPlus ('Su x) a = 'Su (SPlus x a)
>   SPlus a ('SNegate b) = SMinus a b
>   SPlus ('SNegate a) b = SMinus b a

>type family SMinus (n :: SNat) (n' :: SNat) where
>   SMinus 'Ze 'Ze = 'Ze
>   SMinus d 'Ze = d
>   SMinus 'Ze d = 'SNegate d
>   SMinus ('Su d) ('Su d') = SMinus d d'
>   SMinus a ('SNegate b) = SPlus a b
>   SMinus ('SNegate a) b = 'SNegate (SPlus a b)


>class SingInteger n where
>   singToInteger :: n -> Integer

>class SingRational r where
>   singToRational :: r -> Rational

>class SingPrime (n :: Prime) where
>   singToPrimeIndex :: PrimeSing n -> Integer

>data PrimeSing :: Prime -> * where
>  SPrTwo :: PrimeSing 'PrTwo
>  SPrNext :: PrimeSing n -> PrimeSing ('NextPrimeAfter n)

>instance SingPrime 'PrTwo where { singToPrimeIndex SPrTwo = 1 }
>instance (SingPrime p) => SingPrime ('NextPrimeAfter p) where
>   singToPrimeIndex (SPrNext p) = succ $ singToPrimeIndex p

>instance (SingPrime n) => SingInteger (PrimeSing n) where
>   singToInteger x = streamindex (singToPrimeIndex x) primes

>data PrimePowerSing :: PrimePower -> * where
>  SPrimePower :: PrimeSing p -> Sing n -> PrimePowerSing ('PrimePower p n)

>instance (SingInteger (Sing n), SingPrime p) => SingInteger (PrimePowerSing ('PrimePower p n)) where
>   singToInteger (SPrimePower p n) = singToInteger p ^ singToInteger n

>data RatSing :: SRat -> * where
>  SRatOne :: RatSing 'PrOne
>  SRatTimes :: PrimePowerSing p -> RatSing r -> RatSing ('PrTimes p r)

>data SRat  = PrOne | PrTimes PrimePower SRat
>data PrimePower = PrimePower Prime SNat
>data Prime = PrTwo | NextPrimeAfter Prime

>type family InvertPrime (a :: SRat) = (c :: SRat) | c -> a where
>   InvertPrime 'PrOne = 'PrOne
>   InvertPrime ('PrTimes ('PrimePower p n) r) = 'PrTimes ('PrimePower p ('SNegate n))
>                                                         (InvertPrime r)

>type family ShiftPrime (a :: SRat) = (c :: SRat) | c -> a where
>   ShiftPrime 'PrOne = 'PrTimes PPTwo 'PrOne
>   ShiftPrime ('PrTimes ('PrimePower p n) r) = 'PrTimes ('PrimePower ('NextPrimeAfter p) n) r

>type family RatMul (a :: SRat) (b :: SRat) :: SRat where
>   RatMul ('PrTimes ('PrimePower 'PrTwo n) r)
>                ('PrTimes ('PrimePower 'PrTwo m) r')
>        = 'PrTimes ('PrimePower 'PrTwo (SPlus n m)) (RatMul r r')
>   RatMul ('PrTimes ('PrimePower ('NextPrimeAfter p) n) r)
>                ('PrTimes ('PrimePower ('NextPrimeAfter q) n') r')
>        = ShiftPrime (RatMul ('PrTimes ('PrimePower p n) r)
>                                   ('PrTimes ('PrimePower q n') r'))
>   RatMul ('PrTimes ('PrimePower 'PrTwo n) r)
>                ('PrTimes ('PrimePower ('NextPrimeAfter p) m) r')
>      = 'PrTimes ('PrimePower 'PrTwo n) (RatMul r ('PrTimes ('PrimePower ('NextPrimeAfter p) m) r'))
>   RatMul ('PrTimes ('PrimePower ('NextPrimeAfter p) n) r)
>                ('PrTimes ('PrimePower 'PrTwo m) r')
>      = 'PrTimes ('PrimePower 'PrTwo m) (RatMul ('PrTimes ('PrimePower ('NextPrimeAfter p) n) r) r')
>   RatMul 'PrOne ('PrTimes a b) = 'PrTimes a b
>   RatMul ('PrTimes a b) 'PrOne = 'PrTimes a b
>   RatMul 'PrOne 'PrOne = 'PrOne

>type family RatDiv (a :: SRat) (b :: SRat) :: SRat where
>   RatDiv ('PrTimes ('PrimePower 'PrTwo n) r)
>          ('PrTimes ('PrimePower 'PrTwo m) r')
>        = 'PrTimes ('PrimePower 'PrTwo (SMinus n m)) (RatDiv r r')
>   RatDiv ('PrTimes ('PrimePower ('NextPrimeAfter p) n) r)
>          ('PrTimes ('PrimePower ('NextPrimeAfter q) n') r')
>        = ShiftPrime (RatDiv ('PrTimes ('PrimePower p n) r)
>                                   ('PrTimes ('PrimePower q n') r'))
>   RatDiv ('PrTimes ('PrimePower 'PrTwo n) r)
>          ('PrTimes ('PrimePower ('NextPrimeAfter p) m) r')
>      = 'PrTimes ('PrimePower 'PrTwo n) (RatDiv r ('PrTimes ('PrimePower ('NextPrimeAfter p) m) r'))
>   RatDiv ('PrTimes ('PrimePower ('NextPrimeAfter p) n) r)
>          ('PrTimes ('PrimePower 'PrTwo m) r')
>      = 'PrTimes ('PrimePower 'PrTwo m) (RatDiv ('PrTimes ('PrimePower ('NextPrimeAfter p) n) r) r')
>   RatDiv 'PrOne ('PrTimes a b) = InvertPrime ('PrTimes a b)
>   RatDiv ('PrTimes a b) 'PrOne = 'PrTimes a b
>   RatDiv 'PrOne 'PrOne = 'PrOne

>type PrThree = 'NextPrimeAfter 'PrTwo
>type PrFive  = 'NextPrimeAfter PrThree
>type PrSeven = 'NextPrimeAfter PrFive

>type PPThird = 'PrimePower PrThree SMinusOne
>type PPHalf = 'PrimePower 'PrTwo SMinusOne
>type PPTwo = 'PrimePower 'PrTwo SOne
>type PPThree = 'PrimePower PrThree SOne
>type PPFour  = 'PrimePower 'PrTwo STwo
>type PPFive  = 'PrimePower PrFive SOne
>type PPSeven = 'PrimePower PrSeven SOne
>type PPEight = 'PrimePower 'PrTwo SThree

>type SOne = 'Su 'Ze
>type STwo = 'Su SOne
>type SThree = 'Su STwo
>type SFour  = 'Su SThree
>type SMinusOne = 'SNegate SOne
>type SMinusTwo = 'SNegate STwo
>type SMinusThree = 'SNegate SThree
>type SMinusFour  = 'SNegate SFour


>type POne = 'PrOne
>type PTwo = 'PrTimes PPTwo 'PrOne
>type PThree = 'PrTimes PPThree 'PrOne
>type PFour  = 'PrTimes PPFour 'PrOne
>type PFive  = 'PrTimes PPFive 'PrOne
>type PSix  = 'PrTimes PPTwo ('PrTimes PPThree 'PrOne)
>type PSeven = 'PrTimes PPSeven 'PrOne
>type PEight = 'PrTimes PPEight 'PrOne
