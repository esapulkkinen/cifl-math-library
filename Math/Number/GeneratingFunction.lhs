>{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
>module Math.Number.GeneratingFunction where
>import Control.Applicative
>import Math.Number.Stream
>import Math.Matrix.Interface
>import Math.Tools.PrettyP
>import Math.Tools.Median
>-- | data types that represent generating functions with specific convention
>-- | OGF = Ordinary generating function
>newtype OGF a = OGF { unOGF :: Stream a }
>   deriving newtype (Show, PpShow, PpShowF, PpShowVerticalF)
>   deriving newtype (Functor, Applicative, Monad, ConjugateSymmetric)

>z_ogf :: (Num a) => OGF a
>z_ogf = OGF z

>-- | EGF = Exponential generating function <https://en.wikipedia.org/wiki/Generating_function>
>newtype EGF a = EGF { unEGF :: Stream a }
>   deriving newtype (Show,PpShow, PpShowF, PpShowVerticalF)
>   deriving newtype (Functor, Applicative, Monad, ConjugateSymmetric)

>ogf_to_egf :: (Num a) => OGF a -> EGF a
>ogf_to_egf (OGF s) = EGF $ liftA2 (*) s factorial

>egf_to_pg :: (Floating a, Eq a) => EGF a -> PG a
>egf_to_pg (EGF s) = PG $ s / exp z

>-- | Poisson generating function
>newtype PG a = PG { unPG :: Stream a }
>   deriving newtype (Show,PpShow, PpShowF, PpShowVerticalF)
>   deriving newtype (Functor, Applicative, Monad, ConjugateSymmetric)
>-- | Lambert series
>newtype LG a = LG { unLG :: Stream a }
>   deriving newtype (Show,PpShow, PpShowF, PpShowVerticalF)
>   deriving newtype (Functor, Applicative, Monad, ConjugateSymmetric)
>-- | Bell series
>newtype BG a = BG { unBG :: Stream a }
>   deriving newtype (Show,PpShow, PpShowF, PpShowVerticalF)
>   deriving newtype (Functor, Applicative, Monad, ConjugateSymmetric)
>-- | Dirichlet series generating function
>newtype DG a = DG { unDG :: Stream a }
>   deriving newtype (Show,PpShow, PpShowF, PpShowVerticalF)
>   deriving newtype (Functor, Applicative, Monad, ConjugateSymmetric)

>instance (Num a) => Num (OGF a) where
>   (OGF a) + (OGF b) = OGF (a + b)
>   (OGF a) - (OGF b) = OGF (a - b)
>   (OGF a) * (OGF b) = OGF (a * b)
>   negate (OGF a) = OGF (negate a)
>   abs (OGF a) = OGF (abs a)
>   signum (OGF a) = OGF (signum a)
>   fromInteger i = OGF (constant (fromInteger i))
>
>instance (Fractional a) => Num (EGF a) where
>   (EGF a) + (EGF b) = EGF (a + b)
>   (EGF a) - (EGF b) = EGF (a - b)
>   (EGF x) * (EGF y) = EGF $ fmap sum_seq $ liftA2 (liftA2 (*)) q p
>      where p = codiagonals_seq $ matrix (*) x y
>            q = codiagonals_seq $ Matrix $ 1 / (1 - z - z2)
>   negate (EGF x) = EGF (negate x)
>   abs (EGF x) = EGF (abs x)
>   signum (EGF x) = EGF (signum x)
>   fromInteger i = EGF (constant (fromInteger i))



