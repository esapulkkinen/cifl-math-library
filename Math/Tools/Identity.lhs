>{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, TypeOperators #-}
>module Math.Tools.Identity where
>import Math.Tools.I
>import Math.Tools.PrettyP
>import Math.Tools.Adjunction
>import Math.Tools.Group
>import Math.Tools.NaturalTransformation
>import Math.Matrix.Interface

>reduce_first :: (I :*: g) :~> g
>reduce_first = NatTrans (unI . cells)
>
>reduce_second :: (Functor f) => (f :*: I) :~> f
>reduce_second = NatTrans (fmap unI . cells) 

>counit_trans :: (Adjunction f g) => (f :*: g) :~> I
>counit_trans = NatTrans (I . counit . cells)

>unit_trans :: (Adjunction f g) => I :~> (g :*: f)
>unit_trans = NatTrans (Matrix . unit . unI)

>instance (PpShow a) => PpShow (I a) where
>	  pp (I x) = pp x

>instance Group I 
