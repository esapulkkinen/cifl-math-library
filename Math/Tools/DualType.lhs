>{-# LANGUAGE UndecidableInstances, RankNTypes, TypeFamilies, FlexibleInstances #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.DualType where


>type family Dual a

>newtype Bot = Bot (forall a.a)

>undef :: Bot
>undef = Bot undefined

>type instance Dual () = Bot
>type instance Dual Bot = ()

>type instance Dual (Either a b) = (Dual a,Dual b)
>type instance Dual (a,b) = Either (Dual a) (Dual b)

>class Duality a where
>   unit_dual   :: a -> Dual (Dual a)
>   counit_dual :: Dual (Dual a) -> a

REMEMBER: Duality is undecidable. In particular, it is impossible to
compare _objects_ of C to objects of C^{op}. This is why category
theory can name C^{op} objects with the same name than C objects.  It
is still true that products in C^{op} correspond to coproducts in C.
However, products in C^{op} behave like coproducts in C. Similarly,
coproducts in C^{op} behave like products in C. 

>instance (Dual (Dual a) ~ a) => Duality a where
>   unit_dual = id
>   counit_dual = id

>map_dual :: (Dual (Dual c) ~ c, Dual (Dual a) ~ a) => (a -> c) -> Dual (Dual a) -> Dual (Dual c)
>map_dual f = unit_dual . f . counit_dual 

>unmap_dual :: (Dual (Dual a) ~ a, Dual (Dual b) ~ b) => (Dual (Dual b) -> Dual (Dual a)) -> b -> a
>unmap_dual f = counit_dual . f . unit_dual
