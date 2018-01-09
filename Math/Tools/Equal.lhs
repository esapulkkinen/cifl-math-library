>{-# LANGUAGE Rank2Types, KindSignatures, GADTs #-}
>module Math.Tools.Equal where
>import Math.Tools.I
>import Math.Tools.Identity
>import Math.Tools.Adjunction
>import Data.Generics hiding (TypeRep)
>import Unsafe.Coerce
>import Math.Matrix.Interface

Equality is from Baars, Swierstra: Typing Dynamic Typing

>data Dynamic typeRep where
>   Dynamic :: a -> typeRep a -> Dynamic typeRep

>toDynamic :: a -> typeRep a -> Dynamic typeRep
>toDynamic x t = Dynamic x t

>fromDynamic :: (TypeRep typeRep) => typeRep a -> Dynamic typeRep -> Maybe a
>fromDynamic tr (Dynamic x t) = case is_equal_type t tr of
>                               (Just eq) -> Just (coerce eq x)
>                               Nothing -> Nothing

>has_same_type_as :: (Typeable a, Typeable b) => a -> b -> Maybe (EqType a b)
>has_same_type_as x y = if typeOf x == typeOf y
>                       then Just (unsafeCoerce EqType)
>                       else Nothing

>is_same_type :: (Typeable a, Typeable b) => h a -> h b -> Maybe (EqType a b)
>is_same_type x y = if typeOf (getArg x) == typeOf (getArg y)
>                     then Just (unsafeCoerce EqType)
>                     else Nothing
>    where getArg :: c x -> x
>          getArg = undefined


>data EqType a b where
>   EqType :: EqType c c

>data FEqual (f :: * -> *) (g :: * -> *) = FEqual (forall h. h f -> h g)

>class TypeRep rep where
>   is_equal_type :: rep a -> rep b -> Maybe (EqType a b)


>par_eq :: EqType a b -> EqType a b -> EqType a b
>par_eq EqType EqType = EqType

>coerce :: EqType a b -> a -> b
>coerce ab = subst I unI ab

>curryT :: EqType ((a,b) -> c) (a -> b -> c)
>curryT = unsafeCoerce EqType

>reflex :: EqType a a
>reflex = EqType

>trans :: EqType a b -> EqType b c -> EqType a c
>trans EqType EqType = EqType

>subst :: (ta -> c a) -> (c b -> tb) -> EqType a b -> ta -> tb
>subst from to EqType = to . id . from

>symm :: EqType a b -> EqType b a
>symm ab = (subst Flip unFlip ab) reflex

>newtype Flip y x = Flip { unFlip :: EqType x y }


>arg :: EqType a b -> EqType (f a) (f b)
>arg ab = subst Matrix cells ab reflex

>rewrite :: EqType a b -> EqType c (f a) -> EqType c (f b)
>rewrite a_b c_fa = let fa_fb = arg a_b in trans c_fa fa_fb

>map_eqtype :: EqType a b -> f a -> f b
>map_eqtype eqt = coerce (arg eqt)

>data Pair x = Pair { unPair :: (x,x) }

>data MapEqTypeSecond f a c = MapEqTypeSecond { unmap_eqtype_second :: f c a }
>data MapEqTypeThird f a b c = MapEqTypeThird { unmap_eqtype_third  :: f c b a }

>map_eqtype_2nd :: EqType a b -> f a c -> f b c
>map_eqtype_2nd eqt val = subst MapEqTypeSecond unmap_eqtype_second eqt val

>map_eqtype_3rd :: EqType a b -> f a c d -> f b c d
>map_eqtype_3rd eqt val = subst MapEqTypeThird unmap_eqtype_third eqt val

>data Equal a b = Equal (forall f. f a -> f b)

>coerceEqual :: Equal a b -> a -> b
>coerceEqual ab = substEqual I unI ab

>reflexEqual :: Equal a a
>reflexEqual = Equal id

>transEqual :: Equal a b -> Equal b c -> Equal a c
>transEqual ab bc = case (ab,bc) of
>              (Equal f, Equal g) -> Equal (g . f)
>substEqual :: (ta -> c a) -> (c b -> tb) -> Equal a b -> ta -> tb
>substEqual from to (Equal ab) = to . ab . from

>substPair :: Equal a b -> (a,a) -> (b,b)
>substPair = substEqual Pair unPair

>newtype FlipEqual y x = FlipEqual { unFlipEqual :: Equal x y }

>symmEqual :: Equal a b -> Equal b a
>symmEqual ab = (substEqual FlipEqual unFlipEqual ab) reflexEqual

>argEqual :: Equal a b -> Equal (f a) (f b)
>argEqual ab = Equal (substEqual Matrix cells ab)

>rewriteEqual :: Equal a b -> Equal c (f a) -> Equal c (f b)
>rewriteEqual a_b c_fa = let fa_fb = argEqual a_b in transEqual c_fa fa_fb


