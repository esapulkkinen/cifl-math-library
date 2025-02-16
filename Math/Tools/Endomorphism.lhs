>{-# LANGUAGE FlexibleInstances, Trustworthy #-}
>{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms, LambdaCase #-}
>{-# OPTIONS_HADDOCK hide #-}
>module Math.Tools.Endomorphism where
>import qualified Data.Monoid as Mon
>import Math.Tools.Isomorphism
>import Math.Tools.Universe

>newtype Endo a = EndoI { unEndo :: Mon.Endo a }
>  deriving (Semigroup, Monoid)

>pattern Endo f = EndoI (Mon.Endo f)

>class (Monoid x) => GraphAction x where
>   source   :: x
>   target   :: x
>   isSource :: x -> Bool
>   isTarget :: x -> Bool
>   isIdentity :: x -> Bool

>class (GraphAction x) => ReversibleGraphAction x where
>   invert   :: x
>   isInvert :: x -> Bool

>instance GraphAction () where
>   source = ()
>   target = ()
>   isSource _ = True
>   isTarget _ = True
>   isIdentity _ = True

>instance GraphAction (Endo Bool) where
>   isIdentity = isIdentityE
>   source   = falseE
>   isSource = isFalseE
>   target   = trueE
>   isTarget = isTrueE

>instance GraphAction Ordering where
>   source   = LT
>   target   = GT
>   isIdentity x = x == EQ 
>   isSource   x = x == LT
>   isTarget   x = x == GT

>instance ReversibleGraphAction (Endo Bool) where
>   invert = notE
>   isInvert = isNotE

>instance (Universe a, Eq a) => Eq (Endo a) where
>  (Endo f) == (Endo g) = and [f x == g x | x <- allElements]

>instance (Show a, Universe a) => Show (Endo a) where
>   show (Endo f) = show [(x,f x) | x <- allElements]

>inverse_image_endo :: (a -> a) -> Endo a -> Endo a
>inverse_image_endo f (Endo g) = Endo (g . f)

>isoMapEndo :: Iso a b -> Endo a -> Endo b
>isoMapEndo i (Endo f) = Endo (isomorphismEpimorphism i . f . isomorphismSection i)

>identityE :: Endo a
>identityE = Endo id -- == mempty

>isIdentityE :: (Universe a, Eq a) => Endo a -> Bool
>isIdentityE (Endo f) = and [f x == x | x <- allElements]

>notE :: Endo Bool
>notE = Endo not

>isNotE :: Endo Bool -> Bool
>isNotE (Endo f) = f True == False && f False == True

>trueE :: Endo Bool
>trueE = Endo (const True)

>isTrueE :: Endo Bool -> Bool
>isTrueE (Endo f) = f True == True && f False == True

>falseE :: Endo Bool
>falseE = Endo (const False)

>isFalseE :: Endo Bool -> Bool
>isFalseE (Endo f) = f True == False && f False == False

>threeT :: (Ordering,Ordering,Ordering) -> Endo Ordering
>threeT (f,g,h) = Endo $ \case
>   LT -> f
>   EQ -> g
>   GT -> h

