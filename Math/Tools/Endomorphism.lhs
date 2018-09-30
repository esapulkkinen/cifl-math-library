>{-# LANGUAGE FlexibleInstances, Trustworthy #-}
>{-# OPTIONS_HADDOCK hide #-}
>module Math.Tools.Endomorphism where
>import Data.Monoid
>import Math.Tools.Isomorphism
>import Math.Tools.Universe

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
>  (Endo f) == (Endo g) = and [f x == g x | x <- all_elements]

>instance (Show a, Universe a) => Show (Endo a) where
>   show (Endo f) = show [(x,f x) | x <- all_elements]

>inverse_image_endo :: (a -> a) -> Endo a -> Endo a
>inverse_image_endo f (Endo g) = Endo (g . f)

>iso_map_endo :: Iso a b -> Endo a -> Endo b
>iso_map_endo i (Endo f) = Endo (isomorphism_epimorphism i . f . isomorphism_section i)

>identityE :: Endo a
>identityE = Endo id -- == mempty

>isIdentityE :: (Universe a, Eq a) => Endo a -> Bool
>isIdentityE (Endo f) = and [f x == x | x <- all_elements]

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
>threeT (f,g,h) = Endo $ \act -> case act of { LT -> f ; EQ   -> g ; GT   -> h }

