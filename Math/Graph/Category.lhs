>{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeOperators #-}
>{-# LANGUAGE StandaloneDeriving, RankNTypes #-}
>{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
>{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
>module Math.Graph.Category where
>import Data.Set
>import Math.Tools.NaturalTransformation
>import Math.Tools.FixedPoint
>import qualified Control.Category as Cat
>import Prelude hiding ((.), id, Monad(..))
>import qualified Prelude (Monad(..),id,(.))

>class (Cat.Category arr, Cat.Category arr') => Adjunction2 arr arr' f g
> | f -> g, g -> f, arr -> arr', arr' -> arr where
>  lad :: arr (f a) b -> arr' a (g b)
>  rad :: arr' a (g b) -> arr (f a) b
>  unit :: arr' a (g (f a))
>  counit :: arr (f (g b)) b

>class Category3 arr where
>  point3 :: arr a a a
>  line3  :: arr a a b
>  rotate3 :: arr a b c -> arr b c a

  compose3 :: arr a b c -> arr b d c -> arr d e c -> arr e f c -> arr f g c
           -> arr (

    A-----G            AC
    /\   / \           | \
   /  \ /   \          |  \
  B----C-----F  ==>    |   FC
   \  / \   /          |  /
    \/   \ /           | / 
     D----E            DC 

>class (Category3 arr, Category3 arr', Category3 arr'')
> => Adjunction3 arr arr' arr'' f g h
>    | f -> g, g -> h, h -> f, arr -> arr', arr' -> arr'', arr'' -> arr where
>  lad3 :: arr (f a) b c -> arr' a (g b) c
>  mad3 :: arr' a (g b) c -> arr'' a b (h c)
>  rad3 :: arr'' a b (h c) -> arr (f a) b c
>  unit3 :: arr' a (g (f a)) (f a)
>  munit3 :: arr'' (g b) b (h (g b))
>  counit3 :: arr (f (h c)) (h c) c

>data Iso (arr :: k -> k -> *) (a :: k) (b :: k) where
>  (:<->:) :: arr a b -> arr b a -> Iso arr a b

>data NatT f g = NatT { unNatT :: forall a. f a -> g a }
>data NatIso f g = NatIso { unNatIso :: forall a. Iso (->) (f a) (g a) }

>data KleisliA a b = KleisliA { unKleisliA :: a -> IO b }

>instance Cat.Category NatT where
>   id = NatT Cat.id
>   (NatT f) . (NatT g) = NatT (f Cat.. g)

>instance Cat.Category KleisliA where
>   id = KleisliA Prelude.return
>   (KleisliA f) . (KleisliA g) = KleisliA $ \a -> g a Prelude.>>= f

>instance (Cat.Category arr) => Cat.Category (Iso arr) where
>   id = Cat.id :<->: Cat.id
>   (f :<->: g) . (f' :<->: g') = (f Cat.. f') :<->: (g' Cat.. g)

>type family Hom (a :: k) (b :: k) :: *
>type instance Hom a b = NatT a b
>type instance Hom a b = KleisliA a b

