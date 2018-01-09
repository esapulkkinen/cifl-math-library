>{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
>module Math.Tools.AdjunctionArrow where
>import Prelude hiding (id,(.))
>import Control.Category

>class (Category c, Category d) => FunctorA f c d where
>   mapA :: c a b -> d (f a) (f b)

>class (Category c, Category d) => CofunctorA f c d where
>   inverseImageA :: c a b -> d (f b) (f a)

>class (FunctorA f d c, FunctorA g c d) => 
>        AdjunctionArrow f g c d | f -> g, g -> f, c -> d, d -> c
>   where
>      leftAdjunctA  :: c (f a) b -> d a (g b)
>      rightAdjunctA :: d a (g b) -> c (f a) b
>      unitA   :: d a (g (f a))
>      counitA :: c (f (g b)) b
>      unitA = leftAdjunctA id
>      counitA = rightAdjunctA id
>      leftAdjunctA f = mapA f <<< unitA
>      rightAdjunctA g = counitA <<< mapA g

