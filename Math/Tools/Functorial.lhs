>module Math.Tools.Functorial where
>import Math.Tools.CoFunctor
>import Math.Tools.FixedPoint

===== COFUNCTOR =====

>class EqFunctor eqf where
>      equal_structure :: (Eq a) => eqf a -> eqf a -> Bool

>class (EqFunctor f) => OrdFunctor f where
>      compare_structure :: (Ord a) => f a -> f a -> Ordering

>instance (EqFunctor f) => Eq (Rec f) where { (In x) == (In y) = equal_structure x y }
>instance (OrdFunctor f) => Ord (Rec f) where { compare (In x) (In y) = compare_structure x y }
>instance EqFunctor Maybe where 
>	  equal_structure Nothing Nothing = True
>	  equal_structure (Just x) (Just y) = x == y
>	  equal_structure _ _ = False

>instance OrdFunctor Maybe where
>	  compare_structure Nothing Nothing = EQ
>	  compare_structure (Just x) (Just y) = compare x y
>	  compare_structure Nothing (Just _) = LT
>	  compare_structure (Just _) Nothing = GT

>inner :: (CoFunctor f, CoFunctor g) => (a -> b -> c) -> f (g b) -> g c -> f a
>inner f x y = inverse_image (\a -> inverse_image (f a) y) x

>contraMap :: (Functor f, CoFunctor g) => (a -> b -> c) -> g (f c) -> f b -> g a
>contraMap f x y = inverse_image (\a -> fmap (f a) y) x
