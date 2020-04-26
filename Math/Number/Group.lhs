>{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, Safe #-}
>module Math.Number.Group where
>import Prelude hiding ((.),id)
>import Control.Category
>import Data.Ratio
>import Data.Monoid
>import Data.Set (Set)
>import Math.Tools.CoFunctor
>import Math.Tools.Universe
>import Math.Tools.Prop (Prop)
>import qualified Math.Tools.Prop
>import qualified Data.Set as Set
>import Math.Matrix.Interface

>-- | <https://en.wikipedia.org/wiki/Group_(mathematics)>

>class (Monoid g) => Group g where
>   ginvert :: g -> g


>class SetLike s where
>   sintersection :: s -> s -> s
>   sunion :: s -> s -> s
>   scomplement :: s -> s

>-- | Abelian groups are required to be commutative with respect to mappend.
>class (Monoid g) => AbelianGroup g where
>   gnegate :: g -> g

>instance Group Ordering where
>   ginvert LT = GT
>   ginvert EQ = EQ
>   ginvert GT = LT

>subgroup :: (Group a) => Prop a -> Prop (a,a)
>subgroup restriction = characteristic $ \ (a,b) ->
>   ((a `member` restriction) -&- (b `member` restriction))
>     -=>- ((a <> ginvert b) `member` restriction)

>subgroup_identity :: (Group a) => Prop a -> Prop a
>subgroup_identity r = characteristic $ \ a -> (a,a) `member` subgroup r

>subgroup_inverse :: (Group a) => Prop a -> Prop a
>subgroup_inverse r = characteristic $ \a -> (mempty,a) `member` subgroup r

>subgroup_closure :: (Group a) => Prop a -> Prop (a,a)
>subgroup_closure r = characteristic $ \(a,b) -> (a,ginvert b) `member` subgroup r

>data SubGroup a b = SubGroup { subgroup_part :: a -> b,
>                               subgroup_restriction :: Prop a }

>instance Category SubGroup where
>   id = SubGroup id true
>   (SubGroup f p) . (SubGroup g q) = SubGroup (f . g)
>      (characteristic $ \c -> (c `member` q) -&- ((g c) `member` p))

>instance Group () where
>   ginvert () = ()

>instance (Fractional a) => Group (Product a) where
>   ginvert (Product x) = Product $ 1 / x

>instance (Num a) => AbelianGroup (Sum a) where
>   gnegate (Sum x) = Sum $ negate x

>instance (Group b) => Group (a -> b) where
>   ginvert f = ginvert . f

>instance Group All where
>   ginvert (All x) = All (not x)

>instance Group Any where
>   ginvert (Any x) = Any (not x)

>instance (Group b) => Group (Endo b) where
>   ginvert (Endo f) = Endo $ ginvert . f

>instance (Group a, Group b) => Group (a,b) where
>   ginvert (a,b) = (ginvert a, ginvert b)

>instance (Group a, Group b, Group c) => Group (a,b,c) where
>   ginvert (a,b,c) = (ginvert a, ginvert b, ginvert c)
>   
>instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
>   ginvert (a,b,c,d) = (ginvert a, ginvert b, ginvert c, ginvert d)

>automorphism :: (Group g) => g -> Endo g
>automorphism x = Endo $ \y -> x <> y <> ginvert x

>-- | https://en.wikipedia.org/wiki/Commutator

>group_commutator :: (Group g) => g -> g -> g
>group_commutator g h = ginvert g <> ginvert h <> g <> h

>-- | <https://en.wikipedia.org/wiki/Group_ring>

>data GroupRing r g = GroupRing { groupring_apply :: g -> r }

>groupring_element :: (Universe g, VectorSpace g) => GroupRing (Scalar g) g -> g
>groupring_element (GroupRing f) = vsum [f x %* x | x <- all_elements]
>
>instance (Num r) => VectorSpace (GroupRing r g) where
>   type Scalar (GroupRing r g) = r
>   vzero = GroupRing $ const 0
>   vnegate (GroupRing f) = GroupRing $ negate . f 
>   (GroupRing f) %+ (GroupRing g) = GroupRing $ \a -> f a + g a
>   x %* (GroupRing f) = GroupRing $ \a -> x * f a
>
>instance (Num r,Group g, Universe g) => Num (GroupRing r g) where
>   (GroupRing f) + (GroupRing g) = GroupRing $ \a -> f a + g a
>   (GroupRing f) - (GroupRing g) = GroupRing $ \a -> f a - g a
>   (GroupRing f) * (GroupRing g) = GroupRing $ \a -> sum $
>             [f u * g (ginvert a <> u) | u <- all_elements]
>   abs (GroupRing f) = GroupRing (abs . f)
>   signum (GroupRing f) = GroupRing (signum . f)
>   fromInteger i = GroupRing $ \_ -> fromInteger i


