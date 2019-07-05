>{-# LANGUAGE Safe,GADTs, Rank2Types, FlexibleInstances #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Graph.Monoid where
>import Prelude hiding (id,(.))
>import Control.Category
>import Data.Monoid
                
>data Mon a b where
>   Mon :: ((Monoid m) => m) -> Mon m m

>instance Semigroup (Mon x x) where
>   (Mon x) <> (Mon y) = Mon (x <> y)

>instance Monoid (Mon x x) where
>   mempty = Mon mempty
>   mappend (Mon x) (Mon y) = Mon (mappend x y)

>instance Category Mon where
>  id = Mon mempty
>  (Mon x) . (Mon y) = Mon (mappend x y)

>instance (Monoid a,Show a) => Show (Mon a a) where
>  show (Mon x) = "monoid[" ++ show x ++ "]"

>instance Eq (Any -> Any) where
>  f == g = (f (Any True) == g (Any True)) && (f (Any False) == g (Any False))

Monoid action is really a functor F : M -> Set.

>data MAction x y = MAction { maction :: Mon x x -> y -> y }
> -- required: 
>  -- maction act mempty x == x
>  -- maction act (mappend x y) z == maction act x (maction act y z)


>type MGraph a = MAction (Any -> Any) a

>nameF :: (Category cat) => Mon (cat a a) (cat a a)
>nameF = Mon id
>inverseF :: Mon (Any -> Any) (Any -> Any)
>inverseF = Mon (Any . not . getAny)
>sourceF :: Mon (a -> Any) (a -> Any)
>sourceF = Mon (const (Any False))
>targetF :: Mon (a -> Any) (a -> Any)
>targetF = Mon (const (Any True))

>source :: MGraph a -> a -> a
>source g x = maction g sourceF x

>target :: MGraph a -> a -> a
>target g x = maction g targetF x

>inverseM :: MGraph a -> a -> a
>inverseM g x = maction g inverseF x

>isLoop :: (Eq a) => MGraph a -> a -> Bool
>isLoop g x = source g x == target g x

>isOneLaneLoop :: (Eq a) => MGraph a -> a -> Bool
>isOneLaneLoop g x = inverseM g x == x

>isVertex :: (Eq a) => MGraph a -> a -> Bool
>isVertex g x = source g x == x

>hasArrow :: (Eq a) => MGraph a -> a -> a -> a -> Bool
>hasArrow g x a b = source g x == a && target g x == b

The elements in the list are
(edge,source,target,inverse)

>tableGraph :: (Eq a) => [(a,a,a,a)] -> a -> MGraph a
>tableGraph lst def = MAction $ \act x -> maybe def (foldFour act)
>                             $ findElem lst x
  
>findElem :: (Eq a) => [(a,a,a,a)] -> a -> Maybe (a,a,a,a)
>findElem (z@(a,_,_,_):r) a' | a == a' = Just z
>                            | otherwise = findElem r a'
>findElem [] _ = Nothing

>foldFour :: Mon (Any -> Any) (Any -> Any) -> (a,a,a,a) -> a
>foldFour (Mon f) (a',b',c',d') = case (f (Any False),f (Any True)) of
>           (Any False, Any True ) -> a' -- nameF
>           (Any False, Any False) -> b' -- sourceF
>           (Any True , Any True ) -> c' -- targetF
>           (Any True , Any False) -> d' -- inverseF
