>{-# LANGUAGE Safe #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Graph.TransformationMonoid where
>import Math.Tools.CoFunctor
>import Data.Monoid
>import Data.Maybe

-- from Data.Monoid:
-- newtype Endo a = Endo { appEndo :: a -> a }
-- instance Monoid (Endo a)

instance HasMonoidAction (Endo b) b where
   action = appEndo


>data ReversibleGraph a = ReversibleGraph { runRG :: Endo Bool -> a -> a }

instance HasMonoidAction (Endo Bool) FooGraph where

>data FooGraph = FLeave | FIn | FOut | FEnter | FForay
>  deriving (Eq)

>fooGraph :: ReversibleGraph FooGraph
>fooGraph = actionGraph [(FLeave, (FEnter, FIn, FOut)),
>                        (FEnter, (FLeave, FOut,FIn)),
>                        (FForay, (FForay, FIn,FIn)),
>                        (FOut, (FIn, FOut,FOut)),
>                        (FIn,  (FOut, FIn, FIn))]

each pair <n,<inverse(n), source(n),target(n)>> in the input list represents
information about 'n'.

This function does _not_ ensure that the rules for action graph, that is:

   inverse(inverse(n)) == n
   source(target(n)) == target(n)
   source(source(n)) == source(n)
   target(target(n)) == target(n)
   target(source(n)) == source(n)

>actionGraph :: (Eq n) => [(n,(n,n,n))] -> ReversibleGraph n
>actionGraph lst = ReversibleGraph func
> where func f x | isFalseE f = let (_,s,_) = (fromJust $ lookup x lst) in s
>                | isTrueE  f = let (_,_,t) = (fromJust $ lookup x lst) in t
>                | isIdentityE f = x
>                | isNotE f = let (n,_,_) = (fromJust $ lookup x lst) in n

isVertex :: (HasMonoidAction (Endo Bool) b, Eq b) => b -> Bool

>isVertex :: (Eq n) => ReversibleGraph n -> n -> Bool
>isVertex (ReversibleGraph action) x = action falseE x == x

>data Vertex a = VertexPrivate a -- do not use constructor

>vertex :: (Eq b) => ReversibleGraph b -> b -> Maybe (Vertex b)
>vertex rg x | isVertex rg x = Just (VertexPrivate x)
>            | otherwise  = Nothing

>sourceOf :: ReversibleGraph b -> b -> b
>sourceOf (ReversibleGraph action) x = action falseE x

>targetOf :: ReversibleGraph b -> b -> b
>targetOf (ReversibleGraph action) x = action trueE x

>isLoop :: (Eq b) => ReversibleGraph b -> b -> Bool
>isLoop g x = sourceOf g x == targetOf g x

>isTwoLaneLoop :: (Eq b) => ReversibleGraph b -> b -> Bool
>isTwoLaneLoop g x = isLoop g x && not (isOneLaneLoop g x)

>isOneLaneLoop :: (Eq b) => ReversibleGraph b -> b -> Bool
>isOneLaneLoop (ReversibleGraph action) x = action notE x == x

>isArrowFromTo :: (Eq b) => ReversibleGraph b -> b -> b -> b -> Bool
>isArrowFromTo g x a b = sourceOf g x == a && targetOf g x == b

>inverse_image_endo :: (a -> a) -> Endo a -> Endo a
>inverse_image_endo f (Endo g) = Endo (g . f)

>identityE :: Endo Bool
>identityE = Endo id

>isIdentityE :: Endo Bool -> Bool
>isIdentityE (Endo f) = f True == True && f False == False

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


