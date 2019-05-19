>{-# OPTIONS_HADDOCK prune #-}
>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
>-- | Module: Math.Graph.Action
>--   Copyright: Esa Pulkkinen, 2018
>--   License: LGPL
>-- 
>-- actions as contravariant functor
>module Math.Graph.Action where
>import Prelude hiding ((.),id)
>import Control.Category
>import Data.Monoid
>import Math.Tools.CoFunctor
>import Math.Tools.Universe
>import Math.Tools.NaturalTransformation
>import Math.Matrix.Interface
>import Math.Tools.Prop

>-- | See Lawvere,Rosebrugh: Sets for mathematics
>newtype Action x a = Action { runAction :: a -> x }

>(=*=) :: Action x b -> (a -> b) -> Action x a
>(Action e) =*= m = Action $ e . m

>smooth :: (x -> y) -> Action x :~> Action y
>smooth f = NatTrans $ \(Action x) -> Action $ f . x

>action_member :: a -> Action x a -> x
>action_member x (Action f) = f x

>instance Category Action where
>   id = Action id
>   (Action f) . (Action g) = Action (g . f)

>instance CoFunctor (Action x) where
>   inverse_image f (Action g) = Action (g . f)

>instance Propositional (Action soc) where
>   runProposition (Action f) x = Action (\ () -> f x)

>instance PropositionalLogic (Action Bool) where
>   (Action f) -&- (Action g) = Action (\x -> f x && g x)
>   (Action f) -|- (Action g) = Action (\x -> f x || g x)
>   invert (Action f) = Action (not . f)

>instance BooleanLogic (Action Bool) where
>   true = Action $ const True

>instance ImplicativeLogic (Action Bool) where
>   (Action f) -=>- (Action g) = Action $ \x -> if f x then g x else True

>instance Relational (Action Bool) where
>   relation f = Action $ \ (x,y) -> y `action_member` f x
>   unrelation p x = Action $ \y -> (x,y) `action_member` p

>instance HasEqualizers (Action Bool) (Action Bool Bool) where
>   equalizer f g = Action $ \x -> runAction (f x !==! g x) ()

>to_prop :: Action Bool x -> Prop x
>to_prop (Action f) = Characteristic f

>from_prop :: Prop x -> Action Bool x
>from_prop (Characteristic f) = Action f

>infix 4 !==!

>(!==) :: (Eq a) => Action a x -> Action a x -> Action Bool x
>(!==) (Action f) (Action g) = Action $ \e -> f e == g e

>-- | performance problem if @a@ in @Action x a@ is large!
>(!==!) :: (Universe a, Eq x) => Action x a -> Action x a -> Action Bool ()
>(!==!) (Action f) (Action g) = Action $ \ () -> and $ map (\a -> f a == g a) all_elements
  
>-- | separator f g
>instance (Universe a, Eq x) => Eq (Action x a) where
>   x == y = runAction (x !==! y) ()

>action_compose :: Action t' t -> Action t t'' -> Action t' t''
>action_compose (Action f) (Action g) = Action (f . g)

>action_image :: (x -> y) -> Action x a -> Action y a
>action_image f (Action x) = Action (f . x)

>action_product :: Action a a' -> Action b b' -> Action (a,b) (a',b')
>action_product (Action f) (Action g) = Action (\ (a',b') -> (f a',g b'))

>action_coproduct :: Action a a' -> Action b b' -> Action (Either a b) (Either a' b')
>action_coproduct (Action f) (Action g) = Action (either (Left . f) (Right . g))

>yoneda :: Action x c -> (Action c) :~> (Action x)
>yoneda f = NatTrans (action_compose f)

>unyoneda :: (Action c) :~> (Action x) -> Action x c
>unyoneda (NatTrans f) = f id

>chaotic :: (Category cat) => cat b c -> (cat a c -> x) -> Action x (cat a b)
>chaotic a f = Action (\t -> f (a . t))

>action :: Action x c -> Action c c' -> (Action c') :~> (Action x)
>action x y = yoneda x `vert` yoneda y

>stack_action :: Action x  c -> Action x' c'
>             -> (Action c :*: Action x') :~> (Action x :*: Action c')
>stack_action x y = yoneda x `inverse_horiz` yoneda y

