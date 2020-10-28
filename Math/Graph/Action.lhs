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
>newtype x :<-: a = Action { runAction :: a -> x }

>(=*=) :: x :<-: b -> (a -> b) -> x :<-: a
>(Action e) =*= m = Action $ e . m

>smooth :: (x -> y) -> (:<-:) x :~> (:<-:) y
>smooth f = NatTrans $ \(Action x) -> Action $ f . x

>action_member :: a -> x :<-: a -> x
>action_member x (Action f) = f x

>instance Category (:<-:) where
>   id = Action id
>   (Action f) . (Action g) = Action (g . f)

>instance CoFunctor ((:<-:) x) where
>   inverse_image f (Action g) = Action (g . f)

>instance Propositional ((:<-:) soc) where
>   runProposition (Action f) x = Action (\ () -> f x)

>instance PropositionalLogic ((:<-:) Bool) where
>   (Action f) -&- (Action g) = Action (\x -> f x && g x)
>   (Action f) -|- (Action g) = Action (\x -> f x || g x)
>   invert (Action f) = Action (not . f)

>instance BooleanLogic ((:<-:) Bool) where
>   true = Action $ const True

>instance ImplicativeLogic ((:<-:) Bool) where
>   (Action f) -=>- (Action g) = Action $ \x -> if f x then g x else True

>instance Relational ((:<-:) Bool) where
>   relation f = Action $ \ (x,y) -> y `action_member` f x
>   unrelation p x = Action $ \y -> (x,y) `action_member` p

>instance HasEqualizers ((:<-:) Bool) (Bool :<-: Bool) where
>   equalizer f g = Action $ \x -> runAction (f x !==! g x) ()

>to_prop :: Bool :<-: x -> Prop x
>to_prop (Action f) = Characteristic f

>from_prop :: Prop x -> Bool :<-: x
>from_prop (Characteristic f) = Action f

>infix 4 !==!

>(!==) :: (Eq a) => a :<-: x -> a :<-: x -> Bool :<-: x
>(!==) (Action f) (Action g) = Action $ \e -> f e == g e

>-- | performance problem if @a@ in @x :<-: a@ is large!
>(!==!) :: (Universe a, Eq x) => x :<-: a -> x :<-: a -> Bool :<-: ()
>(!==!) (Action f) (Action g) = Action $ \ () -> and $ map (\a -> f a == g a) all_elements
  
>-- | separator f g
>instance (Universe a, Eq x) => Eq (x :<-: a) where
>   x == y = runAction (x !==! y) ()

>action_compose :: t' :<-: t -> t :<-: t'' -> t' :<-: t''
>action_compose (Action f) (Action g) = Action (f . g)

>action_image :: (x -> y) -> x :<-: a -> y :<-: a
>action_image f (Action x) = Action (f . x)

>action_product :: a :<-: a' -> b :<-: b' -> (a,b) :<-: (a',b')
>action_product (Action f) (Action g) = Action (\ (a',b') -> (f a',g b'))

>action_coproduct :: a :<-: a' -> b :<-: b' ->(Either a b) :<-: (Either a' b')
>action_coproduct (Action f) (Action g) = Action (either (Left . f) (Right . g))

>yoneda :: x :<-: c -> ((:<-:) c) :~> ((:<-:) x)
>yoneda f = NatTrans (action_compose f)

>unyoneda :: ((:<-:) c) :~> ((:<-:) x) -> x :<-: c
>unyoneda (NatTrans f) = f id

>chaotic :: (Category cat) => cat b c -> (cat a c -> x) -> x :<-: (cat a b)
>chaotic a f = Action (\t -> f (a . t))

>action :: x :<-: c -> c :<-: c' -> ((:<-:) c') :~> ((:<-:) x)
>action x y = yoneda x `vert` yoneda y

>stack_action :: x :<-: c -> x' :<-: c'
>             -> ((:<-:) c :*: (:<-:) x') :~> ((:<-:) x :*: (:<-:) c')
>stack_action x y = yoneda x `inverse_horiz` yoneda y

