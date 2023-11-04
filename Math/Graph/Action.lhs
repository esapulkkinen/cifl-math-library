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
>import Control.Arrow
>import Data.Monoid
>import Data.Functor.Contravariant
>import Math.Tools.CoFunctor
>import Math.Tools.Universe
>import Math.Tools.NaturalTransformation
>import Math.Tools.Adjunction
>import Math.Tools.I
>import Math.Matrix.Interface
>import Math.Tools.Prop

>-- | See Lawvere,Rosebrugh: Sets for mathematics
>newtype x :<-: a = Action { runAction :: a -> x }

>(=*=) :: x :<-: b -> (a -> b) -> x :<-: a
>(Action e) =*= m = Action $ e . m

>action_id :: x :<-: a -> x :<-: a
>action_id act = act =*= id

>monoid_act :: (Monoid a) => r :<-: a -> a -> r :<-: a
>monoid_act act m = act =*= (mappend m)

>semigroup_act :: (Semigroup a) => r :<-: a -> a -> r :<-: a
>semigroup_act act m = act =*= (<> m)

>cons_act :: x :<-: [a] -> a -> x :<-: [a]
>cons_act act a = act =*= (a:)

>category_act :: (Category cat) => x :<-: cat a c -> cat a b -> x :<-: cat b c
>category_act act m = act =*= (. m)

>cocategory_act :: (Category cat) => x :<-: cat a c -> cat b c -> x :<-: cat a b
>cocategory_act act m = act =*= (>>> m)

>functor_act :: (Functor f) => x :<-: f b -> (a -> b) -> x :<-: f a
>functor_act act f = act =*= fmap f
>
>cofunctor_act :: (CoFunctor p) => x :<-: p a -> (a -> b) -> x :<-: p b
>cofunctor_act act f = act =*= inverse_image f

>pure_act :: (Applicative f) => x :<-: f a -> x :<-: a
>pure_act act = act =*= pure
>
>applicative_act :: (Applicative f) => x :<-: f b -> f (a -> b) -> x :<-: f a
>applicative_act act f = act =*= (f <*>)

>pair_act :: (x :<-: (c,c')) -> (b -> c, b' -> c') -> x :<-: (b,b')
>pair_act act (f,g) = act =*= (f *** g)

>product_act :: (x :<-: (c,c')) -> (a -> c, a -> c') -> x :<-: a
>product_act act (f,g) = act =*= (f &&& g)

>either_act :: x :<-: b -> (a -> b, c -> b) -> x :<-: Either a c
>either_act act (f,g) = act =*= either f g

>maybe_act :: x :<-: b -> (b, a -> b) -> x :<-: Maybe a
>maybe_act act (x,f) = act =*= maybe x f

>just_act :: x :<-: Maybe a -> x :<-: a
>just_act act = act =*= Just

>nattrans_act :: x :<-: g a -> f :~> g -> x :<-: f a
>nattrans_act act x = act =*= nattrans_component x

>horiz_act :: (Functor h) => h :~> k -> f :~> g -> x :<-: (k :*: g) a -> x :<-: (h :*: f) a
>horiz_act f g = (`nattrans_act` horiz f g)
>
>vert_act :: g :~> h -> f :~> g -> x :<-: h a -> x :<-: f a
>vert_act f g = (`nattrans_act` vert f g)

>counit_act :: (Adjunction f g) => x :<-: a -> x :<-: (f :*: g) a
>counit_act x = ((x =*= unI) `nattrans_act` counit_trans)

>unit_act :: (Adjunction f g) => x :<-: (g :*: f) a -> x :<-: a
>unit_act x = (x `nattrans_act` unit_trans) =*= I

>return_act :: (Monad m) => x :<-: m a -> x :<-: a
>return_act act = act =*= return
> 
>monad_act :: (Monad m) => x :<-: m b -> (a -> m b) -> x :<-: m a
>monad_act act f = act =*= (>>= f)

>smooth :: (x -> y) -> (:<-:) x :~> (:<-:) y
>smooth f = NatTrans $ \(Action x) -> Action $ f . x

>action_member :: a -> x :<-: a -> x
>action_member x (Action f) = f x

>instance Category (:<-:) where
>   id = Action id
>   (Action f) . (Action g) = Action (g . f)

>instance Contravariant ((:<-:) x) where
>   contramap f (Action g) = Action (g . f)

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

