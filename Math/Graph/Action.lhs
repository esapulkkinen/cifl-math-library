>{-# OPTIONS_HADDOCK prune #-}
>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
>{-# LANGUAGE FunctionalDependencies #-}
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

>class (Contravariant f, Contravariant g) => ContraAdjunction f g | f -> g, g -> f where
>   contraLeftAdjunct :: (f a :<-: b) -> a :<-: g b
>   contraRightAdjunct :: (a :<-: g b) -> f a :<-: b
>   contraUnit :: a :<-: g (f a)
>   contraCounit :: f (g b) :<-: b
>   contraUnit = contraLeftAdjunct (Action id)
>   contraCounit = contraRightAdjunct (Action id)

>infixr 8 =*=

>(=*=) :: x :<-: b -> (a -> b) -> x :<-: a
>(Action e) =*= m = Action $ e . m

>actionId :: x :<-: a -> x :<-: a
>actionId act = act =*= id

>monoidAct :: (Monoid a) => r :<-: a -> a -> r :<-: a
>monoidAct act m = act =*= (mappend m)

>semigroupAct :: (Semigroup a) => r :<-: a -> a -> r :<-: a
>semigroupAct act m = act =*= (<> m)

>consAct :: x :<-: [a] -> a -> x :<-: [a]
>consAct act a = act =*= (a:)

>categoryAct :: (Category cat) => x :<-: cat a c -> cat a b -> x :<-: cat b c
>categoryAct act m = act =*= (. m)

>cocategoryAct :: (Category cat) => x :<-: cat a c -> cat b c -> x :<-: cat a b
>cocategoryAct act m = act =*= (>>> m)

>functorAct :: (Functor f) => x :<-: f b -> (a -> b) -> x :<-: f a
>functorAct act f = act =*= fmap f
>
>cofunctorAct :: (CoFunctor p) => x :<-: p a -> (a -> b) -> x :<-: p b
>cofunctorAct act f = act =*= inverseImage f

>pureAct :: (Applicative f) => x :<-: f a -> x :<-: a
>pureAct act = act =*= pure
>
>applicativeAct :: (Applicative f) => x :<-: f b -> f (a -> b) -> x :<-: f a
>applicativeAct act f = act =*= (f <*>)

>pairAct :: (x :<-: (c,c')) -> (b -> c, b' -> c') -> x :<-: (b,b')
>pairAct act (f,g) = act =*= (f *** g)

>productAct :: (x :<-: (c,c')) -> (a -> c, a -> c') -> x :<-: a
>productAct act (f,g) = act =*= (f &&& g)

>eitherAct :: x :<-: b -> (a -> b, c -> b) -> x :<-: Either a c
>eitherAct act (f,g) = act =*= either f g

>maybeAct :: x :<-: b -> (b, a -> b) -> x :<-: Maybe a
>maybeAct act (x,f) = act =*= maybe x f

>justAct :: x :<-: Maybe a -> x :<-: a
>justAct act = act =*= Just

>nattransAct :: x :<-: g a -> f :~> g -> x :<-: f a
>nattransAct act x = act =*= nattransComponent x

>horizAct :: (Functor h) => h :~> k -> f :~> g -> x :<-: (k :*: g) a -> x :<-: (h :*: f) a
>horizAct f g = (`nattransAct` horiz f g)
>
>vertAct :: g :~> h -> f :~> g -> x :<-: h a -> x :<-: f a
>vertAct f g = (`nattransAct` vert f g)

>counitAct :: (Adjunction f g) => x :<-: a -> x :<-: (f :*: g) a
>counitAct x = ((x =*= unI) `nattransAct` counitTrans)

>unitAct :: (Adjunction f g) => x :<-: (g :*: f) a -> x :<-: a
>unitAct x = (x `nattransAct` unitTrans) =*= I

>returnAct :: (Monad m) => x :<-: m a -> x :<-: a
>returnAct act = act =*= return
> 
>monadAct :: (Monad m) => x :<-: m b -> (a -> m b) -> x :<-: m a
>monadAct act f = act =*= (>>= f)

>smooth :: (x -> y) -> (:<-:) x :~> (:<-:) y
>smooth f = NatTrans $ \(Action x) -> Action $ f . x

>actionMember :: a -> x :<-: a -> x
>actionMember x (Action f) = f x

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
>   relation f = Action $ \ (x,y) -> y `actionMember` f x
>   unrelation p x = Action $ \y -> (x,y) `actionMember` p

>instance HasEqualizers ((:<-:) Bool) (Bool :<-: Bool) where
>   equalizer f g = Action $ \x -> runAction (f x !==! g x) ()

>toProp :: Bool :<-: x -> Prop x
>toProp (Action f) = Characteristic f

>fromProp :: Prop x -> Bool :<-: x
>fromProp (Characteristic f) = Action f

>infix 4 !==!

>(!==) :: (Eq a) => a :<-: x -> a :<-: x -> Bool :<-: x
>(!==) (Action f) (Action g) = Action $ \e -> f e == g e

>-- | performance problem if @a@ in @x :<-: a@ is large!
>(!==!) :: (Universe a, Eq x) => x :<-: a -> x :<-: a -> Bool :<-: ()
>(!==!) (Action f) (Action g) = Action $ \ () -> and $ map (\a -> f a == g a) allElements
  
>-- | separator f g
>instance (Universe a, Eq x) => Eq (x :<-: a) where
>   x == y = runAction (x !==! y) ()

>actionCompose :: t' :<-: t -> t :<-: t'' -> t' :<-: t''
>actionCompose (Action f) (Action g) = Action (f . g)

>actionImage :: (x -> y) -> x :<-: a -> y :<-: a
>actionImage f (Action x) = Action (f . x)

>actionProduct :: a :<-: a' -> b :<-: b' -> (a,b) :<-: (a',b')
>actionProduct (Action f) (Action g) = Action (\ (a',b') -> (f a',g b'))

>actionCoproduct :: a :<-: a' -> b :<-: b' ->(Either a b) :<-: (Either a' b')
>actionCoproduct (Action f) (Action g) = Action (either (Left . f) (Right . g))

>yoneda :: x :<-: c -> ((:<-:) c) :~> ((:<-:) x)
>yoneda f = NatTrans (actionCompose f)

>unyoneda :: ((:<-:) c) :~> ((:<-:) x) -> x :<-: c
>unyoneda (NatTrans f) = f id

>chaotic :: (Category cat) => cat b c -> (cat a c -> x) -> x :<-: (cat a b)
>chaotic a f = Action (\t -> f (a . t))

>action :: x :<-: c -> c :<-: c' -> ((:<-:) c') :~> ((:<-:) x)
>action x y = yoneda x `vert` yoneda y

>stackAction :: x :<-: c -> x' :<-: c'
>             -> ((:<-:) c :*: (:<-:) x') :~> ((:<-:) x :*: (:<-:) c')
>stackAction x y = yoneda x `inverseHoriz` yoneda y

