>{-# LANGUAGE Safe,TypeOperators, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, ConstraintKinds, PolyKinds #-}
>module Math.Tools.CoFunctor where
>import safe Math.Tools.I
>import safe Math.Tools.Universe
>import Data.Functor.Contravariant

>-- | For exposition of classical logic used here,
>-- see "Lawvere,Rosebrugh: Sets for mathematics".
>-- 
>-- > (x |>> f) |>> g == x |>> (f . g)
>-- > inverse_image g (inverse_image f x) == inverse_image (f . g) x
>-- Notice, CoFunctor is nowadays in base, so
>-- please use Data.Functor.Contravariant.Contravariant
>type CoFunctor p = Contravariant p

>inverseImage :: (CoFunctor p) => (a -> b) -> p b -> p a
>inverseImage = contramap
> 
>(|>>) :: (CoFunctor p) => p b -> (a -> b) -> p a
>(|>>) = flip contramap

class (Contravariant p) => CoFunctor p where
   inverse_image :: (a -> b) -> p b -> p a
   (|>>) :: p b -> (a -> b) -> p a
   (|>>) = flip inverse_image
   inverse_image = contramap

>class Representable p where
>   type Representation p :: * -> *
>   represent :: Representation p a -> p a
>   (!>) :: p a -> (Representation p a -> p a) -> p a

>class Propositional p where
>   runProposition :: p a -> a -> p ()

>class (Propositional p) => BinaryLogic p where
>   toBool :: p () -> Bool

>class (CoFunctor p) => Relational p where
>   relation   :: (a -> p b) -> p (a,b)
>   unrelation :: p (a,b)    -> a -> p b

direct_image(f) -| inverse_image(f) -| universal(f)

>class (CoFunctor p) => Existential p where
>   directImage :: (Universe a,Eq b) => (a -> b) -> p a -> p b
>   universalImage   :: (Universe a,Eq b) => (a -> b) -> p a -> p b


>class (CoFunctor p) => Difference p minus | p -> minus where
>   difference :: (p a -> p b) -> p (b `minus` a)
>   undifference :: p (b `minus` a) -> p a -> p b

>class (Eq c, CoFunctor p) => HasEqualizers p c where
>   -- minimal complete definition: equalizer
>   equalizer :: (a -> c) -> (a -> c) -> p a
>   pullback  :: (a -> c) -> (b -> c) -> p (a,b)
>   pullback f g = equalizer (f . fst) (g . snd)

>class (Functor p) => Topology p where
>   arbitraryUnion  :: [p a] -> p a
>   finiteIntersect :: [p a] -> p a

>class HasTerminalObject t where
>   terminalArrow :: a -> t

>class (CoFunctor p) => HasVariables p t where
>   variable :: String -> p t

>class (CoFunctor p) => PropositionalLogic p where
>   (-&-) :: p a -> p a -> p a
>   (-|-) :: p a -> p a -> p a
>   invert  :: p a -> p a

>-- | <https://en.wikipedia.org/wiki/Modal_logic>

>class (PropositionalLogic p) => ModalLogic p where
>   eventually :: p a -> p a
>   always     :: p a -> p a
>   always     = invert . eventually . invert
>   eventually = invert . always     . invert

>class (CoFunctor p) => Continuous p where
>   expand :: p (Either a b) -> (p a, p b)

>class (CoFunctor p) => SubtractiveLogic p where
>   (-\-) :: p a -> p a -> p a

>class (CoFunctor p) => ImplicativeLogic p where
>   (-=>-) :: p a -> p a -> p a

>class (CoFunctor p) => BooleanLogic p where
>   true   :: p a

>class (CoFunctor p) => UniversalLogic p where
>   singleton   :: (Eq a) => a -> p a
>   allMembers :: (Universe a) => p a

>class (CoFunctor p) => Universal p where
>   exist :: (Universe b) => p (a,b) -> p a
>   univ  :: (Universe b) => p (a,b) -> p a

>member :: (Propositional p) => a -> p a -> p ()
>member x p = runProposition p x

>isIn :: (BinaryLogic p) => a -> p a -> Bool
>isIn x p = toBool $ x `member` p

>isNotIn :: (BinaryLogic p) => a -> p a -> Bool
>isNotIn x p = not $ x `isIn` p

>terminalPart :: (Propositional p) => p () -> p ()
>terminalPart p = runProposition p ()

>propositionalImplication :: (PropositionalLogic p) => p a -> p a -> p a
>propositionalImplication f g = (invert f) -|- g

>relativeComplement :: (PropositionalLogic p) => p a -> p a -> p a
>relativeComplement f g = f -&- invert g

>intersectList :: (BooleanLogic p, PropositionalLogic p) => [p a] -> p a
>intersectList = foldr (-&-) true

>unionList :: (BooleanLogic p, PropositionalLogic p) => [p a] -> p a
>unionList = foldr (-|-) falseProp

>characteristic :: (BooleanLogic p) => (a -> p ()) -> p a
>characteristic f = inverseImage f true

false :: (BooleanLogic p) => p (p ())
false = characteristic not

>falseProp :: (BooleanLogic p, PropositionalLogic p) => p a
>falseProp = invert true

>(-<=>-) :: (ImplicativeLogic p, PropositionalLogic p) => p a -> p a -> p a
>p -<=>- q = (p -=>- q) -&- (q -=>- p)

>symmetricDifference :: (PropositionalLogic p, SubtractiveLogic p) => p a -> p a -> p a
>symmetricDifference x y = (x -\- y) -|- (y -\- x)

The boundary is not useful in propositional logic, this produces a
proposition that is always false. true -\- p == invert p,
so boundary p == p -&- invert p == false

>boundary :: (PropositionalLogic p, BooleanLogic p, SubtractiveLogic p)
>            => p a -> p a
>boundary p = p -&- (true -\- p)

>currySubst :: (CoFunctor p) => ((a,b) -> c) -> p (b -> c) -> p a
>currySubst = inverseImage . curry

>uncurrySubst :: (CoFunctor p) => (a -> b -> c) -> p c -> p (a,b)
>uncurrySubst = inverseImage . uncurry

>dualize :: (Difference p minus) => (a -> b) -> p (a `minus` b)
>dualize = difference . inverseImage

>coin1 :: (CoFunctor p) => p a -> p (a,b)
>coin1 = inverseImage fst

>coin2 :: (CoFunctor p) => p b -> p (a,b)
>coin2 = inverseImage snd

>cofst :: (CoFunctor p) => p (Either a b) -> p a
>cofst = inverseImage Left

>cosnd :: (CoFunctor p) => p (Either a b) -> p b
>cosnd = inverseImage Right

>cocase :: (CoFunctor p) => (Either a b -> c) -> p c -> (p a, p b)
>cocase f x = (inverseImage (f . Left) x, inverseImage (f . Right) x)

>copair :: (Propositional p, BooleanLogic p) => p a -> p b -> p (Either a b)
>copair f g = characteristic $ either (runProposition f) (runProposition g)

>pairSubst :: (CoFunctor p) => (a -> b) -> (a -> c) -> p (b,c) -> p a
>pairSubst f g = inverseImage (\x -> (f x,g x))

>graphSubst :: (CoFunctor p) => (a -> b) -> p (a,b) -> p a
>graphSubst f = inverseImage (\x -> (x,f x))

>instance HasTerminalObject () where
>   terminalArrow _ = ()

>instance Representable IO where
>   type Representation IO = I
>   represent = return . unI
>   m !> f = m >>= (f . I)
>
>instance Representable [] where
>   type Representation [] = I
>   represent = return . unI
>   m !> f = m >>= (f . I)

>data Assertion a = PrimAssert { assertedCondition :: a -> Bool }
>                 | NotAssert { assertedNegations :: Assertion a }
>                 | AndAssert { assertedConjunctions :: [Assertion a] }
>                 | OrAssert { assertedDisjunctions :: [Assertion a] }
> 
>instance ImplicativeLogic Assertion where
>   f -=>- g = OrAssert [NotAssert f, g]

>instance Contravariant Assertion where
>  contramap f (PrimAssert p) = PrimAssert (p . f)
>  contramap f (NotAssert x) = NotAssert (contramap f x)
>  contramap f (AndAssert lst) = AndAssert $ map (contramap f) lst
>  contramap f (OrAssert lst)  = OrAssert $  map (contramap f) lst

>instance PropositionalLogic Assertion where
>  p1 -&- p2 = AndAssert [p1,p2]
>  p1 -|- p2 = OrAssert [p1,p2]
>  invert = NotAssert

>instance BooleanLogic Assertion where
>   true = PrimAssert $ const True

>instance Propositional Assertion where
>  runProposition (PrimAssert p) x = PrimAssert $ const (p x)
>  runProposition (NotAssert p) x = NotAssert (runProposition p x)
>  runProposition (AndAssert lst) x = AndAssert $ map (`runProposition` x) lst
>  runProposition (OrAssert lst) x = OrAssert $ map (`runProposition` x) lst

>instance BinaryLogic Assertion where
>  toBool (PrimAssert p) = p ()
>  toBool (NotAssert p) = not (toBool p)
>  toBool (AndAssert lst) = all toBool lst
>  toBool (OrAssert lst) = any toBool lst
