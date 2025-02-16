>{-# LANGUAGE Safe,ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeFamilies, TypeOperators, GADTs, LambdaCase, ConstraintKinds #-}
>module Math.Tools.Prop where
>import Prelude
>import qualified Control.Monad.Fix as Fix
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.Map (Map)
>import Data.Functor.Contravariant
>import qualified Data.Map as Map
>import qualified Control.Category as Cat
>import Math.Tools.CoFunctor
>import Math.Tools.Universe
>import Math.Tools.Adjunction (swap, Adjunction(..))
>import Math.Tools.Visitor
>import Math.Tools.Show
>import Math.Tools.Median
>import Math.Tools.Arrow
>import Math.Matrix.Interface

>-- | We must represent classical logic with characteristic functions,
>-- because of the excluded middle axiom, and because characteristic
>-- functions (as properties) have complex relationship with possible
>-- elements of 'a'.  See the Universe type class for a solution.
>-- For exposition of classical logic used here,
>-- see "Lawvere,Rosebrugh: Sets for mathematics".
>newtype Prop a = Characteristic { runCharacteristic :: a -> Bool }

>instance (Universe a) => Ord (Prop a) where { (<=) = subseteq }

prop_matrix :: (CoFunctor p, CoFunctor q) => p (q a) -> q b -> p (a -> b)
prop_matrix f = (|>>) f Prelude.. (|>>)

>subseteq :: (Universe a) => Prop a -> Prop a -> Bool
>subseteq f (Characteristic g) = and $ map g (enumerate f)

A `intersects` B == (A `intersect` B != {})

>intersects :: (Universe a) => Prop a -> Prop a -> Bool
>intersects f (Characteristic g) = or $ map g (enumerate f)

>-- | See "Soare: Recursively enumerable sets and degrees", this is split
>-- of recursive into two recursively enumerable sets.

>splitProp :: (Universe a) => Prop a -> ([a],[a])
>splitProp (Characteristic f) = (filter f         allElements,
>                            filter (not . f) allElements)

>propositionalTable :: (Universe a) => Prop a -> [(a,Bool)]
>propositionalTable p = [(x,runCharacteristic p x) | x <- allElements]

>instance BinaryLogic Prop where
>  toBool (Characteristic f) = f ()

>fromBool :: Bool -> Prop a
>fromBool = Characteristic Cat.. const

>instance Contravariant Prop where
>   contramap f (Characteristic g) = Characteristic (g Cat.. f)

>propSubstitute :: (Contravariant p) => p a -> p (a,b)
>propSubstitute = contramap fst

>propExists :: (Adjunction f p, Contravariant p) => f (p a) -> (a,b)
>propExists = rightAdjunct (contramap fst)
>
>propExistsSnd :: (Adjunction f p, Contravariant p) => f (p b) -> (a,b)
>propExistsSnd = rightAdjunct (contramap snd)

>propAlways :: (Adjunction p g, Contravariant p) => a -> g (p (a,b))
>propAlways = leftAdjunct (contramap fst)

>propAlwaysSnd :: (Adjunction p g, Contravariant p) => b -> g (p (a,b))
>propAlwaysSnd = leftAdjunct (contramap snd)

reflexive_and_transitive_frames = characteristic (uncurry system_S4)
reflexive_frames  = characteristic (uncurry system_T)
frames = characteristic (uncurry system_K)
serial_frames = characteristic (uncurry system_D)

>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiomN :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiomN p = p -=>- always p
>
>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiomK :: (ModalLogic p, ImplicativeLogic p) => p a -> p a -> p a
>axiomK p q = always (p -=>- q) -=>- (always p -=>- always q)

>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiom4 :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom4 p = always p -=>- always (always p)

>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiomB :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiomB p = p -=>- always (eventually p)

>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiomD :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiomD p = always p -=>- eventually p
>
>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiom5 :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom5 p = eventually p -=>- always (eventually p)
>
>-- | <https://en.wikipedia.org/wiki/Modal_logic>
>axiomT :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiomT p = always p -=>- p

>systemK p q = axiomK p q -&- axiomN p -&- axiomN q
>systemT p q = systemK p q -&- axiomT p -&- axiomT q
>systemS4 p q = systemT p q -&- axiom4 p -&- axiom4 q
>systemS5 p q = systemS4 p q -&- axiom5 p -&- axiom5 q
>systemD p q = systemK p q -&- axiomD p -&- axiomD q

>                       


>instance (Universe a) => Eq (Prop a) where
>  (Characteristic p) == (Characteristic q) = and [p x == q x | x <- allElements]

>instance (Universe a,Eq a) => Universe (Prop a) where
>   allElements = map Characteristic allElements

>instance (Show a, Universe a) => Show (Prop a) where
>   showsPrec _ (Characteristic (f :: a -> Bool)) = punctuate_list ";" $  map (\ a -> show a ++ "->" ++ show (f a)) $ allElements

>      
>instance (Eq c) => HasEqualizers Prop c where
>   equalizer f g = Characteristic $ \x -> f x == g x

>vectorEqualizer :: (Eq (n a), LinearTransform m n a)
>                 => (m :*: n) a -> (m :*: n) a -> Prop (m a)
>vectorEqualizer a b = Characteristic $ \v -> a <<*> v == b <<*> v

>instance OverCategory (->) a where
>   overmap g f = f . g

>instance Visitor (Prop a) where
>   data Fold (Prop a) b = forall c. PropFold [a] (a -> c) (a -> c) ([c] -> b)
>   visit (PropFold lst f g alts) (Characteristic p) = alts [if p x then f x else g x | x <- lst]

>instance Builder (Prop a) where
>   data Unfold (Prop a) b = PropUnfold (b -> a -> Bool)
>   build (PropUnfold g) x = Characteristic $ g x

>-- | It is impossible to convert characteristic index into recursively
>-- enumerable index (indexing of r.e. sets), or into canonical index
>-- (which has one bit per element describing membership). Thus we must
>-- use the Universe constraint.

>list_truthvalues :: (Universe a) => Prop a -> [(a,Bool)]
>list_truthvalues (Characteristic f) = map (\x -> (x,f x)) allElements

>-- | relation and unrelation represent the natural isomorphism
>-- \[Hom(-,P(A)) \cong Sub(- \times A)\]

>instance Relational Prop where
>    relation f = Characteristic $ \ (x,y) -> toBool $ y `member` f x
>    unrelation p x = Characteristic $ \y -> toBool $ (x,y) `member` p

>apply :: Prop (Prop b,b)
>apply = relation id

>-- | result and the bind operations provide Monad instance, except for the
>-- | fact that the Universe and Eq constraints are required. Thus an
>-- adjunction instance also exists.

>result :: (Eq a) => a -> Prop a
>result x = Characteristic (== x) 
>      -- == singleton
>      -- == unrelation reflexive

>existentialBind :: (Universe a) => Prop a -> (a -> Prop b) -> Prop b
>existentialBind f g = unionList $ map g (enumerate f)

>existentialJoin :: (Universe b,Eq b) => Prop (Prop b) -> Prop b
>existentialJoin = unionList . enumerate

>universalBind :: (Universe a) => Prop a -> (a -> Prop b) -> Prop b
>universalBind f g = intersectList $ map g (enumerate f)

>universalJoin :: (Universe b, Eq b) => Prop (Prop b) -> Prop b
>universalJoin = intersectList . enumerate

>instance PropositionalLogic Prop where
>   (-&-) = intersect
>   (-|-) = union
>   invert (Characteristic f) = Characteristic (not . f)

>instance BooleanLogic Prop where
>   true = truthvalue True

>instance SubtractiveLogic Prop where
>   (-\-) = relativeComplement 

>instance ImplicativeLogic Prop where
>   (-=>-) = propositionalImplication

>instance Propositional Prop where
>   runProposition p x = fromBool $ runCharacteristic p x


>propFixedpoint :: (Prop a -> Prop a) -> Prop a
>propFixedpoint = Fix.fix

>truthvalue :: Bool -> Prop a
>truthvalue = Characteristic . const

>-- | Satisfiable returns true if there exists an element of the universe
>-- such that the proposition is satisfied. Note symmetry with 'axiom'.

>satisfiable :: (Universe a) => Prop a -> Bool
>satisfiable = toBool Cat.. terminalPart Cat.. propExistential Cat.. propIsoTerminalInverse

>-- | axiom returns true, if the proposition holds for all elements of the
>-- universe.
>axiom :: (Universe a) => Prop a -> Bool
>axiom = toBool Cat.. terminalPart Cat.. propUniversal Cat.. propIsoTerminalInverse

>notMember :: a -> Prop a -> Bool
>notMember x (Characteristic p) = not (p x)

>-- | \[x \in subseteq i j \iff x \in i \implies x \in j\]
>subseteqProp :: Prop a -> Prop a -> Prop a
>subseteqProp i j = Characteristic (\x -> if toBool (x `member` i) then toBool (x `member` j) else True) 

>isSection :: Prop a -> (Bool -> a) -> Bool
>isSection p g = toBool ((g True ) `member` p) && ((g False) `notMember` p)

>doublePropJoin :: Prop (Prop (Prop (Prop a))) -> Prop (Prop a)
>doublePropJoin (Characteristic f) = Characteristic $ 
>           \pa -> f $ Characteristic $
>              \ ppa -> runCharacteristic ppa pa

>instance UniversalLogic Prop where
>  singleton x = Characteristic $ \y -> x == y
>  allMembers = Characteristic $ const True

>fromAssoc :: (Eq a) => [(a,Bool)] -> Prop a
>fromAssoc lst = Characteristic $ \v -> maybe False Cat.id (lookup v lst)

>fromAssocWith :: (Eq a) => [(a,b)] -> Prop b -> Prop a
>fromAssocWith lst (Characteristic p) 
>   = Characteristic $ \v -> maybe False p (lookup v lst)

>prop_fromList :: (Eq a) => [a] -> Prop a
>prop_fromList lst = Characteristic $ (`elem` lst)

>prop_fromSet :: (Ord a) => Set a -> Prop a
>prop_fromSet s = Characteristic (`Set.member` s)

>prop_fromMap :: (Ord i,Eq e) => Map i e -> Prop (i,e)
>prop_fromMap m = Characteristic $ \ (i,e) -> Just e == Map.lookup i m

>-- | Due to constraints, Prop cannot be made as instance of Functor.

>map_prop :: (Eq b, Universe a) => (a -> b) -> Prop a -> Prop b
>map_prop f = prop_fromList Cat.. map f Cat.. enumerate

>equalizer_fixedpoint :: (HasEqualizers p a) => (a -> a) -> p a
>equalizer_fixedpoint f = equalizer f Cat.id

>invertible :: (HasEqualizers p a) => (b -> a) -> (a -> b) -> p a
>invertible f g = equalizer_fixedpoint (f Cat.. g)

>kernel :: (Num c, HasEqualizers p c) => (a -> c) -> p a
>kernel f = equalizer f (const 0)

>ternary_op :: (Bool -> Bool -> Bool -> Bool)
>           -> Prop a -> Prop a -> Prop a -> Prop a
>ternary_op op (Characteristic f) (Characteristic g) (Characteristic h)
>    = Characteristic $ \x -> op (f x) (g x) (h x)

>binary_op :: (Bool -> Bool -> Bool) -> Prop a -> Prop a -> Prop a
>binary_op op (Characteristic f) (Characteristic g)
>   = Characteristic (\x -> f x `op` g x)

>gen_binary_op :: (b -> b' -> c) -> (a -> b) -> (a -> b') -> (a -> c)
>gen_binary_op op f g x = f x `op` g x

>binary :: (Relational p) => (a -> b -> p c) -> p (a,(b,c))
>binary f = relation $ \x -> relation $ \y -> f x y

The following two functions are the same function.

>evaluate :: a -> Prop (Prop a)
>evaluate x = Characteristic $ \ f -> runCharacteristic f x

>evaluate' :: (Universe a, Eq a) => a -> Prop (Prop a)
>evaluate' = integrate_prop Cat.. singleton

>integrate_prop :: (Universe a) => Prop a -> Prop (Prop a)
>integrate_prop a = Characteristic $ \p -> 
>                  or $ [runCharacteristic p x | x <- enumerate a]

>intersect :: Prop a -> Prop a -> Prop a
>intersect = binary_op (&&)

>intersect3 :: Prop a -> Prop a -> Prop a -> Prop a
>intersect3 = ternary_op (\x y z -> x && y && z)

>median3 :: Prop a -> Prop a -> Prop a -> Prop a
>median3 = ternary_op median

>union :: Prop a -> Prop a -> Prop a
>union = binary_op (||)

>union3 :: Prop a -> Prop a -> Prop a -> Prop a
>union3 = ternary_op (\x y z -> x || y || z)

>excluded_middle :: (Universe b) => Prop b -> Bool
>excluded_middle p = axiom (p -|- invert p)

>noncontradiction :: (Universe b) => Prop b -> Bool
>noncontradiction p = axiom (invert (p -&- invert p))

>demorgan_union :: (Universe a) => Prop a -> Prop a -> Bool
>demorgan_union p q = axiom (invert (p -|- q) 
>                       -<=>- (invert p -&- invert q))

>demorgan_intersect :: (Universe a) => Prop a -> Prop a -> Bool
>demorgan_intersect p q = axiom (invert (p -&- q)
>                         -<=>- (invert p -|- invert q))

>disjunction_commutative :: (Universe a) => Prop a -> Prop a -> Bool
>disjunction_commutative p q = axiom (p -&- q -<=>- q -&- p)

>conjunction_commutative :: (Universe a) => Prop a -> Prop a -> Bool
>conjunction_commutative p q = axiom (p -|- q -<=>- q -|- p)

>-- | \[graph(f) = [<x,y> | y = f(x)]\]
>graph :: (Eq b) => (a -> b) -> Prop (a,b)
>graph f = relation (singleton Cat.. f) -- == [(x,y)| y = f x ]

>image_factorization :: (Eq b, Universe a) => (a -> b) -> Prop b
>image_factorization = image Cat.. graph 

>bind :: (Universe b) => (a -> Prop b) -> (b -> Prop c) -> a -> Prop c
>bind x y = unrelation $ prop_compose (relation x) (relation y)

>prop_compose :: (Universe b) => Prop (a,b) -> Prop (b,c) -> Prop (a,c)
>prop_compose (Characteristic f) (Characteristic g) =
>  Characteristic $ \ (a,c) -> any (\b -> f (a,b) && g (b,c)) allElements

>-- | \[<x,z> \in transitive \mathbb{R} \iff (\forall y. <x,y> \in \mathbb{R} & <y,z> \in \mathbb{R} \implies <x,z> \in \mathbb{R})\]

>transitive :: (Universe a) => Prop (a,a) -> Bool
>transitive r = prop_compose r r <= r

>antisymmetric :: (Eq a) => Prop (a,a) -> Prop (a,a)
>antisymmetric p = (p -&- opposite p) -=>- reflexive

>propIsoTerminal :: Prop ((),b) -> Prop b
>propIsoTerminal (Characteristic f) = Characteristic (\b -> f ((),b))

>propIsoTerminalInverse :: Prop b -> Prop ((),b)
>propIsoTerminalInverse (Characteristic f) = Characteristic (\ ((),x) -> f x)

>-- | \[fiber(p,y) = { x | p(x) = y } = p^{-1}({y})\]
>fiber :: (HasEqualizers Prop c) => (b -> c) -> c -> Prop b
>fiber p y = propIsoTerminal $ pullback (const y) p

>fiber_ :: (Eq b) => (a -> b) -> b -> Prop a
>fiber_ p y = inverseImage p (result y)

An equivalence relation is reflexive, transitive and symmetric
(this is not apparent from the definition, but it's true).

>equivalenceRelation :: (HasEqualizers Prop b) => (a -> b) -> Prop (a,a)
>equivalenceRelation f = pullback f f

>reflexive :: (Eq a) => Prop (a,a)
>reflexive = binaryRelProp (==)

>reflexiveClosure :: (Eq a) => Prop (a,a) -> Prop (a,a)
>reflexiveClosure p = p -|- reflexive

>symmetricClosure :: Prop (a,a) -> Prop (a,a)
>symmetricClosure p = p -|- opposite p

>transitiveStep :: (Universe a) => Prop (a,a) -> Prop (a,a)
>transitiveStep p = Characteristic $ \ ~(a,c) ->
>     or [toBool ((a,b) `member` p) && toBool ((b,c) `member` p) | b <- allElements]

>transitiveClosure :: (Universe a) => Prop (a,a) -> Prop (a,a)
>transitiveClosure p = p `union` transitiveClosure (transitiveStep p)

>equivalenceGeneratedBy :: (Eq a,Universe a) => Prop (a,a) -> Prop (a,a)
>equivalenceGeneratedBy = transitiveClosure Cat.. symmetricClosure Cat.. reflexiveClosure

>opposite :: Prop (a,b) -> Prop (b,a)
>opposite = inverseImage swap


>-- | Adjunction \[P^{op} \dashv P\] between functors \[P\] and \[P^{op}\],
>-- where \[P : C^{op} \Rightarrow C\] and \[P^{op} : C \Rightarrow C^{op}\].
>-- This is based on symmetry of product in relations.

>leftAdjunctProp :: (a -> Prop b) -> b -> Prop a
>leftAdjunctProp = unrelation Cat.. opposite Cat.. relation

>unitProp :: b -> Prop (Prop b)
>unitProp = leftAdjunctProp Cat.id

>-- | rightAdjunct of that adjunction is difficult, because it would be in
>-- C^{op}, and we don't have that category here. So how could we
>-- implement that? We'd need opposite arrow, but then the implementation
>-- would be exactly same as leftAdjunct_prop and unit_prop, but lifted to
>-- opposite category. I'm not implementing that now.

>relProp :: (a -> Bool) -> Prop a
>relProp = Characteristic

>binaryRelProp :: (a -> a -> Bool) -> Prop (a,a)
>binaryRelProp = Characteristic Cat.. uncurry

>-- | <http://nlab.mathforge.org/nlab/show/well-founded+relation>

>binaryRel :: (a -> a -> Bool) -> a -> Prop a
>binaryRel f x = Characteristic (\y -> f x y)

>equalFunctions :: (Universe a, Eq b) => Prop (a -> b, a -> b)
>equalFunctions = binaryRelProp separator

>testEqualityAt :: a -> Prop (Prop a, Prop a)
>testEqualityAt x = Characteristic $ \ (p,q) -> runCharacteristic p x == runCharacteristic q x

>equalizerProp :: Prop a -> Prop a -> Prop a
>equalizerProp (Characteristic f) (Characteristic g) = equalizer f g

>pullbackProp :: Prop a -> Prop b -> Prop (a,b)
>pullbackProp (Characteristic f) (Characteristic g) = pullback f g

>sum :: Prop a -> Prop b -> Prop (Either a b)
>sum (Characteristic f) (Characteristic g) = Characteristic (either f g)

>propAnd :: Prop (Either a b) -> Prop (a,b)
>propAnd (Characteristic f) = Characteristic $ \(x,y) -> f (Left x) && f (Right y)

>propOr :: Prop (Either a b) -> Prop (a,b)
>propOr (Characteristic f) = Characteristic $ \ (x,y) -> f (Left x) || f (Right y)

>propIf :: Prop a -> Prop (Either a a) -> Prop a
>propIf (Characteristic f) (Characteristic g)
>    = Characteristic $ \ x -> if f x then g (Left x) else g (Right x)

>propIfGeneral :: Prop a -> Prop (Either b c) -> Prop (a,b,c)
>propIfGeneral (Characteristic f) (Characteristic g)
>   = Characteristic $ \ (x,y,z) -> if f x then g (Left y) else g (Right z)


>equal :: (Eq a) => Prop (a,a)
>equal = binaryRelProp (==)

>notEqual :: (Eq a) => Prop (a,a)
>notEqual = binaryRelProp (/=)

>lessthan :: (Ord a) => Prop (a,a)
>lessthan = binaryRelProp (<)

>lesseq :: (Ord a) => Prop (a,a)
>lesseq = binaryRelProp (<=)

>orListProp :: Prop a -> Prop [a]
>orListProp (Characteristic f) = Characteristic (any f)

>andListProp :: Prop a -> Prop [a]
>andListProp (Characteristic f) = Characteristic (all f)

>enumerate :: (Universe a) => Prop a -> [a]
>enumerate (Characteristic p) = filter p allElements



that 'existential_map(f)' is a functor implies that
p |--> existential_map f p    preserves order

existential_map f p = { b | p `intersect` f^-1({b}) }

>existentialMap :: (Universe a, Eq b) => (a -> b) -> Prop a -> Prop b
>existentialMap f p = Characteristic $ \b -> p `intersects` fiber f b

>-- | \[universal\_map( f, p ) = \{ b | p <= f^{-1}(\{b\}) \}\]


>universalMap :: (Universe a, Eq b) => (a -> b) -> Prop a -> Prop b
>universalMap f p = Characteristic $ \b -> p <= fiber f b
                                            
>-- | prop_existential is basically the same as 'existential_map fst',
>-- except for performance. 'existential_map fst' will need to iterate
>-- over all pairs \[<a,b>\], whereas prop_existential will only iterate
>-- through all elements of b only. Similarly for prop_universal.

>propExistential :: (Universe b) => Prop (a,b) -> Prop a
>propExistential (Characteristic rel) = 
>   Characteristic $ \a -> any (\b -> rel (a,b)) allElements

>propUniversal :: (Universe b) => Prop (a,b) -> Prop a
>propUniversal (Characteristic rel) = 
>   Characteristic $ \a -> all (\b -> rel (a,b)) allElements

>image :: (Universe a) => Prop (a,b) -> Prop b
>image (Characteristic rel) = 
>   Characteristic $ \b -> any (\a -> rel (a,b)) allElements

>propUniversalImage :: (Universe a, Universe b, Eq b) => Prop (a,b) -> Prop b
>propUniversalImage (Characteristic rel) 
>   = Characteristic $ \b -> all (\a -> rel (a,b)) allElements

>instance Existential Prop where
>   directImage = existentialMap
>   universalImage    = universalMap

>forevery :: (Universe b) => Prop b -> Prop ()
>forevery = propUniversal Cat.. propIsoTerminalInverse

>newtype (b :\: a) = PSub { runpsub :: PProp a -> PProp b }

>data PProp a where
>   PProp :: Prop a -> PProp a
>   PNeg  :: (Prop a -> Prop b) -> PProp (b :\: a)

>pnegFunc :: (a -> b) -> PProp (a :\: b)
>pnegFunc f = PNeg $ \a -> inverseImage f a

>instance Contravariant PProp where
>   contramap f (PProp p) = PProp $ inverseImage f p

   inverse_image f (PNeg g) = PNeg (g . inverse_image f)
