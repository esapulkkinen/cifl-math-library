>{-# LANGUAGE Safe,ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeFamilies, TypeOperators, GADTs, LambdaCase #-}
>module Math.Tools.Prop where
>import Prelude
>import qualified Control.Monad.Fix as Fix
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.Map (Map)
>import qualified Data.Map as Map
>import qualified Control.Category as Cat
>import Math.Tools.CoFunctor
>import Math.Tools.Universe
>import Math.Tools.Adjunction (swap)
>import Math.Tools.Visitor
>import Math.Tools.Show
>import Math.Tools.Median
>import Math.Tools.Arrow
>import Math.Matrix.Matrix
>import Math.Matrix.Interface

>-- | We must represent classical logic with characteristic functions,
>-- because of the excluded middle axiom, and because characteristic
>-- functions (as properties) have complex relationship with possible
>-- elements of 'a'.  See the Universe type class for a solution.
>-- For exposition of classical logic used here,
>-- see "Lawvere,Rosebrugh: Sets for mathematics".
>data Prop a = Characteristic { runCharacteristic :: a -> Bool }

>instance (Universe a) => Ord (Prop a) where { (<=) = subseteq }

>subseteq :: (Universe a) => Prop a -> Prop a -> Bool
>subseteq f (Characteristic g) = and $ map g (enumerate f)

A `intersects` B == (A `intersect` B != {})

>intersects :: (Universe a) => Prop a -> Prop a -> Bool
>intersects f (Characteristic g) = or $ map g (enumerate f)

>-- | See "Soare: Recursively enumerable sets and degrees", this is split
>-- of recursive into two recursively enumerable sets.

>split_prop :: (Universe a) => Prop a -> ([a],[a])
>split_prop (Characteristic f) = (filter f         all_elements,
>                            filter (not . f) all_elements)

>propositional_table :: (Universe a) => Prop a -> [(a,Bool)]
>propositional_table p = [(x,runCharacteristic p x) | x <- all_elements]

>instance BinaryLogic Prop where
>  toBool (Characteristic f) = f ()

>fromBool :: Bool -> Prop a
>fromBool = Characteristic Cat.. const

>instance CoFunctor Prop where
>   inverse_image f (Characteristic g) = Characteristic (g Cat.. f)

reflexive_and_transitive_frames = characteristic (uncurry system_S4)
reflexive_frames  = characteristic (uncurry system_T)
frames = characteristic (uncurry system_K)
serial_frames = characteristic (uncurry system_D)

>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_N :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom_N p = p -=>- always p
>
>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_K :: (ModalLogic p, ImplicativeLogic p) => p a -> p a -> p a
>axiom_K p q = always (p -=>- q) -=>- (always p -=>- always q)

>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_4 :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom_4 p = always p -=>- always (always p)

>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_B :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom_B p = p -=>- always (eventually p)

>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_D :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom_D p = always p -=>- eventually p
>
>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_5 :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom_5 p = eventually p -=>- always (eventually p)
>
>-- | https://en.wikipedia.org/wiki/Modal_logic
>axiom_T :: (ModalLogic p, ImplicativeLogic p) => p a -> p a
>axiom_T p = always p -=>- p

>system_K p q = axiom_K p q -&- axiom_N p -&- axiom_N q
>system_T p q = system_K p q -&- axiom_T p -&- axiom_T q
>system_S4 p q = system_T p q -&- axiom_4 p -&- axiom_4 q
>system_S5 p q = system_S4 p q -&- axiom_5 p -&- axiom_5 q
>system_D p q = system_K p q -&- axiom_D p -&- axiom_D q

>                       


>instance (Universe a) => Eq (Prop a) where
>  (Characteristic p) == (Characteristic q) = and [p x == q x | x <- all_elements]

>instance (Universe a,Eq a) => Universe (Prop a) where
>   all_elements = map Characteristic all_elements

>instance (Show a, Universe a) => Show (Prop a) where
>   showsPrec _ (Characteristic (f :: a -> Bool)) = punctuate_list ";" $  map (\ a -> show a ++ "->" ++ show (f a)) $ all_elements

>      
>instance (Eq c) => HasEqualizers Prop c where
>   equalizer f g = Characteristic $ \x -> f x == g x

>vector_equalizer :: (Eq (n a), LinearTransform m n a)
>                 => (m :*: n) a -> (m :*: n) a -> Prop (m a)
>vector_equalizer a b = Characteristic $ \v -> a <<*> v == b <<*> v

>matrix_equalizer :: (Eq (Scalar (h a)),
>                     InnerProductSpace (h a),
>                     Transposable h f,
>                     Applicative g, Applicative f,
>                     Foldable g, Foldable f)
>                 => (g :*: h) a -> (g :*: h) a -> Prop ((h :*: f) a)
>matrix_equalizer a b = Characteristic $ \v -> a %*% v == b %*% v

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
>list_truthvalues (Characteristic f) = map (\x -> (x,f x)) all_elements

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

>existential_bind :: (Universe a) => Prop a -> (a -> Prop b) -> Prop b
>existential_bind f g = union_list $ map g (enumerate f)

>existential_join :: (Universe b,Eq b) => Prop (Prop b) -> Prop b
>existential_join = union_list . enumerate

>universal_bind :: (Universe a) => Prop a -> (a -> Prop b) -> Prop b
>universal_bind f g = intersect_list $ map g (enumerate f)

>universal_join :: (Universe b, Eq b) => Prop (Prop b) -> Prop b
>universal_join = intersect_list . enumerate

>instance PropositionalLogic Prop where
>   (-&-) = intersect
>   (-|-) = union
>   invert (Characteristic f) = Characteristic (not . f)

>instance BooleanLogic Prop where
>   true = truthvalue True

>instance SubtractiveLogic Prop where
>   (-\-) = relative_complement 

>instance ImplicativeLogic Prop where
>   (-=>-) = propositional_implication

>instance Propositional Prop where
>   runProposition = \p x -> fromBool $ runCharacteristic p x


>prop_fixedpoint :: (Prop a -> Prop a) -> Prop a
>prop_fixedpoint = Fix.fix

>truthvalue :: Bool -> Prop a
>truthvalue = Characteristic . const

>-- | Satisfiable returns true if there exists an element of the universe
>-- such that the proposition is satisfied. Note symmetry with 'axiom'.

>satisfiable :: (Universe a) => Prop a -> Bool
>satisfiable = toBool Cat.. terminal_part Cat.. prop_existential Cat.. prop_iso_terminal_inverse

>-- | axiom returns true, if the proposition holds for all elements of the
>-- universe.
>axiom :: (Universe a) => Prop a -> Bool
>axiom = toBool Cat.. terminal_part Cat.. prop_universal Cat.. prop_iso_terminal_inverse

>notMember :: a -> Prop a -> Bool
>notMember x (Characteristic p) = not (p x)

>-- | \[x \in subseteq i j \iff x \in i \implies x \in j\]
>subseteq_prop :: Prop a -> Prop a -> Prop a
>subseteq_prop i j = Characteristic (\x -> if toBool (x `member` i) then toBool (x `member` j) else True) 

>is_section :: Prop a -> (Bool -> a) -> Bool
>is_section p g = toBool ((g True ) `member` p) && ((g False) `notMember` p)

>doublePropJoin :: Prop (Prop (Prop (Prop a))) -> Prop (Prop a)
>doublePropJoin (Characteristic f) = Characteristic $ 
>           \pa -> f $ Characteristic $
>              \ ppa -> runCharacteristic ppa pa

>instance UniversalLogic Prop where
>  singleton x = Characteristic $ \y -> x == y
>  all_members = Characteristic $ const True

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
>  Characteristic $ \ (a,c) -> or $ map (\b -> f (a,b) && g (b,c)) all_elements

>-- | \[<x,z> \in transitive \mathbb{R} \iff (\forall y. <x,y> \in \mathbb{R} & <y,z> \in \mathbb{R} \implies <x,z> \in \mathbb{R})\]

>transitive :: (Universe a) => Prop (a,a) -> Bool
>transitive r = prop_compose r r <= r

>antisymmetric :: (Eq a) => Prop (a,a) -> Prop (a,a)
>antisymmetric p = (p -&- opposite p) -=>- reflexive

>prop_iso_terminal :: Prop ((),b) -> Prop b
>prop_iso_terminal (Characteristic f) = Characteristic (\b -> f ((),b))

>prop_iso_terminal_inverse :: Prop b -> Prop ((),b)
>prop_iso_terminal_inverse (Characteristic f) = Characteristic (\ ((),x) -> f x)

>-- | \[fiber(p,y) = { x | p(x) = y } = p^{-1}({y})\]
>fiber :: (HasEqualizers Prop c) => (b -> c) -> c -> Prop b
>fiber p y = prop_iso_terminal $ pullback (const y) p

>fiber_ :: (Eq b) => (a -> b) -> b -> Prop a
>fiber_ p y = inverse_image p (result y)

An equivalence relation is reflexive, transitive and symmetric
(this is not apparent from the definition, but it's true).

>equivalence_relation :: (HasEqualizers Prop b) => (a -> b) -> Prop (a,a)
>equivalence_relation f = pullback f f

>reflexive :: (Eq a) => Prop (a,a)
>reflexive = binary_rel_prop (==)

>reflexive_closure :: (Eq a) => Prop (a,a) -> Prop (a,a)
>reflexive_closure p = p -|- reflexive

>symmetric_closure :: Prop (a,a) -> Prop (a,a)
>symmetric_closure p = p -|- opposite p

>transitive_step :: (Universe a) => Prop (a,a) -> Prop (a,a)
>transitive_step p = Characteristic $ \ ~(a,c) ->
>     or [toBool ((a,b) `member` p) && toBool ((b,c) `member` p) | b <- all_elements]

>transitive_closure :: (Universe a) => Prop (a,a) -> Prop (a,a)
>transitive_closure p = p `union` transitive_closure (transitive_step p)

>equivalence_generated_by :: (Eq a,Universe a) => Prop (a,a) -> Prop (a,a)
>equivalence_generated_by = transitive_closure Cat.. symmetric_closure Cat.. reflexive_closure

>opposite :: Prop (a,b) -> Prop (b,a)
>opposite = inverse_image swap


>-- | Adjunction \[P^{op} \dashv P\] between functors \[P\] and \[P^{op}\],
>-- where \[P : C^{op} \Rightarrow C\] and \[P^{op} : C \Rightarrow C^{op}\].
>-- This is based on symmetry of product in relations.

>leftAdjunct_prop :: (a -> Prop b) -> b -> Prop a
>leftAdjunct_prop = unrelation Cat.. opposite Cat.. relation

>unit_prop :: b -> Prop (Prop b)
>unit_prop = leftAdjunct_prop Cat.id

>-- | rightAdjunct of that adjunction is difficult, because it would be in
>-- C^{op}, and we don't have that category here. So how could we
>-- implement that? We'd need opposite arrow, but then the implementation
>-- would be exactly same as leftAdjunct_prop and unit_prop, but lifted to
>-- opposite category. I'm not implementing that now.

>rel_prop :: (a -> Bool) -> Prop a
>rel_prop = Characteristic

>binary_rel_prop :: (a -> a -> Bool) -> Prop (a,a)
>binary_rel_prop = Characteristic Cat.. uncurry

>-- | <http://nlab.mathforge.org/nlab/show/well-founded+relation>

>binary_rel :: (a -> a -> Bool) -> a -> Prop a
>binary_rel f x = Characteristic (\y -> f x y)

>equal_functions :: (Universe a, Eq b) => Prop (a -> b, a -> b)
>equal_functions = binary_rel_prop separator

>test_equality_at :: a -> Prop (Prop a, Prop a)
>test_equality_at x = Characteristic $ \ (p,q) -> runCharacteristic p x == runCharacteristic q x

>equalizer_prop :: Prop a -> Prop a -> Prop a
>equalizer_prop (Characteristic f) (Characteristic g) = equalizer f g

>pullback_prop :: Prop a -> Prop b -> Prop (a,b)
>pullback_prop (Characteristic f) (Characteristic g) = pullback f g

>sum :: Prop a -> Prop b -> Prop (Either a b)
>sum (Characteristic f) (Characteristic g) = Characteristic (either f g)

>prop_and :: Prop (Either a b) -> Prop (a,b)
>prop_and (Characteristic f) = Characteristic $ \(x,y) -> f (Left x) && f (Right y)

>prop_or :: Prop (Either a b) -> Prop (a,b)
>prop_or (Characteristic f) = Characteristic $ \ (x,y) -> f (Left x) || f (Right y)

>prop_if :: Prop a -> Prop (Either a a) -> Prop a
>prop_if (Characteristic f) (Characteristic g)
>    = Characteristic $ \ x -> if f x then g (Left x) else g (Right x)

>prop_if_general :: Prop a -> Prop (Either b c) -> Prop (a,b,c)
>prop_if_general (Characteristic f) (Characteristic g)
>   = Characteristic $ \ (x,y,z) -> if f x then g (Left y) else g (Right z)


>equal :: (Eq a) => Prop (a,a)
>equal = binary_rel_prop (==)

>not_equal :: (Eq a) => Prop (a,a)
>not_equal = binary_rel_prop (/=)

>lessthan :: (Ord a) => Prop (a,a)
>lessthan = binary_rel_prop (<)

>lesseq :: (Ord a) => Prop (a,a)
>lesseq = binary_rel_prop (<=)

>or_list_prop :: Prop a -> Prop [a]
>or_list_prop (Characteristic f) = Characteristic (\lst -> or $ map f lst)

>and_list_prop :: Prop a -> Prop [a]
>and_list_prop (Characteristic f) = Characteristic (\lst -> and $ map f lst)

>enumerate :: (Universe a) => Prop a -> [a]
>enumerate (Characteristic p) = filter p all_elements



that 'existential_map(f)' is a functor implies that
p |--> existential_map f p    preserves order

existential_map f p = { b | p `intersect` f^-1({b}) }

>existential_map :: (Universe a, Eq b) => (a -> b) -> Prop a -> Prop b
>existential_map f p = Characteristic $ \b -> p `intersects` fiber f b

>-- | \[universal\_map( f, p ) = \{ b | p <= f^{-1}(\{b\}) \}\]


>universal_map :: (Universe a, Eq b) => (a -> b) -> Prop a -> Prop b
>universal_map f p = Characteristic $ \b -> p <= fiber f b
                                            
>-- | prop_existential is basically the same as 'existential_map fst',
>-- except for performance. 'existential_map fst' will need to iterate
>-- over all pairs \[<a,b>\], whereas prop_existential will only iterate
>-- through all elements of b only. Similarly for prop_universal.

>prop_existential :: (Universe b) => Prop (a,b) -> Prop a
>prop_existential (Characteristic rel) = 
>   Characteristic $ \a -> or $ map (\b -> rel (a,b)) all_elements

>prop_universal :: (Universe b) => Prop (a,b) -> Prop a
>prop_universal (Characteristic rel) = 
>   Characteristic $ \a -> and $ map (\b -> rel (a,b)) all_elements

>image :: (Universe a) => Prop (a,b) -> Prop b
>image (Characteristic rel) = 
>   Characteristic $ \b -> or $ map (\a -> rel (a,b)) all_elements

>prop_universal_image :: (Universe a, Universe b, Eq b) => Prop (a,b) -> Prop b
>prop_universal_image (Characteristic rel) 
>   = Characteristic $ \b -> and $ map (\a -> rel (a,b)) all_elements

>instance Existential Prop where
>   direct_image = existential_map
>   universal_image    = universal_map

>forevery :: (Universe b) => Prop b -> Prop ()
>forevery = prop_universal Cat.. prop_iso_terminal_inverse

>data (b :\: a) = PSub { runpsub :: PProp a -> PProp b }

>data PProp a where
>   PProp :: Prop a -> PProp a
>   PNeg  :: (Prop a -> Prop b) -> PProp (b :\: a)

>pneg_func :: (a -> b) -> PProp (a :\: b)
>pneg_func f = PNeg $ \a -> inverse_image f a

>instance CoFunctor PProp where
>   inverse_image f (PProp p) = PProp $ inverse_image f p

   inverse_image f (PNeg g) = PNeg (g . inverse_image f)
