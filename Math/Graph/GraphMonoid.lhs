>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
>{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GADTs #-}
>{-# LANGUAGE UndecidableInstances, KindSignatures, ConstraintKinds #-}
>{-# LANGUAGE Arrows, FlexibleContexts #-}
>module Math.Graph.GraphMonoid where
>import Control.Arrow
>import qualified Control.Category as Cat
>import GHC.Generics
>import Data.Typeable
>import Data.Data
>import Data.Text (Text)
>import qualified Data.Text as Text
>import Math.Tools.Isomorphism
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Math.Tools.PrettyP
>import Math.Tools.CoMonad
>import Math.Tools.Adjunction (swap)
>import Data.Map (Map)
>import qualified Data.Map as Map
>import qualified Data.Monoid as Monoid
>import Data.Monoid (Endo(Endo), appEndo)
>import Math.Number.Group
>import Math.Matrix.Interface
>import Math.Graph.Action
>import Data.Kind

>class (Eq (arr a a), Monoid.Monoid (arr a a)) => GraphMonoid arr a where
>   gdom :: arr a a
>   gcod :: arr a a

>class (GraphMonoid arr a) => ReversibleGraphMonoid arr a where
>   gnot :: arr a a

>instance Eq (Bool -> Bool) where
>  a == b = a True == b True && a False == b False
>

>instance Eq (Endo Bool) where
>  x == y = appEndo x True  == appEndo y True
>        && appEndo x False == appEndo y False

>-- <https://ncatlab.org/nlab/show/delooping>
>-- This is a delooping of a three element monoid.
>-- That is, we think of the three elements as
>-- the hom-set of a endomorphism of a distinguished object
>data Three b c where
>   TId :: Three a a
>   TDom :: Three a a
>   TCod :: Three a a
>
>deriving instance Eq (Three a a)
>deriving instance Show (Three a a)
>deriving instance Ord (Three a a)
>deriving instance Typeable (Three a a)
>deriving instance (Typeable a, Data a) => Data (Three a a)

>instance Cat.Category Three where
>   id = TId
>   (.) = mappend_three

data Three = TId | TDom | TCod deriving (Eq,Show,Ord, Typeable, Data, Generic)

>three_action :: (g -> g) -> (g -> g) -> (g -> g) -> Three g g -> Endo g
>three_action a b c = \case
>  TId -> Endo a
>  TDom -> Endo b
>  TCod -> Endo c

>three_action_arr :: (ArrowChoice arr) => arr g g -> arr g g -> arr g g -> Three g g -> arr g g
>three_action_arr a b c f = proc i -> case f of { TId -> a -< i ; TDom -> b -< i ; TCod -> c -< i }

>instance (Arrow arr) => MonoidArrow arr (Three Bool Bool) Bool where
>   monoidA TId = arr id
>   monoidA TDom = arr (const False)
>   monoidA TCod = arr (const True)

>instance Universe (Three a a) where
>  all_elements = [TId,TDom,TCod]
> 
>instance PpShow (Three a a) where
>   pp TId = "id"
>   pp TDom = "dom"
>   pp TCod = "cod"

>instance Semigroup (Three a a) where
>   (<>) = mappend_three

>instance Monoid (Three a a) where
>   mempty = TId
>   mappend = mappend_three

>mappend_three :: Three b c -> Three a b -> Three a c
>mappend_three TId x = x
>mappend_three x TId = x
>mappend_three TDom TDom = TDom
>mappend_three TDom TCod = TDom
>mappend_three TCod TDom = TCod
>mappend_three TCod TCod = TCod

>instance GraphMonoid Three a where
>   gdom = TDom
>   gcod = TCod

>data Four a b where
>  FId :: Four a a
>  FDom :: Four a a
>  FCod :: Four a a
>  FNot :: Four a a

>deriving instance Eq (Four a a)
>deriving instance Show (Four a a)
>deriving instance Ord (Four a a)
>deriving instance Typeable (Four a a)
>deriving instance (Data a, Typeable a) => Data (Four a a)

>data Restricted (g :: * -> Constraint) a b where
>  Restricted :: (g a) => a -> Restricted g a a

>data GEndo b c where
>  GEndo :: Endo a -> GEndo a a
>

data Four = FId | FDom | FCod | FNot deriving (Eq,Show, Ord, Typeable, Data, Generic)

>three_to_four :: Three a b -> Four a b
>three_to_four = \case { TId -> FId ;  TDom -> FDom ; TCod -> FCod }

>four_action_arr :: (ArrowChoice arr)
>  => arr g g -> arr g g -> arr g g -> arr g g -> Four g g -> arr g g
>four_action_arr aid adom acod anot f = proc v -> case f of
>   FId -> aid -< v
>   FDom -> adom -< v
>   FCod -> acod -< v
>   FNot -> anot -< v

>four_action :: (g -> g) -> (g -> g) -> (g -> g) -> (g -> g) -> Four g g -> Endo g
>four_action aid adom acod anot = \case
>  FId -> Endo aid
>  FDom -> Endo adom
>  FCod -> Endo acod
>  FNot -> Endo anot

>instance Universe (Four a a) where 
>  all_elements = [FId,FDom, FCod,FNot]
> 
>instance PpShow (Four a a) where
>   pp FId = "id"
>   pp FDom = "dom"
>   pp FCod = "cod"
>   pp FNot = "not"

>instance MonoidArrow (->) (Four Bool Bool) Bool where
>   monoidA FId  = id
>   monoidA FDom = arr (const False)
>   monoidA FCod = arr (const True)
>   monoidA FNot = arr not


>instance Semigroup (Four a a) where
>   (<>) = mappend_four

>instance Monoid (Four a a) where
>   mempty = FId
>   mappend = mappend_four
>
>instance Cat.Category Four where
>   id = FId
>   (.) = mappend_four

>mappend_four :: Four b c -> Four a b -> Four a c
>mappend_four FId x = x
>mappend_four x FId = x
>mappend_four FDom FDom = FDom
>mappend_four FDom FCod = FDom
>mappend_four FDom FNot = FDom
>mappend_four FCod FDom = FCod
>mappend_four FCod FCod = FCod
>mappend_four FCod FNot = FCod
>mappend_four FNot FDom = FCod
>mappend_four FNot FCod = FDom
>mappend_four FNot FNot = FId

>instance Group (Four a a) where
>   ginvert FId = FId
>   ginvert FDom = FCod
>   ginvert FCod = FDom
>   ginvert FNot = FNot

>instance Group (Three a a) where
>   ginvert TId = TId
>   ginvert TDom = TCod
>   ginvert TCod = TDom

>instance GraphMonoid Four a where
>   gdom = FDom
>   gcod = FCod

>instance ReversibleGraphMonoid Four a where
>   gnot = FNot

>vertexEndo :: m -> Endo a
>vertexEndo m = Endo id

>optEndo :: (Eq a) => [(a,a)] -> Bool -> Endo a
>optEndo lst m = Endo $ \a -> if m then maybe a id (lookup a lst) else a

>edge :: a -> (a,a) -> Three a a -> a
>edge a (x,y) m | m == gdom = x
>               | m == gcod = y
>               | otherwise = a

>bidirectionalEdge :: (a,a) -> (a,a) -> Four a a -> a
>bidirectionalEdge (a,ra) (x,y) m
> | m == gdom   = x
> | m == gcod   = y
> | m == mempty = a
> | m == gnot   = ra

>edgesFromMapArr :: (Ord a, ArrowChoice arr, FailureArrow arr String) => Map a (a,a) -> Three a a -> arr a a
>edgesFromMapArr edgemap m = proc a -> do
>   v <- arr (\ (a',e) -> Map.lookup a' e) -< (a,edgemap)
>   case v of
>     (Just (x,y)) -> returnA -< edge a (x,y) m
>     Nothing -> do
>        edges <- arr Map.elems -< edgemap
>        vertices <- arr (\e -> map fst e ++ map snd e) -< edges
>        if a `elem` vertices then returnA -< a
>                   else failA -< "edgesFromMapArr: Cannot find vertex"

>edgesFromMapEndo :: (Ord a) => Map a (a,a) -> Three a a -> Endo a
>edgesFromMapEndo edgemap m = Endo $ \a -> case Map.lookup a edgemap of
>   (Just (x,y)) -> edge a (x,y) m
>   Nothing      -> let edges = Map.elems edgemap
>                       vertices = map fst edges ++ map snd edges
>                    in if a `elem` vertices then a else error "edgesFromMapEndo: Cannot find vertex"

>-- | from list of (v_i,e_i), the loopEndo contains edges e_i : v_i -> v_i
>loopEndo :: (Eq a) => [(a,a)] -> Three a a -> Endo a
>loopEndo lst = edgesEndo $ map (\(v,e) -> (v,(e,e))) lst

>edgesArr :: (Eq a, ArrowChoice arr, FailureArrow arr String) => [(a,(a,a))] -> Three a a -> arr a a
>edgesArr lst m = proc a -> do
>   case lookup a lst of
>     (Just pair) -> returnA -< edge a pair m
>     Nothing -> do
>        let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>          in if a `elem` vertices then returnA -< a else failA -< "Cannot find vertex"



>edgesEndo :: (Eq a) => [(a,(a,a))] -> Three a a -> Endo a
>edgesEndo lst m = Endo $ \a -> case lookup a lst of
>   (Just pair) -> edge a pair m
>   Nothing  -> let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>                in if a `elem` vertices then a else error "Cannot find vertex"

>reversibleEdgesArr :: (Eq a, ArrowChoice arr) => [((a,a),(a,a))] -> Four a a -> arr a a
>reversibleEdgesArr lst = four_action_arr Cat.id domLookup codLookup negLookup
>  where fwdSearch = map (\ ~(~(a,b),x) -> (a,(b,x))) lst
>                  ++ map (\ ~(~(a,b),x) -> (b,(a,swap x))) lst
>        domLookup = proc a -> returnA -< maybe a (fst . snd) (lookup a fwdSearch)
>        codLookup = proc a -> returnA -< maybe a (snd . snd) (lookup a fwdSearch)
>        negLookup = proc a -> returnA -< maybe a fst (lookup a fwdSearch)

>reversibleEdgesEndo :: (Eq a) => [((a,a),(a,a))] -> Four a a -> Endo a
>reversibleEdgesEndo lst = four_action id domLookup codLookup negLookup
>   where fwdSearch = map (\ ~(~(a,b),x) -> (a,(b,x))) lst
>                  ++ map (\ ~(~(a,b),x) -> (b,(a,swap x))) lst
>         domLookup a = maybe a (fst . snd) (lookup a fwdSearch)
>         codLookup a = maybe a (snd . snd) (lookup a fwdSearch)
>         negLookup a = maybe a fst         (lookup a fwdSearch)

>edgePairEndo :: (m -> Endo a) -> (n -> Endo b) -> (m,n) -> Endo (a,b)
>edgePairEndo f g (x,y) = Endo $ \(a,b) -> (f x `appEndo` a, g y `appEndo` b)

>-- | This is closest that the endomorphism is to a functor.
>mapEndo :: a :==: b -> Endo a :==: Endo b
>mapEndo f =   (\ (Endo g) -> Endo (runIso f . g . runIsoInverse f))
>          <-> (\ (Endo h) -> Endo (runIsoInverse f . h . runIso f))

>instance FunctorArrow Endo (:==:) where
>   amap = mapEndo
