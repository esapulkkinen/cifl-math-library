>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
>{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GADTs #-}
>{-# LANGUAGE UndecidableInstances, KindSignatures, ConstraintKinds #-}
>module Math.Graph.GraphMonoid where
>import Control.Arrow
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

data Three = TId | TDom | TCod deriving (Eq,Show,Ord, Typeable, Data, Generic)

>three_action :: (g -> g) -> (g -> g) -> (g -> g) -> Three g g -> Endo g
>three_action a b c = \case
>  TId -> Endo a
>  TDom -> Endo b
>  TCod -> Endo c

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
>   
>mappend_three :: Three a a -> Three a a -> Three a a
>mappend_three TId x = x
>mappend_three x TId = x
>mappend_three TDom x = TDom
>mappend_three TCod x = TCod

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
>mappend_four :: Four a a -> Four a a -> Four a a
>mappend_four FId x = x
>mappend_four x FId = x
>mappend_four FDom x = FDom
>mappend_four FCod x = FCod
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

>edgesFromMapEndo :: (Ord a) => Map a (a,a) -> Three a a -> Endo a
>edgesFromMapEndo edgemap m = Endo $ \a -> case Map.lookup a edgemap of
>   (Just (x,y)) -> edge a (x,y) m
>   Nothing      -> let edges = Map.elems edgemap
>                       vertices = map fst edges ++ map snd edges
>                    in if a `elem` vertices then a else error "edgesFromMapEndo: Cannot find vertex"

>-- | from list of (v_i,e_i), the loopEndo contains edges e_i : v_i -> v_i
>loopEndo :: (Eq a) => [(a,a)] -> Three a a -> Endo a
>loopEndo lst = edgesEndo $ map (\(v,e) -> (v,(e,e))) lst

>edgesEndo :: (Eq a) => [(a,(a,a))] -> Three a a -> Endo a
>edgesEndo lst m = Endo $ \a -> case lookup a lst of
>   (Just pair) -> edge a pair m
>   Nothing  -> let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>                in if a `elem` vertices then a else error "Cannot find vertex"

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
