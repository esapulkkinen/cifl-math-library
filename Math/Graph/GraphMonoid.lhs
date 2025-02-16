>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
>{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GADTs #-}
>{-# LANGUAGE UndecidableInstances, KindSignatures, ConstraintKinds #-}
>{-# LANGUAGE Arrows, FlexibleContexts, PolyKinds #-}
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
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.Map (Map)
>import qualified Data.Map as Map
>import qualified Data.Monoid as Monoid
>import Data.Monoid (Endo(Endo), appEndo)
>import Math.Number.Group
>import Math.Matrix.Interface
>import Math.Graph.Action
>import Data.Kind
>import Data.Typeable
>import Data.Data

>data GraphElem v e = Vertex v | Edge e
>  deriving (Eq, Ord, Show, Read)

>data ReversibleGraphProp v e = VertexProp v
>                   | EdgeProp e
>                   | LoopProp e
>                   | OneLaneLoopProp e
>                   | BandProp e

>deriving instance (Typeable v, Typeable e) => Typeable (GraphElem v e)
>deriving instance (Data v, Data e) => Data (GraphElem v e)

>instance (PpShow v, PpShow e) => PpShow (GraphElem v e) where
>   pp (Vertex v) = "vertex(" <> pp v <> ")"
>   pp (Edge e)   = "edge(" <> pp e <> ")"

>mapGraphElem :: (v -> v') -> (e -> e') -> GraphElem v e -> GraphElem v' e'
>mapGraphElem f g (Vertex v) = Vertex (f v)
>mapGraphElem f g (Edge e) = Edge (g e)
>
>eitherGraphElem :: (v -> w) -> (e -> w) -> GraphElem v e -> w
>eitherGraphElem f g (Vertex v) = f v
>eitherGraphElem f g (Edge e) = g e

instance (Universe v, Universe e) => Universe (GraphElem v e) where
   all_elements = (map Vertex all_elements) ++ (map Edge all_elements)

>class (Cat.Category arr, Eq (arr a a), Monoid.Monoid (arr a a))
> => GraphMonoid arr a where
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
>deriving instance (Typeable a) => Typeable (Three a a)
>deriving instance (Data a) => Data (Three a a)

>instance Cat.Category Three where
>   id = TId
>   (.) = mappendThree

data Three = TId | TDom | TCod deriving (Eq,Show,Ord, Typeable, Data, Generic)

>-- | intent is that g =~= F Bool, where F : 2^{op} -> Set. But opposite categories are difficult to represent.
>threeAction :: (g -> g) -> (g -> g) -> (g -> g)
>   -> Three Bool Bool -> Endo g
>threeAction a b c = \case
>  TId -> Endo a
>  TDom -> Endo b
>  TCod -> Endo c

>-- | intent is that g =~= F Bool, where F : 2^{op} -> Set. But opposite categories are difficult to represent.
>threeActionArr :: (ArrowChoice arr) => arr g g -> arr g g -> arr g g -> Three Bool Bool -> arr g g
>threeActionArr a b c TId = proc i -> a -< i
>threeActionArr a b c TDom = proc i -> b -< i
>threeActionArr a b c TCod = proc i -> c -< i


>instance (Arrow arr) => MonoidArrow arr (Three Bool Bool) Bool where
>   monoidA TId = arr id
>   monoidA TDom = arr (const False)
>   monoidA TCod = arr (const True)

>instance Universe (Three a a) where
>  allElements = [TId,TDom,TCod]
> 
>instance PpShow (Three a a) where
>   pp TId = "id"
>   pp TDom = "dom"
>   pp TCod = "cod"

>instance Semigroup (Three a a) where
>   (<>) = mappendThree

>instance Monoid (Three a a) where
>   mempty = TId
>   mappend = mappendThree

>mappendThree :: Three b c -> Three a b -> Three a c
>mappendThree TId x = x
>mappendThree x TId = x
>mappendThree TDom TDom = TDom
>mappendThree TDom TCod = TDom
>mappendThree TCod TDom = TCod
>mappendThree TCod TCod = TCod

>instance GraphMonoid Three Bool where
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

>threeToFour :: Three a b -> Four a b
>threeToFour = \case { TId -> FId ;  TDom -> FDom ; TCod -> FCod }

>-- | intent is that g =~= F2, where F : 2^{op} -> Set. But opposite categories are difficult to represent.
>fourActionArr :: (ArrowChoice arr)
>  => arr g g -> arr g g -> arr g g -> arr g g -> Four Bool Bool -> arr g g
>fourActionArr aid adom acod anot FId = proc v -> aid -< v
>fourActionArr aid adom acod anot FDom = proc v -> adom -< v
>fourActionArr aid adom acod anot FCod = proc v -> acod -< v
>fourActionArr aid adom acod anot FNot = proc v -> anot -< v

>-- | intent is that g =~= F2, where F : 2^{op} -> Set. But opposite categories are difficult to represent.
>fourAction :: (g -> g) -> (g -> g) -> (g -> g) -> (g -> g) -> Four Bool Bool -> Endo g
>fourAction aid adom acod anot = \case
>  FId -> Endo aid
>  FDom -> Endo adom
>  FCod -> Endo acod
>  FNot -> Endo anot

>sortPair :: (Ord a) => (a,a) -> (a,a)
>sortPair (x,y) | x <= y = (x,y)
>               | otherwise = (y,x)

>instance Universe (Four a a) where 
>  allElements = [FId,FDom, FCod,FNot]
> 
>instance PpShow (Four a a) where
>   pp FId = "id"
>   pp FDom = "dom"
>   pp FCod = "cod"
>   pp FNot = "not"

>instance (Arrow arr) => MonoidArrow arr (Four Bool Bool) Bool where
>   monoidA FId  = Cat.id
>   monoidA FDom = arr (const False)
>   monoidA FCod = arr (const True)
>   monoidA FNot = arr not

>instance Semigroup (Four Bool Bool) where
>   (<>) = mappendFour

>instance Monoid (Four Bool Bool) where
>   mempty = FId
>   mappend = mappendFour
>
>instance Cat.Category Four where
>   id = FId
>   (.) = mappendFour

>mappendFour :: Four b c -> Four a b -> Four a c
>mappendFour FId x = x
>mappendFour x FId = x
>mappendFour FDom FDom = FDom
>mappendFour FDom FCod = FDom
>mappendFour FDom FNot = FDom
>mappendFour FCod FDom = FCod
>mappendFour FCod FCod = FCod
>mappendFour FCod FNot = FCod
>mappendFour FNot FDom = FCod
>mappendFour FNot FCod = FDom
>mappendFour FNot FNot = FId

>instance Group (Four Bool Bool) where
>   ginvert FId = FId
>   ginvert FDom = FCod
>   ginvert FCod = FDom
>   ginvert FNot = FNot

>instance Group (Three Bool Bool) where
>   ginvert TId = TId
>   ginvert TDom = TCod
>   ginvert TCod = TDom

>instance GraphMonoid Four Bool where
>   gdom = FDom
>   gcod = FCod

>instance ReversibleGraphMonoid Four Bool where
>   gnot = FNot

>vertexEndo :: m -> Endo a
>vertexEndo m = Endo id

>optEndo :: (Eq a) => [(a,a)] -> Bool -> Endo a
>optEndo lst m = Endo $ \a -> if m then maybe a id (lookup a lst) else a

>edge :: a -> (a,a) -> Three Bool Bool -> a
>edge a (x,y) = \m -> case m of
>    TDom -> x
>    TCod -> y
>    TId -> a

>edgeE :: e -> (v,v) -> Three Bool Bool -> GraphElem v e
>edgeE a (x,y) = \m -> case m of
>    TDom -> Vertex x
>    TCod -> Vertex y
>    _ -> Edge a

>bidirectionalEdge :: (f a,f a) -> (f a,f a) -> Four Bool Bool -> f a
>bidirectionalEdge (a,ra) (x,y) = \m -> case m of
>  FDom -> x
>  FCod -> y
>  FId -> a
>  FNot -> ra
>
>bidirectionalEdgeE :: (e,e) -> (v,v) -> Four Bool Bool -> GraphElem v e
>bidirectionalEdgeE (a,ra) (x,y) = \m -> case m of
> FDom -> Vertex x
> FCod -> Vertex y
> FId -> Edge a
> FNot -> Edge ra

>edgesFromMapArr :: (Ord a, ArrowChoice arr, FailureArrow arr String) => Map a (a,a) -> Three Bool Bool -> arr a a
>edgesFromMapArr edgemap = \ m -> proc a -> do
>   v <- arr (\ (a',e) -> Map.lookup a' e) -< (a,edgemap)
>   case v of
>     (Just (x,y)) -> returnA -< edge a (x,y) m
>     Nothing -> do
>        edges <- arr Map.elems -< edgemap
>        vertices <- arr (\e -> map fst e ++ map snd e) -< edges
>        if a `elem` vertices then returnA -< a
>                   else failA -< "edgesFromMapArr: Cannot find vertex"

>edgesFromMapEndo :: (Ord a) => Map a (a,a) -> Three Bool Bool -> Endo a
>edgesFromMapEndo edgemap = \m -> Endo $ \a -> case Map.lookup a edgemap of
>   (Just (x,y)) -> edge a (x,y) m
>   Nothing      -> let edges = Map.elems edgemap
>                       vertices = map fst edges ++ map snd edges
>                    in if a `elem` vertices then a else error "edgesFromMapEndo: Cannot find vertex"

>edgesFromMapEndoE :: (Ord e, Ord v) => Map e (v,v) -> Three Bool Bool -> Endo (GraphElem v e)
>edgesFromMapEndoE edgemap = \m -> Endo $ \case
> (Edge e) -> case Map.lookup e edgemap of
>     (Just (x,y)) -> edgeE e (x,y) m
>     Nothing -> error "edgesFromMapEndoE: cannot find edge"
> (Vertex v) -> let edges = Map.elems edgemap
>                   vertices = map fst edges ++ map snd edges
>               in if v `elem` vertices then Vertex v
>                    else error "edgesFromMapEndoE: cannot find vertex"

>-- | from list of (v_i,e_i), the loopEndo contains edges e_i : v_i -> v_i
>loopEndo :: (Ord a) => [(a,a)] -> Three Bool Bool -> Endo a
>loopEndo lst = edgesEndo $ map (\(v,e) -> (e,(v,v))) lst

>loopEndoE :: (Ord v, Ord e) => [(v,e)] -> Three Bool Bool -> Endo (GraphElem v e)
>loopEndoE lst = edgesEndoE $ map (\(v,e) -> (e,(v,v))) lst

>reversibleOneLaneLoopEndoE :: (Ord v, Ord e) => [(v,e)] -> Four Bool Bool -> Endo (GraphElem v e)
>reversibleOneLaneLoopEndoE lst = reversibleEdgesEndoE $ map (\ (v,e) -> ((e,e),(v,v))) lst

>reversibleLoopEndoE :: (Ord v, Ord e) => [(v,e,e)] -> Four Bool Bool -> Endo (GraphElem v e)
>reversibleLoopEndoE lst = reversibleEdgesEndoE $ map (\ (v,e,e') -> ((e,e'),(v,v))) lst

>edgesArr :: (Eq a, ArrowChoice arr, FailureArrow arr String) => [(a,(a,a))] -> Three Bool Bool -> arr a a
>edgesArr lst = \m -> proc a -> do
>   case lookup a lst of
>     (Just pair) -> returnA -< edge a pair m
>     Nothing -> do
>        let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>          in if a `elem` vertices then returnA -< a else failA -< "Cannot find vertex"

>edgesEndoE :: (Ord v, Ord e)
> => [(e,(v,v))] -> Three Bool Bool -> Endo (GraphElem v e)
>edgesEndoE lst = edgesEndo (lst' lst)
>   where lst' ll = fmap (\ (e,(v1,v2)) -> (Edge e,(Vertex v1, Vertex v2))) ll

>edgesEndo :: (Ord a) => [(a,(a,a))] -> Three Bool Bool -> Endo a
>edgesEndo lst m = Endo find
>   where find a = case lookup a lst of
>            (Just pair) -> edge a pair m
>            Nothing  -> let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>                         in if a `elem` vertices then a else error "Cannot find vertex"

>edgesEndo' :: (Eq a) => [(a,(a,a))] -> Three Bool Bool -> Endo a
>edgesEndo' lst = \m -> Endo $ \a -> case lookup a lst of
>   (Just pair) -> edge a pair m
>   Nothing  -> let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>                in if a `elem` vertices then a else error "Cannot find vertex"

>reversibleEdgesEndoE :: (Ord v, Ord e)
> => [((e,e),(v,v))] -> Four Bool Bool -> Endo (GraphElem v e)
>reversibleEdgesEndoE lst = reversibleEdgesEndo $
>  fmap (\((e1,e2), (v1,v2)) -> ((Edge e1, Edge e2), (Vertex v1, Vertex v2))) lst

>reversibleEdgesEndo :: (Ord a) => [((a,a),(a,a))] -> Four Bool Bool -> Endo a
>reversibleEdgesEndo lst = fourAction id domLookup codLookup negLookup
>   where fwdSearch = map (\ ~(~(a,b),x) -> (a,(b,x))) lst
>                  ++ map (\ ~(~(a,b),x) -> (b,(a,swap x))) lst
>         domLookup a = maybe a (fst . snd) (lookup a fwdSearch)
>         codLookup a = maybe a (snd . snd) (lookup a fwdSearch)
>         negLookup a = maybe a fst         (lookup a fwdSearch)

>edgePairEndo :: (m -> Endo a) -> (n -> Endo b) -> (m,n) -> Endo (a,b)
>edgePairEndo f g = \(x,y) -> Endo $ \(a,b) -> (f x `appEndo` a, g y `appEndo` b)

>-- | This is closest that the endomorphism is to a functor.
>mapEndo :: a :==: b -> Endo a :==: Endo b
>mapEndo f =   (\ (Endo g) -> Endo (runIso f . g . runIsoInverse f))
>          <-> (\ (Endo h) -> Endo (runIsoInverse f . h . runIso f))

>instance FunctorArrow Endo (:==:) (:==:) where
>   amap = mapEndo
