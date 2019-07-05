>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators, DeriveGeneric, DeriveDataTypeable, LambdaCase #-}
>module Math.Graph.GraphMonoid where
>import Control.Arrow
>import GHC.Generics
>import Data.Typeable
>import Data.Data
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

>class (Eq x, Monoid.Monoid x) => GraphMonoid x where
>   gdom :: x
>   gcod :: x

>class (GraphMonoid x) => ReversibleGraphMonoid x where
>   gnot :: x

>instance GraphMonoid (Endo Bool) where
>   gdom = Endo $ const minBound
>   gcod = Endo $ const maxBound

>instance ReversibleGraphMonoid (Endo Bool) where
>   gnot = Endo $ not

>instance (GraphMonoid a, GraphMonoid b) => GraphMonoid (a,b) where
>   gdom = (gdom,gdom)
>   gcod = (gcod,gcod)

>gdomcod :: (GraphMonoid a, GraphMonoid b) => (a,b)
>gdomcod = (gdom,gcod)

>gcoddom :: (GraphMonoid a, GraphMonoid b) => (a,b)
>gcoddom = (gcod,gdom)

>instance (ReversibleGraphMonoid a, ReversibleGraphMonoid b)
>  => ReversibleGraphMonoid (a,b) where
>   gnot = (gnot,gnot)

>instance Eq (Endo Bool) where
>  x == y = appEndo x True  == appEndo y True
>        && appEndo x False == appEndo y False

>instance Show (Endo Bool) where
>  show x | x == mempty = "id"
>         | x == gdom = "source"
>         | x == gcod = "target"
>         | x == gnot = "invert"

>data Three = TId | TDom | TCod deriving (Eq,Show,Ord, Typeable, Data, Generic)

>three_action :: (g -> g) -> (g -> g) -> (g -> g) -> Three -> Endo g
>three_action a b c = \case
>  TId -> Endo a
>  TDom -> Endo b
>  TCod -> Endo c

>instance Universe Three where { all_elements = [TId,TDom,TCod] }
>instance PpShow Three where
>   pp TId = pp "id"
>   pp TDom = pp "dom"
>   pp TCod = pp "cod"

>instance Semigroup Three where
>   (<>) = mappend_three

>instance Monoid Three where
>   mempty = TId
>   mappend = mappend_three
>   
>mappend_three TId x = x
>mappend_three x TId = x
>mappend_three TDom x = TDom
>mappend_three TCod x = TCod

>instance GraphMonoid Three where
>   gdom = TDom
>   gcod = TCod

>data Four = FId | FDom | FCod | FNot deriving (Eq,Show, Ord, Typeable, Data, Generic)

>three_to_four :: Three -> Four
>three_to_four = \case { TId -> FId ;  TDom -> FDom ; TCod -> FCod }

>four_action :: (g -> g) -> (g -> g) -> (g -> g) -> (g -> g) -> Four -> Endo g
>four_action aid adom acod anot = \case
>  FId -> Endo aid
>  FDom -> Endo adom
>  FCod -> Endo acod
>  FNot -> Endo anot

>instance Universe Four where { all_elements = [FId,FDom, FCod,FNot] }
>instance PpShow Four where
>   pp FId = pp "id"
>   pp FDom = pp "dom"
>   pp FCod = pp "cod"
>   pp FNot = pp "not"

>instance MonoidArrow (->) Four Bool where
>   monoidA FId  = id
>   monoidA FDom = arr (const False)
>   monoidA FCod = arr (const True)
>   monoidA FNot = arr not

>instance Semigroup Four where
>   (<>) = mappend_four

>instance Monoid Four where
>   mempty = FId
>   mappend = mappend_four
>
>mappend_four FId x = x
>mappend_four x FId = x
>mappend_four FDom x = FDom
>mappend_four FCod x = FCod
>mappend_four FNot FDom = FCod
>mappend_four FNot FCod = FDom
>mappend_four FNot FNot = FId

>instance Group Four where
>   ginvert FId = FId
>   ginvert FDom = FCod
>   ginvert FCod = FDom
>   ginvert FNot = FNot

>instance Group Three where
>   ginvert TId = TId
>   ginvert TDom = TCod
>   ginvert TCod = TDom

>instance GraphMonoid Four where
>   gdom = FDom
>   gcod = FCod

>instance ReversibleGraphMonoid Four where
>   gnot = FNot

>data MonAct m a = MonAct { monActElem :: a,
>                           runMonAct :: m -> Endo a }

>vertexEndo :: m -> Endo a
>vertexEndo m = Endo id

>optEndo :: (Eq a) => [(a,a)] -> Bool -> Endo a
>optEndo lst m = Endo $ \a -> if m then maybe a id (lookup a lst) else a

>edge :: a -> (a,a) -> Three -> a
>edge a (x,y) m | m == gdom = x
>               | m == gcod = y
>               | otherwise = a

>bidirectionalEdge :: (a,a) -> (a,a) -> Four -> a
>bidirectionalEdge (a,ra) (x,y) m
> | m == gdom   = x
> | m == gcod   = y
> | m == mempty = a
> | m == gnot   = ra

>edgesFromMapEndo :: (Ord a) => Map a (a,a) -> Three -> Endo a
>edgesFromMapEndo edgemap m = Endo $ \a -> case Map.lookup a edgemap of
>   (Just (x,y)) -> edge a (x,y) m
>   Nothing      -> let edges = Map.elems edgemap
>                       vertices = map fst edges ++ map snd edges
>                    in if a `elem` vertices then a else error "edgesFromMapEndo: Cannot find vertex"

>edgesEndo :: (Eq a) => [(a,(a,a))] -> Three -> Endo a
>edgesEndo lst m = Endo $ \a -> case lookup a lst of
>   (Just pair) -> edge a pair m
>   Nothing  -> let vertices = map (fst . snd) lst ++ map (snd . snd) lst
>                in if a `elem` vertices then a else error "Cannot find vertex"

>reversibleEdgesEndo :: (Eq a) => [((a,a),(a,a))] -> Four -> Endo a
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
