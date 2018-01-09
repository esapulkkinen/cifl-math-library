>{-# LANGUAGE Safe,FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
>module Math.Graph.GraphMonoid where
>import Control.Arrow
>import Math.Tools.Isomorphism
>import Math.Tools.Arrow
>import Math.Tools.Universe
>import Math.Tools.PrettyP
>import Math.Tools.CoMonad
>import Math.Tools.Adjunction (swap)
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Data.Monoid
>import Math.Number.Group
>import Math.Graph.Action

>class (Eq x, Monoid x) => GraphMonoid x where
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

>data Three = TId | TDom | TCod deriving (Eq,Show,Ord)

>three_action :: (g -> g) -> (g -> g) -> (g -> g) -> Three -> Endo g
>three_action a _ _ TId  = Endo a
>three_action _ b _ TDom = Endo b
>three_action _ _ c TCod = Endo c

>instance Universe Three where { all_elements = [TId,TDom,TCod] }
>instance PpShow Three where
>   pp TId = pp "id"
>   pp TDom = pp "dom"
>   pp TCod = pp "cod"

>instance Monoid Three where
>   mempty = TId
>   mappend TId x = x
>   mappend x TId = x
>   mappend TDom x = TDom
>   mappend TCod x = TCod

>instance GraphMonoid Three where
>   gdom = TDom
>   gcod = TCod

>data Four = FId | FDom | FCod | FNot deriving (Eq,Show, Ord)

>three_to_four :: Three -> Four
>three_to_four TId = FId
>three_to_four TDom = FDom
>three_to_four TCod = FCod

>four_action :: (g -> g) -> (g -> g) -> (g -> g) -> (g -> g) -> Four -> Endo g
>four_action aid _ _ _ FId   = Endo aid
>four_action _ adom _ _ FDom = Endo adom
>four_action _ _ acod _ FCod = Endo acod
>four_action _ _ _ anot FNot = Endo anot

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

>instance Monoid Four where
>   mempty = FId
>   mappend FId x = x
>   mappend x FId = x
>   mappend FDom x = FDom
>   mappend FCod x = FCod
>   mappend FNot FDom = FCod
>   mappend FNot FCod = FDom
>   mappend FNot FNot = FId

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

instance Comonad (MonAct m) where
   extract (MonAct x _) = x
   duplicate (MonAct x f) = MonAct (MonAct x f) $ \m ->
         Endo $ \ (MonAct a af) -> MonAct (af m `appEndo` a) $ \m' ->
         Endo $ \ a' -> af m' `appEndo` a'


>-- | This is closest that the endomorphism is to a functor.
>mapEndo :: a :==: b -> Endo a :==: Endo b
>mapEndo f =   (\ (Endo g) -> Endo (runIso f . g . runIsoInverse f))
>          <-> (\ (Endo h) -> Endo (runIsoInverse f . h . runIso f))

>instance (Num a) => Num (Endo a) where
>   (Endo f) + (Endo g) = Endo $ \x -> f x + g x
>   (Endo f) - (Endo g) = Endo $ \x -> f x - g x
>   (Endo f) * (Endo g) = Endo $ \x -> f x * g x
>   negate (Endo f) = Endo $ \x -> negate (f x)
>   abs (Endo f) = Endo $ \x -> abs (f x)
>   signum (Endo f) = Endo $ \x -> signum (f x)
>   fromInteger i = Endo $ const (fromInteger i)

>instance (Fractional a) => Fractional (Endo a) where
>   (Endo f) / (Endo g) = Endo $ \x -> f x / g x
>   recip (Endo f) = Endo $ \x -> recip (f x)
>   fromRational r = Endo $ const (fromRational r)

>instance FunctorArrow Endo (:==:) where
>   amap = mapEndo
