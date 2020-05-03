>{-# LANGUAGE Safe, TypeFamilies,ExistentialQuantification, MultiParamTypeClasses,
>             FlexibleInstances, GADTs, LambdaCase
>  #-}
>module Math.Tools.Visitor where
>import safe Math.Tools.I
>import Data.Array (Array,array,assocs)
>import Data.Ix
>import Data.Complex
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Control.Monad.Trans.State.Lazy

>-- | loosely based on "Gamma et al: Design Patterns" visitor pattern, and also on
>-- Haskell 'foldr' function and common variations of it.
>class Visitor v where
>   data Fold v :: * -> *
>   visit :: Fold v a -> v -> a

>class Builder v where
>   data Unfold v :: * -> *
>   build :: Unfold v a -> a -> v


>-- | <http://blog.jle.im/entry/fixed-length-vector-types-in-haskell-2015.html>
>-- | we are allowing return of the state from the operation.
>-- | and are using state transformer monad for easier writing
>-- | of instance declarations
>class Unfoldable v where 
>   unfoldF :: (Monad m) => StateT s m a -> StateT s m (v a)

>unfoldVectorM :: (Monad m, Unfoldable v) => (s -> (a,s)) -> s -> m (v a, s)
>unfoldVectorM f s = unfoldF (state f) `runStateT` s

>unfoldVector :: (Unfoldable v) => (s -> (a,s)) -> s -> (v a, s)
>unfoldVector f s = unfoldF (state f) `runState` s

>class FunctorBuilder f where
>  mapBuilder :: (Builder v) => Unfold v a -> Unfold (f v) (f a)

>class (Builder v) => ConstantBuilder v where
>   newBuilder :: v -> Unfold v a



>instance FunctorBuilder ((->) b) where
>   mapBuilder b = FunctionUnfold (\ (f,str) -> build b (f str))

>data VArrow a b where
>  VArrowCompose :: VArrow b c -> VArrow a b -> VArrow a c
>  VArrowId :: (Visitor a, Builder a) => Fold a b -> Unfold a b -> VArrow a a

>varrow :: VArrow a b -> a -> b
>varrow (VArrowId f u) = build u . visit f
>varrow (VArrowCompose x y) = varrow x . varrow y

>transform_visitor :: (Visitor v, Builder v) => Fold v b -> Unfold v a ->  a -> b
>transform_visitor f g = visit f . build g

>class (Builder v) => Unbuilder v b where
>   -- inverse to 'build'.
>   unbuild :: (Unfold v b -> b -> b) -> v -> Unfold v b

>class (Visitor v) => IdVisitor v where
>   idFold :: Fold v v

>class (Visitor v) => ComposableVisitor v where
>   embed :: (v -> b) -> Fold v b
>   (+.+) :: (Visitor b) => Fold b c -> Fold v b -> Fold v c
>   x +.+ y = embed (visit x . visit y)

>class (Builder v) => IdBuilder v where
>   idUnfold :: Unfold v v

>fix2 :: (a -> b -> b) -> a -> b
>fix2 lf a = let x = lf a x in x

>fix3 :: (a -> b -> c -> c) -> a -> b -> c
>fix3 lf a b = let x = lf a b x in x

>bind2 :: (a -> b -> b) -> a -> a -> b
>bind2 lf a b = let x = lf a (lf b x) in x

>bind3 :: (a -> b -> b) -> a -> a -> a -> b
>bind3 lf a b c = let x = lf a (lf b (lf c x)) in x

>bindn :: (a -> b -> b) -> [a] -> b
>bindn lf lst = let x = foldr lf x lst in x

>instance Visitor (I a) where
>   data Fold (I a) b = IFold (a -> b)
>   visit (IFold f) (I x) = f x

>instance ComposableVisitor (I a) where { embed f = IFold (f . I) }

>instance Builder (I a) where
>   data Unfold (I a) b = IUnfold (b -> a)
>   build (IUnfold f) x = I (f x)

>instance Builder (IO a) where
>   data Unfold (IO a) b = IOUnfold (b -> IO a)
>   build (IOUnfold f) x = f x


>instance Visitor (Map i v) where
>   data Fold (Map i v) a = MapFold {
>      mapfold_function :: i -> v -> a -> a,
>      mapfold_element  ::  a }
>   visit (MapFold f x) m = Map.foldrWithKey f x m

>instance (Ord i) => Builder (Map i v) where
>   data Unfold (Map i v) a = MapUnfold { mapunfold_apply :: a -> Maybe (i,v,a) }
>   build z@(MapUnfold m) x = maybe Map.empty inserter (m x)
>      where inserter (i,v,r) = Map.insert i v (build z r)

>mappingBuilder :: (Ord i) => Unfold (i -> v) (Map i v)
>mappingBuilder = FunctionUnfold $ \ (m,index) -> m Map.! index

>predicate :: (Ord i) => i -> Unfold Bool (Set i)
>predicate i = BoolUnfold (\s -> i `Set.member` s)

>var :: (Ord i) => ((Map i v -> v) -> t) -> i -> t
>var f i = f (\m -> m Map.! i)

>boolvar :: (Ord i) => i -> Unfold Bool (Map i Bool)
>boolvar = var BoolUnfold

>intvar :: (Ord i) => i -> Unfold Int (Map i Int)
>intvar = var IntUnfold

>doublevar :: (Ord i) => i -> Unfold Double (Map i Double)
>doublevar = var DoubleUnfold 

>integervar :: (Ord i) => i -> Unfold Integer (Map i Integer)
>integervar = var IntegerUnfold

>instance Visitor (Set v) where
>   data Fold (Set v) a = SetFold {
>     setfold_function :: v -> a -> a,
>     setfold_default :: a }
>   visit (SetFold f x) s = Set.fold f x s

>instance ComposableVisitor (Set v) where
>   embed f = SetFold (\v x -> f (Set.singleton v)) (f (Set.empty))

>instance (Ord v) => Builder (Set v) where
>   data Unfold (Set v) a = SetUnfold { setunfold_apply :: a -> Maybe (v,a) }
>   build z@(SetUnfold m) x = maybe Set.empty inserter (m x)
>     where inserter (i,r) = Set.insert i (build z r)

>instance (Visitor v) => Visitor [v] where
>   data Fold [v] a = forall b. ListFold a (v -> b -> a) (Fold [v] b)
>   visit (ListFold n _ _) [] = n
>   visit (ListFold _ cf z) (c:cr) = cf c (visit z cr)

>instance (Visitor v) => ComposableVisitor [v] where
>   embed f = ListFold (f []) (\e b -> f (e:b)) (embed id)

>listfold :: a -> (v -> a -> a) -> Fold [v] a
>listfold a f = let x = ListFold a f x in x

>ziplistfold :: Fold [v] ([t] -> [(v,t)])
>ziplistfold = listfold e f
>  where e _ = []
>        f x g = \case { [] -> [] ; (y:ys) -> (x,y) : g ys }

>instance Builder [v] where
>   data Unfold [v] b = ListUnfold { listunfold_apply :: b -> Maybe (v,b) }
>   build z@(ListUnfold f) x = maybe [] (\ (a,b) -> a : build z b) (f x)

>instance (Ix a, Visitor a,Visitor b) => Visitor (Array a b) where
>   data Fold (Array a b) c = ArrayFold (Fold [(a,b)] c)
>   visit (ArrayFold f) ar = visit f (assocs ar)

>instance (Builder a,Builder b, Ix a) => Builder (Array a b) where
>   data Unfold (Array a b) c = ArrayUnfold (a,a) (c -> [(a,c)]) (Unfold b c)
>   build z@(ArrayUnfold (a,b) lst g) x = array (a,b) $
>                                            fmap (\ (x',y) -> (x',build g y))
>                                                 (lst x)

>instance Visitor () where
>   data Fold () b = TerminalFold b
>   visit (TerminalFold x) () = x

>instance Builder () where
>   data Unfold () b = Terminal
>   build Terminal _ = ()

>instance (Visitor a, Visitor b) => Visitor (a,b) where
>   data Fold (a,b) c = PairFold (a -> b -> c)
>   visit (PairFold f) (x,y) = f x y

>instance (Visitor a, Visitor b) => ComposableVisitor (a,b) where
>   embed f = PairFold (\a b -> f (a,b))

>instance (Builder a, Builder b) => Builder (a,b) where
>   data Unfold (a,b) c = PairUnfold (Unfold a c,Unfold b c)
>   build (PairUnfold (f', g')) x = (build f' x,build g' x)

>instance (ConstantBuilder a, ConstantBuilder b) => ConstantBuilder (a,b) where
>   newBuilder (x,y) = PairUnfold (newBuilder x,newBuilder y)

>instance (Visitor a, Visitor b, Visitor c) => Visitor (a,b,c) where
>   data Fold (a,b,c) d = TripleFold (a -> b -> c -> d)
>   visit (TripleFold f) (x,y,z) = f x y z

>instance (Visitor a, Visitor b, Visitor c) => ComposableVisitor (a,b,c) where
>   embed f = TripleFold $ \a b c -> f (a,b,c)

>instance (Builder a, Builder b , Builder c) => Builder (a,b,c) where
>   data Unfold (a,b,c) d = TripleUnfold (Unfold a d, Unfold b d, Unfold c d)
>   build (TripleUnfold (f', g', h')) x = (build f' x, build g' x, build h' x)

instance (IdBuilder a, IdBuilder b, IdBuilder c) => IdBuilder (a,b,c) where
   idUnfold = TripleUnfold (idUnfold, idUnfold, idUnfold)

>instance (Visitor a,Visitor b,Visitor c, Visitor d) => Visitor (a,b,c,d) where
>   data Fold (a,b,c,d) e = QuadFold (a -> b -> c -> d -> e)
>   visit (QuadFold f) (x,y,z,a) = f x y z a

>instance (Visitor a, Visitor b, Visitor c, Visitor d) => ComposableVisitor (a,b,c,d) where
>   embed f = QuadFold $ \a b c d -> f (a,b,c,d)

>instance (Builder a, Builder b, Builder c, Builder d) => Builder (a,b,c,d) where
>   data Unfold (a,b,c,d) e = QuadUnfold (Unfold a e, Unfold b e, Unfold c e,Unfold d e)
>   build (QuadUnfold (f', g', h', i')) x = (build f' x, build g' x, build h' x, build i' x)

>instance Visitor (b -> v) where
>   data Fold (b -> v) a = forall c. FunctionFold [b] (b -> v -> c) ([c] -> a)
>   visit (FunctionFold lst f alts) g = alts [f x (g x) | x <- lst]

>instance Builder (b -> v) where
>   data Unfold (b -> v) a = FunctionUnfold ((a,b) -> v)
>   build (FunctionUnfold g) x = \y -> g (x,y)

>instance (Visitor v, Visitor w) => Visitor (Either v w) where
>   data Fold (Either v w) a = EitherFold (Fold v a) (Fold w a)
>   visit (EitherFold vf _) (Left  x) = visit vf x
>   visit (EitherFold _ wf) (Right y) = visit wf y

>instance (ComposableVisitor v, ComposableVisitor w) 
>   => ComposableVisitor (Either v w) where
>    embed f = EitherFold (embed (f . Left)) (embed (f . Right))

>instance (Builder v , Builder w ) => Builder (Either v w) where
>   data Unfold (Either v w) a = EitherUnfold (a -> Either a a, Unfold v a, Unfold w a)
>   build (EitherUnfold (f,x,y)) z = case f z of
>                       (Left ctx) -> Left (build x ctx)
>                       (Right ctx) -> Right (build y ctx)

>instance (Visitor v) => Visitor (Maybe v) where
>   data Fold (Maybe v) a = MaybeFold a (Fold v a)
>   visit (MaybeFold n _) Nothing = n
>   visit (MaybeFold _ f) (Just x) = visit f x

>instance (ComposableVisitor v) => ComposableVisitor (Maybe v) where
>   embed f = MaybeFold (f Nothing) (embed (f . Just))

>instance (Builder v) => Builder (Maybe v) where
>   data Unfold (Maybe v) a = MaybeUnfold (a -> Maybe a) (Unfold v a)
>   build (MaybeUnfold f b) x | Just ctx <- f x = Just (build b ctx)
>                             | otherwise = Nothing  

>instance Visitor Bool where
>   data Fold Bool a = BoolFold a a
>   visit (BoolFold t _) True  = t
>   visit (BoolFold _ f) False = f

>instance ComposableVisitor Bool where 
>  embed f = BoolFold (f True) (f False)

>instance Builder Bool where
>   data Unfold Bool a = BoolUnfold (a -> Bool)
>   build (BoolUnfold f) x = f x

>instance Visitor Char where
>   data Fold Char a = CharFold {charfold_apply :: Char -> a }
>   visit (CharFold f) c = f c

>instance ComposableVisitor Char where { embed = CharFold }

>instance Builder Char where
>   data Unfold Char a = CharUnfold { charunfold_apply :: a -> Char }
>   build (CharUnfold f) c = f c

>instance Visitor Int where
>   data Fold Int a = IntFold (Int -> a)
>   visit (IntFold f) i = f i

>instance ComposableVisitor Int where { embed = IntFold }

>instance (Builder a, RealFloat a) => Builder (Complex a) where
>   data Unfold (Complex a) d = ComplexUnfold {
>      complexunfold_real :: Unfold a d,
>      complexunfold_imag :: Unfold a d }
>   build (ComplexUnfold x y) e = build x e :+ build y e

>instance IdBuilder (Complex Double) where
>   idUnfold = ComplexUnfold (DoubleUnfold realPart) (DoubleUnfold imagPart)

>instance (ConstantBuilder a, RealFloat a) => ConstantBuilder (Complex a) where
>   newBuilder (re :+ im) = ComplexUnfold (newBuilder re) (newBuilder im)

>instance Builder Int where
>   data Unfold Int a = IntUnfold { intunfold_apply :: a -> Int }
>   build (IntUnfold f) i = f i

>instance IdBuilder Int where { idUnfold = IntUnfold id }

>instance Builder Double where
>   data Unfold Double a = DoubleUnfold { doubleunfold_apply :: a -> Double }
>   build (DoubleUnfold f) i = f i

>instance IdBuilder Double where { idUnfold = DoubleUnfold id }

>instance ConstantBuilder Double where
>   newBuilder v = DoubleUnfold (const v)

>instance Visitor Integer where
>   data Fold Integer a = IntegerFold { integerfold_apply :: Integer -> a }
>   visit (IntegerFold f) i = f i

>instance Builder Integer where
>   data Unfold Integer a = IntegerUnfold { integerunfold_apply :: a -> Integer }
>   build (IntegerUnfold f) i = f i

Note sharing in natural numbers. 
let x = In (Just (In (Just (In Nothing))))
let y = In (Just x)

