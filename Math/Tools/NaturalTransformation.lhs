>{-# LANGUAGE RankNTypes, PolyKinds, GADTs, MultiParamTypeClasses, TypeOperators, Arrows, Safe, ConstraintKinds #-}
>module Math.Tools.NaturalTransformation where
>
>-- Basic support for natural transformations.
>-- This is very basic concept from category theory.
>-- 
>-- For reference I've used:
>-- <Natural transformation in wikipedia|https://en.wikipedia.org/wiki/Natural_transformation>
>-- <Natural transformation in ncatlab|https://ncatlab.org/nlab/show/natural+transformation>
>-- Roy L. Crole: Categories for Types.
>import Data.Kind
>import Math.Tools.I
>import Math.Tools.CoMonad
>import Control.Monad
>import Control.Applicative
>import Control.Category
>import Control.Arrow
>import Math.Tools.Adjunction
>import Math.Tools.OppositeArrow
>import Math.Tools.Arrow
>import Math.Tools.CoFunctor
>import Math.Tools.Isomorphism
>import Math.Tools.FixedPoint
>import Math.Matrix.Interface
>import Prelude hiding ((.),id)

=== NATURAL TRANSFORMATION ============

>newtype (f :: k -> Type) :~> (g :: k -> Type) = NatTrans {
>   nattransComponent :: forall (a :: k). f a -> g a
> }

>instance Category (:~>) where
>   id = NatTrans id
>   (NatTrans f) . (NatTrans g) = NatTrans (f . g)

>type MaybeNT m = Maybe :~> m
>type EitherNT a m = (Either a) :~> m
>type PairNT a m = ((,) a) :~> m
>type IndexNT a m = ((->) a) :~> m
>type ListNT m = [] :~> m

>type IMT m = I :~> m
>type IOMT m = m :~> IO
>type NondetMT m = m :~> []
>type FailMT m = m :~> Maybe
>type AltMT a m  = m :~> Either a
>type FunctorMT a m = m :~> ((->) a)

>type MonadNT m = (m :*: m) :~> m
>type ComonadNT m = m :~> (m :*: m)
>type TransposeNT m n = (m :*: n) :~> (n :*: m)

>type (row ::*:: col) elem = (((:==:) row) :*: ((:==:) col)) elem

>failFailMT :: FailMT f
>failFailMT = NatTrans (const Nothing)
>succeedFailMT :: FailMT I
>succeedFailMT = NatTrans (\ (I x) -> Just x)
>returnMT :: (Monad m) => I :~> m
>returnMT = NatTrans (return . unI)
>extractMT :: (Comonad m) => m :~> I
>extractMT = NatTrans (I . extract)
>duplicateMT :: (Comonad m) => m :~> (m :*: m)
>duplicateMT = NatTrans (Matrix . duplicate)
>joinMT :: (Monad m) => (m :*: m) :~> m
>joinMT = NatTrans (join . cells)

>idTrans :: f :~> f
>idTrans = NatTrans id

>unitTrans :: (Adjunction f g) => I :~> (g :*: f)
>unitTrans = NatTrans (\ (I x) -> Matrix $ unit x)

>counitTrans :: (Adjunction f g) => (f :*: g) :~> I
>counitTrans = NatTrans (\ (Matrix f) -> I $ counit f)

>vert :: g :~> h -> f :~> g -> f :~> h
>vert (NatTrans f) (NatTrans g) = NatTrans (f . g)

>horiz :: (Functor h) => h :~> k -> f :~> g -> (h :*: f) :~> (k :*: g)
>horiz s t = NatTrans (Matrix .  nattransComponent s . fmap (nattransComponent t) . cells)

>mapNaturalMatrix :: (Functor f, Functor h) 
>           => f :~> g -> h :~> i -> (a -> b) -> (f :*: h) a -> (g :*: i) b
>mapNaturalMatrix f g t = nattransComponent (mapColumns f)
>                         . nattransComponent (mapRows g)
>                         . fmap t

>data Day f g c = forall a b. Day ((a,b) -> c) (f a) (g b)

>convolve :: (Functor f, Functor g) => Day f g c -> (f :*: g) c
>convolve (Day f x y) = matrix (curry f) x y

>convolveTrans :: (Functor f, Functor g) => Day f g :~> (f :*: g)
>convolveTrans = NatTrans convolve

>mapRows :: (Functor h) => f :~> g -> (h :*: f) :~> (h :*: g)
>mapRows (NatTrans f) = NatTrans (Matrix . fmap f . cells)

>mapColumns :: f :~> g -> (f :*: h) :~> (g :*: h)
>mapColumns (NatTrans f) = NatTrans (Matrix . f . cells)

>mapMatrixNat :: (Functor f, Functor g)
> => f :~> f' -> g :~> g' -> (a -> b) -> (f :*: g) a -> (f' :*: g') b
>mapMatrixNat col row elem m = (col `horiz` row) `nattransComponent` (fmap elem m)

>recMap :: (Functor f) =>  f :~> g -> Rec f -> Rec g
>recMap z (In x) = In $ nattransComponent z $ fmap (recMap z) x

>transformMap :: (Functor f, Functor g)
> => Coalgebra f a -> f :~> g -> Algebra g b -> a -> b
>transformMap divide f combine = fold combine . recMap f . unfold divide

>tmap :: (InitialAlgebra f a, FinalCoalgebra g b) => f :~> g -> a -> b
>tmap f = transformMap unCreate f unDestroy

>tapp :: (Functor f) => f :~> g -> (a -> b) -> f a -> g b
>tapp (NatTrans f) g x = f (fmap g x)

>unyoneda :: (Category cat) => cat a :~> f -> f a
>unyoneda (NatTrans f) = f id

>yonedaFunction :: (Functor t) => t a -> (->) a :~> t
>yonedaFunction f = NatTrans (\g -> fmap g f)

>inverseHoriz :: (CoFunctor k) => f :~> k -> f' :~> k'
>              -> (f :*: k') :~> (k :*: f')
>inverseHoriz s t = NatTrans (Matrix . inverseImage (nattransComponent t) . nattransComponent s . cells)

>firstTrans :: (y -> x) -> (->) (x,d) :~> (->) (y,d)
>firstTrans g = NatTrans (\ f (y,d) -> f (g y,d))

>reverseList :: [] :~> []
>reverseList = NatTrans reverse

>concatList :: ([] :*: []) :~> []
>concatList = NatTrans $ concat . cells

>returnTrans :: (Monad m) => I :~> m
>returnTrans = NatTrans (return . unI)

>joinTrans :: (Monad m) => (m :*: m) :~> m
>joinTrans = NatTrans (join . cells)

>swapDimensions :: (Applicative g, Traversable f) => (f :*: g) :~> (g :*: f)
>swapDimensions = NatTrans (Matrix . sequenceA . cells)

>data f :<~>: g = NaturalIso { fwdNaturalIso :: f :~> g,
>                              bckNaturalIso :: g :~> f }

>instance Category (:<~>:) where
>  id = NaturalIso id id
>  (NaturalIso f g) . (NaturalIso f' g') = NaturalIso (f . f') (g' . g)
>
>idIso :: f :<~>: f
>idIso = NaturalIso idTrans idTrans

>symmetricIso :: g :<~>: f -> f :<~>: g
>symmetricIso (NaturalIso f g) = NaturalIso g f

>vertIso :: g :<~>: h -> f :<~>: g -> f :<~>: h
>vertIso (NaturalIso x x') (NaturalIso y y')
>   = NaturalIso (x `vert` y) (y' `vert` x')

>horizIso :: (Functor f, Functor f') => f :<~>: f' -> g :<~>: g' -> (f :*: g) :<~>: (f' :*: g')
>horizIso (NaturalIso x xinv) (NaturalIso y yinv) = NaturalIso (x `horiz` y) (xinv `horiz` yinv)
>
>isoMatrix :: (Functor g, Functor g') => f :<~>: (g :*: h) -> g :<~>: g' -> h :<~>: h' -> f :<~>: (g' :*: h')
>isoMatrix f x y = (x `horizIso` y) `vertIso` f

>isoMatrix_ :: (Functor g') =>
> f :<~>: ((->) row :*: (->) col) -> (->) row :<~>: g' -> (->) col :<~>: h' -> f :<~>: (g' :*: h')
>isoMatrix_ f x y = (x `horizIso` y) `vertIso` f

>outerIso :: (Functor g') => (->) row :<~>: g' -> (->) col :<~>: h'
>      -> ((->) row :*: (->) col) :<~>: (g' :*: h')
>outerIso = isoMatrix_ idIso

>matrixFrom :: (Functor g) => (row -> col -> a)
>   -> ((->) row :<~>: g) -> ((->) col :<~>: h) -> (g :*: h) a
>matrixFrom f a b = runNaturalIso (outerIso a b) `runIso` (Matrix f)

>matrixIso :: (Functor f, Functor f', Functor g, Functor g')
> => f :<~>: f' -> g :<~>: g' -> a :==: b -> ((f :*: g) a) :==: ((f' :*: g') b)
>matrixIso (NaturalIso f f') (NaturalIso g g') (Iso x x') =
>   Iso (mapMatrixNat f g x) (mapMatrixNat f' g' x')


>swapDimensionsIso :: (Traversable f, Traversable g,
>                        Applicative f, Applicative g)
>                    => (f :*: g) :<~>: (g :*: f)
>swapDimensionsIso = NaturalIso swapDimensions swapDimensions

>runNaturalIso :: f :<~>: g -> f a :==: g a
>runNaturalIso (NaturalIso fwd bck) = nattransComponent fwd <-> nattransComponent bck


>newtype (f :~~> g) a = NaturalTrans { unNatTrans :: f a -> g a }


matrix_map :: (f :*: h) (a -> b)
           -> (f :*: h) a -> (g :*: i) b
matrix_map (Matrix f) (Matrix x) = unNatTrans f x

map_map :: (f :~~> g) ((h :~~> i) (a -> b)) -> f (h a) -> g (i b)

mapmatrix :: (Applicative f) => 
        f ((g :~~> h) a)
     -> (f :~~> i) (h a)
     -> (f :*: g) a
     -> (i :*: h) a

mapmatrix :: (Applicative f) => ((f :~~> i) :*: h) a
                             -> (f :*: (g :~~> h)) a
                             -> (f :*: g) a
                             -> (i :*: h) a

>transformMatrix :: (Applicative f') => ((f :~~> f') :*: g) a
>                             -> (f' :*: (g :~~> g')) a
>                             -> (f  :*: g) a
>                             -> (f' :*: g') a
>transformMatrix (Matrix u) (Matrix t)  (Matrix x) = Matrix $ pure unNatTrans <*> t <*> unNatTrans u x

>newtype NaturalTransA (arr :: k -> k -> Type) f g a = NaturalTransA { unNatTransA :: arr (f a) (g a) }
>newtype NatTransA (arr :: k -> k -> Type) f g = NatTransA { nattransComponentA :: forall a. arr (f a) (g a) }

>instance (Category arr) => Category (NatTransA arr) where
>  id = NatTransA id
>  (NatTransA f) . (NatTransA g) = NatTransA (f . g)

>idTransA :: (Arrow arr) => NatTransA arr f f
>idTransA = NatTransA returnA

>horizA :: (Arrow arr,FunctorArrow k arr arr) => NatTransA arr h k -> NatTransA arr f g -> NatTransA arr (h :*: f) (k :*: g)
>horizA s t = NatTransA (arr Matrix <<< amap (nattransComponentA t) <<< nattransComponentA s <<< arr cells)

>vertA :: (Arrow arr) => NatTransA arr g h -> NatTransA arr f g -> NatTransA arr f h
>vertA (NatTransA f) (NatTransA g) = NatTransA (f <<< g)

>invertNatTransA :: NatTransA arr f g -> NatTransA (OpA arr) g f
>invertNatTransA (NatTransA f) = NatTransA (OpA f)

>-- | For exposition of coyonedaA, see Bartosz Milewski's youtube videos
>-- https://www.youtube.com/watch?v=p_ydgYm9-yg

>coyonedaA :: (Category cat, ArrowApply arr, ArrowChoice arr)
> => arr (NatTransA arr (OpA cat a) f) (f a)
>coyonedaA = proc z -> case z of { (NatTransA f) -> f -<< id }

uncoyoneda f = NatTransA $ proc x -> inverseImageA x -<< f

>unyonedaA :: (Category cat, ArrowApply arr) => arr (NatTransA arr (cat a) f) (f a)
>unyonedaA = proc (NatTransA f) -> f -<< id

>yonedaA :: (FunctorArrow f arr arr, ArrowApply arr) => f c -> NatTransA arr (arr c) f
>yonedaA f = NatTransA (proc g -> amap g -<< f)

>natTransToA :: f :~> g -> NatTransA (->) f g
>natTransToA (NatTrans f) = NatTransA f

>arrowNatTrans :: NatTransA (->) f g -> f :~> g
>arrowNatTrans (NatTransA f) = NatTrans f


