>{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeOperators, Arrows, Safe #-}
>module Math.Tools.NaturalTransformation where
>import Prelude hiding ((.),id)
>import Math.Tools.I
>import Control.Monad
>import Control.Applicative
>import Control.Category
>import Control.Arrow
>import Math.Tools.Adjunction
>import Math.Tools.OppositeArrow
>import Math.Tools.Arrow
>import Math.Tools.CoFunctor
>import Math.Tools.Isomorphism
>import Math.Matrix.Interface
>import Math.Matrix.Matrix

=== NATURAL TRANSFORMATION ============

>newtype f :~> g = NatTrans { nattrans_component :: forall a. f a -> g a }

>id_trans :: f :~> f
>id_trans = NatTrans id


>vert :: g :~> h -> f :~> g -> f :~> h
>vert (NatTrans f) (NatTrans g) = NatTrans (f . g)

>horiz :: (Functor h) => h :~> k -> f :~> g -> (h :*: f) :~> (k :*: g)
>horiz s t = NatTrans (Matrix .  nattrans_component s . fmap (nattrans_component t) . cells)


>map_natural_matrix :: (Functor f, Functor h) 
>           => f :~> g -> h :~> i -> (a -> b) -> (f :*: h) a -> (g :*: i) b
>map_natural_matrix f g t = map_columns f . map_rows g . fmap t

>map_rows :: (Functor h) => f :~> g -> (h :*: f) a -> (h :*: g) a
>map_rows (NatTrans f) = Matrix . fmap f . cells

>map_columns :: f :~> g -> (f :*: h) a -> (g :*: h) a
>map_columns (NatTrans f) = Matrix . f . cells

>mapMatrix :: (Functor f, Functor g)
> => f :~> f' -> g :~> g' -> (a -> b) -> (f :*: g) a -> (f' :*: g') b
>mapMatrix col row elem m = (col `horiz` row) `nattrans_component` (fmap elem m)


>unyoneda :: (Category cat) => cat a :~> f -> f a
>unyoneda (NatTrans f) = f id

>yoneda_function :: (Functor t) => t a -> (->) a :~> t
>yoneda_function f = NatTrans (\g -> fmap g f)

>inverse_horiz :: (CoFunctor k) => f :~> k -> f' :~> k'
>              -> (f :*: k') :~> (k :*: f')
>inverse_horiz s t = NatTrans (Matrix . inverse_image (nattrans_component t) . nattrans_component s . cells)

>first_trans :: (y -> x) -> (->) (x,d) :~> (->) (y,d)
>first_trans g = NatTrans (\ f (y,d) -> f (g y,d))

>reverse_list :: [] :~> []
>reverse_list = NatTrans reverse

>concat_list :: ([] :*: []) :~> []
>concat_list = NatTrans $ concat . cells

>return_trans :: (Monad m) => I :~> m
>return_trans = NatTrans (return . unI)

>join_trans :: (Monad m) => (m :*: m) :~> m
>join_trans = NatTrans (join . cells)

>swap_dimensions :: (Applicative g, Traversable f) => (f :*: g) :~> (g :*: f)
>swap_dimensions = NatTrans (Matrix . sequenceA . cells)

>data f :<~>: g = NaturalIso { fwdNaturalIso :: f :~> g,
>                              bckNaturalIso :: g :~> f }
>
>id_iso :: f :<~>: f
>id_iso = NaturalIso id_trans id_trans

>vertIso :: g :<~>: h -> f :<~>: g -> f :<~>: h
>vertIso (NaturalIso x x') (NaturalIso y y')
>   = NaturalIso (x `vert` y) (y' `vert` x')

>horizIso :: (Functor f, Functor f') => f :<~>: f' -> g :<~>: g' -> (f :*: g) :<~>: (f' :*: g')
>horizIso (NaturalIso x xinv) (NaturalIso y yinv) = NaturalIso (x `horiz` y) (xinv `horiz` yinv)

>matrixIso :: (Functor f, Functor f', Functor g, Functor g')
> => f :<~>: f' -> g :<~>: g' -> a :==: b -> ((f :*: g) a) :==: ((f' :*: g') b)
>matrixIso (NaturalIso f f') (NaturalIso g g') (Iso x x') =
>   Iso (mapMatrix f g x) (mapMatrix f' g' x')


>swap_dimensions_iso :: (Traversable f, Traversable g,
>                        Applicative f, Applicative g)
>                    => (f :*: g) :<~>: (g :*: f)
>swap_dimensions_iso = NaturalIso swap_dimensions swap_dimensions

>runNaturalIso :: f :<~>: g -> f a :==: g a
>runNaturalIso (NaturalIso fwd bck) = nattrans_component fwd <-> nattrans_component bck


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

>transform_matrix :: (Applicative f') => ((f :~~> f') :*: g) a
>                             -> (f' :*: (g :~~> g')) a
>                             -> (f  :*: g) a
>                             -> (f' :*: g') a
>transform_matrix (Matrix u) (Matrix t)  (Matrix x) = Matrix $ pure unNatTrans <*> t <*> unNatTrans u x

>newtype NaturalTransA arr f g a = NaturalTransA { unNatTransA :: arr (f a) (g a) }
>newtype NatTransA arr f g = NatTransA { nattrans_componentA :: forall a. arr (f a) (g a) }

>id_transA :: (Arrow arr) => NatTransA arr f f
>id_transA = NatTransA returnA

>horizA :: (Arrow arr,FunctorArrow k arr) => NatTransA arr h k -> NatTransA arr f g -> NatTransA arr (h :*: f) (k :*: g)
>horizA s t = NatTransA (arr Matrix <<< amap (nattrans_componentA t) <<< nattrans_componentA s <<< arr cells)

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

>yonedaA :: (FunctorArrow f arr, ArrowApply arr) => f c -> NatTransA arr (arr c) f
>yonedaA f = NatTransA (proc g -> amap g -<< f)

>natTransToA :: f :~> g -> NatTransA (->) f g
>natTransToA (NatTrans f) = NatTransA f

>arrowNatTrans :: NatTransA (->) f g -> f :~> g
>arrowNatTrans (NatTransA f) = NatTrans f


