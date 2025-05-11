>{-# LANGUAGE Trustworthy,Rank2Types, KindSignatures, MultiParamTypeClasses,
>             FunctionalDependencies, FlexibleInstances, TypeSynonymInstances,
>             TupleSections,  TypeOperators, AllowAmbiguousTypes, Arrows,TypeFamilies,
>             LambdaCase, IncoherentInstances, FlexibleContexts, PolyKinds
> #-}
>{-# LANGUAGE LinearTypes #-}
>-- |
>-- Module: Math.Tools.Arrow
>-- License: LGPL
>-- Copyright: Esa Pulkkinen, 2018
>--
>-- Arrows were originally described by "Paterson R.: A New notation for arrows, 2001" <http://www.staff.city.ac.uk/~ross/papers/notation.html>
>-- 
>module Math.Tools.Arrow where
>import Text.PrettyPrint hiding (render)
>import System.IO.Error
>import Control.Monad.Trans.Except
>import Control.Exception
>import Control.Arrow 
>import Control.Category
>import Data.Monoid
>import Prelude hiding (id,(.))
>import Math.Tools.PrettyP (render,pp)
>import Math.Tools.I

>class (Category ar, Category ar') => ArrowTransformation ar ar' where
>      mapA :: ar a b -> ar' a b

>class (Category arr, Monoid m) => MonoidArrow arr m a where
>   monoidA :: m -> arr a a

>-- | <https://en.wikipedia.org/wiki/Monoidal_category>
>class (Category arr) => MonoidalCategory (arr :: k -> k -> *) where
>   type Prod (arr :: k -> k -> *) (a :: k) (b :: k) :: k
>   type MUnit (arr :: k -> k -> *) :: k
>   (-*-)   :: arr a b -> arr a' b' -> arr (Prod arr a a') (Prod arr b b')
>   monoidal_assoc   :: arr (Prod arr (Prod arr a b) c) (Prod arr a (Prod arr b c))
>   monoidal_deassoc :: arr (Prod arr a (Prod arr b c)) (Prod arr (Prod arr a b) c)
>   leftunitor :: arr (Prod arr (MUnit arr) a) a
>   unleftunitor :: arr a (Prod arr (MUnit arr) a)
>   rightunitor :: arr (Prod arr a (MUnit arr)) a
>   unrightunitor :: arr a (Prod arr a (MUnit arr))

>-- <https://en.wikipedia.org/wiki/Braided_monoidal_category>
>class (MonoidalCategory arr) => BraidedCategory (arr :: k -> k -> *) where
>   braiding :: arr (Prod arr a b) (Prod arr b a)

>class (Category c) => OverCategory c y where
>   overmap :: c a b -> c b y -> c a y

>class (Category arr, Category arr') => OppositeCategoryAction arr arr' where
>   categoryA :: arr a' a -> arr' x a -> arr' x a'

>class (Category arr) => FailureArrow arr err where
>      failA   :: arr err b
>      catchA  :: arr a b -> arr err b -> arr a b

>class (Category arr) => Groupoid arr where
>     invertA :: arr a b -> arr b a

>-- | <https://ncatlab.org/nlab/show/relative+monad>
>class (Category arr) => RelativeMonad arr j m where
>     relativeUnitA :: arr (j x) (m x)
>     extA :: arr (j x) (m y) -> arr (m x) (m y)
> -- laws: f == extA f . relativeUnitA
> --       extA relativeUnitA == id
> --       extA (extA g . f) == extA g . extA f

>relativeBind :: (RelativeMonad arr j m, ArrowApply arr) => arr (m x, arr (j x) (m y)) (m y)
>relativeBind = proc (x,f) -> extA f -<< x

>relativeBind2 :: (RelativeMonad arr j m, RelativeMonad arr k m, ArrowApply arr) =>
>   arr (j a, k b) (m c) -> arr (m a, m b) (m c)
>relativeBind2 f = proc (x,y) ->
>     relativeBind -< (x, proc a ->
>         relativeBind -< (y, proc b ->
>             f -< (a, b)))

relativeBind2 f x y = relativeBind x $ \a -> relativeBind y $ \b -> f x y

>class (Category arr, Category arr') => OpArrow p arr arr' where
>   inverse_imageA :: arr a b -> arr' (p b) (p a)

>class (Category arr) => CoArrow arr where
>   coarr   :: (b -> a) -> arr a b
>   coleft  :: arr a b -> arr (Either a d) (Either b d)
>   coright :: arr a b -> arr (Either d a) (Either d b)

>class (Category arr) => BiArrow arr where
>   (<->) :: (a -> b) -> (b -> a) -> arr a b

>class (Category arr) => LinearBiArrow arr m where
>  (<==>) :: (a %m -> b) -> (b %m -> a) -> arr a b

>class (Category arr, Category m) => MonoidCategory arr m f | f -> m arr where
>   monoidArr :: m a a -> arr (f a) (f a)

>class (BiArrow arr, FunctorArrow f arr arr) => ApplicativeBiArrow arr f where
>   bipure :: arr a (f a)
>   bimap :: arr (f (arr a b), f a) (f b)
>   biliftA2 :: arr a (arr b c) -> arr (f a, f b) (f c)
>   bimap = biliftA2 Control.Category.id

>class (Category arr) => CPSArrow arr where
>   callcc :: (forall bot. arr c bot -> arr b c) -> arr b c

>-- | Arrow version of Functor from Haskell Prelude
>class (Category arr, Category arr') => FunctorArrow f arr arr' | f arr' -> arr, f arr -> arr' where
>      amap :: arr c d -> arr' (f c) (f d)

>-- | Arrow version of Applicative from Haskell Prelude
>class (FunctorArrow f arr arr, Category arr) => ApplicativeArrow f arr where
>   apure :: arr a (f a)
>   aapply :: f (arr a b) -> arr (f a) (f b)

>alift2 :: (ApplicativeArrow f arr, ArrowApply arr)
>  => f (arr a (arr b c)) -> arr (f a, f b) (f c)
>alift2 f = proc (x,y) -> do
>   fx <- aapply f -< x
>   aapply fx -<< y

>class (Category arr, Category arr') => CoFunctorArrow f arr arr' where
>      inverseImageArr :: arr c d -> arr' (f d) (f c)

>(|>) :: (CoFunctorArrow f (->) (->)) => f d -> (c -> d) -> f c
>x |> f = inverseImageArr f x

>class (Category arr, Category arr')
>     => AdjunctionArrow arr arr' f g
>       | f -> g, g -> f, arr -> arr', arr' -> arr where
>   ladA    :: arr (f a) b  -> arr' a (g b)
>   radA    :: arr' a (g b) -> arr (f a) b
>   unitA   :: arr' a (g (f a))
>   counitA :: arr  (f (g b)) b
>   unitA = ladA id
>   counitA = radA id

>outerA :: (Arrow arr2, FunctorArrow f arr arr', FunctorArrow g arr2 arr, ArrowApply arr, ArrowApply arr')
>   => arr2 (a,b) c -> arr' (f a,g b) (f (g c))
>outerA f = proc (x,y) -> amap (mapper y) -<< x 
>   where inner a = proc b -> f -< (a,b)
>         mapper y = proc a -> amap (inner a) -<< y

>class (FunctorArrow f arr arr) => InitialAlgebraA arr a f | a -> f where
>   deconstructA :: arr a (f a)

>class (Category a) => LoopArrow a prod where 
>      loop_secondSA  :: a (prod b d) (prod c d) -> a b c
>      loop_firstSA :: a (prod d b) (prod d c) -> a b c

>class (Category a) => MessagingArrow a where 
>      splitA :: a (Either b c) (Either d e) -> (a b d, a c e)

>class (Category a) => UnifyingArrow a where
>      joinA :: a (Either b b) b

>class (Category a) => DoublingArrow a where
>      doubleA :: a b (b,b)

>class (Category a) => CircuitArrow a where
>      delayA :: b -> a b b 

>class (Category a) => StorageArrow a where
>      storageA :: a b c -> (a b (), a () c)

>class (Category a) => ArrowUnit a where
>      aunit :: a b ()

>class (Category a) => ArrowFix a prod where
>      fixArrow :: a (prod b c) c -> a b c

>class (Category a) => ArrowCircuit a where
>      delay :: b -> a b b

>class (Category a) => ArrowDual a d where
>      collapseArrow :: (d (d a)) b c -> a b c

>class ArrowDual a d => RecArrow a d where
>      recArrow :: a b c -> (d a) b c

>class ArrowDual a d => ArrowDualApp a d where
>      appArrow :: a (a b c, d a b c) c

>instance (Arrow ar) => ArrowTransformation (->) ar where
>   mapA f = arr f

>instance FailureArrow (Kleisli IO) [Doc] where
>   failA = Kleisli (\e -> throwIO (userError (render (vcat e))))
>   catchA (Kleisli f) (Kleisli h) = Kleisli (\i -> f i `catch` \err -> if isUserError err then h [pp (ioeGetErrorString err)] else throwIO err)

>instance FailureArrow (Kleisli IO) IOError where
>   failA = Kleisli throwIO
>   catchA (Kleisli f) (Kleisli h) = Kleisli (\i -> f i `catch` \err -> h err)

>instance FailureArrow (Kleisli IO) Doc where
>   failA = Kleisli (throwIO . userError . render)
>   catchA (Kleisli f) (Kleisli h) = Kleisli (\i -> f i `catch` \err -> if isUserError err then h (pp (ioeGetErrorString err)) else throwIO err)


>instance (ArrowChoice arr) => FunctorArrow (Either b) arr arr where
>    amap f = id +++ f

>instance (ArrowChoice arr) => FunctorArrow Maybe arr arr where
>    amap f = proc ma -> case ma of
>             Nothing -> returnA -< Nothing
>             (Just x) -> do x' <- f -< x
>                            returnA -< (Just x')

>instance (ArrowChoice arr) => FunctorArrow [] arr arr where
>   amap f = proc lst -> case lst of
>            [] -> returnA -< []
>            (c:cr) -> do c' <- f -< c
>                         cr' <- amap f -< cr
>                         returnA -< (c':cr')

>-- | <https://en.wikipedia.org/wiki/Monoidal_category>
>instance MonoidalCategory (->) where
>   type Prod (->) a b = (a,b)
>   type MUnit (->) = ()
>   (-*-) = (***)
>   monoidal_assoc ((a,b),c) = (a,(b,c))
>   monoidal_deassoc (a,(b,c)) = ((a,b),c)
>   leftunitor ((),a) = a
>   unleftunitor a = ((),a)
>   rightunitor (a,()) = a
>   unrightunitor a = (a,())

>instance BraidedCategory (->) where
>   braiding (a,b) = (b,a)

>instance FunctorArrow I (->) (->) where
>  amap f = arr I . f . arr unI
