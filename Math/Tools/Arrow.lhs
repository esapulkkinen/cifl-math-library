>{-# LANGUAGE Safe,Rank2Types, KindSignatures, MultiParamTypeClasses,
>             FunctionalDependencies, FlexibleInstances, TypeSynonymInstances,
>             TupleSections,  TypeOperators, AllowAmbiguousTypes, Arrows,TypeFamilies,
>             LambdaCase
> #-}
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
>import safe Math.Tools.PrettyP (render,pp)

>class (Category ar, Category ar') => ArrowTransformation ar ar' where
>      mapA :: ar a b -> ar' a b

>class (Category arr, Monoid m) => MonoidArrow arr m a where
>   monoidA :: m -> arr a a

>-- | <https://en.wikipedia.org/wiki/Monoidal_category>
>class (Category arr) => MonoidalCategory arr where
>   type Prod arr a b
>   type MUnit arr
>   (-*-)   :: arr a b -> arr a' b' -> arr (Prod arr a a') (Prod arr b b')
>   monoidal_assoc   :: arr (Prod arr (Prod arr a b) c) (Prod arr a (Prod arr b c))
>   monoidal_deassoc :: arr (Prod arr a (Prod arr b c)) (Prod arr (Prod arr a b) c)
>   leftunitor :: arr (Prod arr (MUnit arr) a) a
>   unleftunitor :: arr a (Prod arr (MUnit arr) a)
>   rightunitor :: arr (Prod arr a (MUnit arr)) a
>   unrightunitor :: arr a (Prod arr a (MUnit arr))

>-- <https://en.wikipedia.org/wiki/Braided_monoidal_category>
>class (MonoidalCategory arr) => BraidedCategory arr where
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

>class OpArrow p arr arr' where
>   inverse_imageA :: arr a b -> arr' (p b) (p a)

>class (Category arr) => CoArrow arr where
>   coarr   :: (b -> a) -> arr a b
>   coleft  :: arr a b -> arr (Either a d) (Either b d)
>   coright :: arr a b -> arr (Either d a) (Either d b)

>class (Category arr) => BiArrow arr where
>   (<->) :: (a -> b) -> (b -> a) -> arr a b

>class (Category arr) => CPSArrow arr where
>   callcc :: (forall bot. arr c bot -> arr b c) -> arr b c

>-- | Arrow version of Functor from Haskell Prelude
>class FunctorArrow (f :: * -> *) (arr :: * -> * -> *) where
>      amap :: arr c d -> arr (f c) (f d)

>-- | Arrow version of Applicative from Haskell Prelude
>class (FunctorArrow f arr) => ApplicativeArrow (f :: * -> *) (arr :: * -> * -> *) where
>   apure :: arr a (f a)
>   aapply :: f (arr a b) -> arr (f a) (f b)

>class CoFunctorArrow (f :: * -> *) (arr :: * -> * -> *) where
>      inverseImage :: arr c d -> arr (f d) (f c)

>class (Category arr, Category arr')
>     => AdjunctionArrow arr arr' f g
>       | f -> g, g -> f, arr -> arr', arr' -> arr where
>   ladA    :: arr (f a) b  -> arr' a (g b)
>   radA    :: arr' a (g b) -> arr (f a) b
>   unitA   :: arr' a (g (f a))
>   counitA :: arr  (f (g b)) b
>   unitA = ladA id
>   counitA = radA id

>outerA :: (FunctorArrow f arr, FunctorArrow g arr, ArrowApply arr)
>   => arr (a,b) c -> arr (f a,g b) (f (g c))
>outerA f = proc (x,y) -> amap (proc a -> amap (proc b -> f -< (a,b)) -<< y) -<< x 

>class (FunctorArrow f arr) => InitialAlgebraA arr a f | a -> f where
>   deconstructA :: arr a (f a)

>class LoopArrow a where 
>      loop_secondSA  :: a (b,d) (c,d) -> a b c
>      loop_firstSA :: a (d,b) (d,c) -> a b c

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

>class (Category a) => ArrowFix a where
>      fixArrow :: a (b,c) c -> a b c

>class (Category a) => ArrowCircuit a where
>      delay :: b -> a b b

>class ArrowDual a d where
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


>instance (ArrowChoice arr) => FunctorArrow (Either b) arr where
>    amap f = id +++ f

>instance (ArrowChoice a) => FunctorArrow Maybe a where
>    amap f = proc ma -> case ma of
>             Nothing -> returnA -< Nothing
>             (Just x) -> do x' <- f -< x
>                            returnA -< (Just x')

>instance (ArrowChoice arr) => FunctorArrow [] arr where
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

