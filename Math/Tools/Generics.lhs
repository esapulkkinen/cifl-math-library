>{-# LANGUAGE RankNTypes,MultiParamTypeClasses #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Generics (module Data.Generics,
>		  GenericMT(..),
>		  everywhereM_ctx, -- GenericQ (GenericMT m) -> GenericM m -> GenericM m
>		  genFilterM
>		  ) where
>import Data.Typeable
>import Data.Data
>import Data.Generics


>data FunTypeRep = FunTypeRep { 
>     domain_type :: TypeRep,
>     codomain_type :: TypeRep,
>     function_names :: [String]
>    }

>class TypeableFunction a b where
>      typeOfFun :: (a -> b) -> FunTypeRep

>newtype GenericMT m = GenericMT (forall a. (Data a) => m a -> m a)

>everywhereM_ctx ::  (Monad m) => GenericQ (GenericMT m)
>			       -> GenericM m -> GenericM m
>everywhereM_ctx q f x = do let GenericMT h = q x
>			    x' <- h (gmapM (everywhereM_ctx q f) x)
>			    f x'

>genFilterM :: (Monad m) => GenericQ Bool -> GenericM m -> GenericM m
>genFilterM q f x = if q x then f x else return x

