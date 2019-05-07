>{-# LANGUAGE Safe, DeriveGeneric, DeriveDataTypeable #-}
>{-# OPTIONS_HADDOCK hide #-}
>module Math.Graph.Regexp where
>import Data.Monoid
>import Data.Typeable
>import Data.Data
>import GHC.Generics
> 
>data RegExp = RStar RegExp
>            | RAlts [RegExp]
>            | RSeq  [RegExp]
>            | RLetter Char
>  deriving (Eq, Ord, Typeable, Data, Generic)

>instance Semigroup RegExp where
>   (<>) = mappend

>instance Monoid RegExp where
>   mempty = RSeq []
>   (RSeq x) `mappend` (RSeq y) = RSeq (x ++ y)
>   (RSeq x) `mappend` y = RSeq (x ++ [y])
>   x `mappend` (RSeq y) = RSeq ([x] ++ y)
>   x `mappend` y = RSeq [x,y]
