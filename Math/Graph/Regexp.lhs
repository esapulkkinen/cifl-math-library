>{-# LANGUAGE Safe #-}
>module Math.Graph.Regexp where
>instance Data.Monoid

>data RegExp = RStar RegExp
>            | RAlts [RegExp]
>            | RSeq  [RegExp]
>            | RLetter Char

>instance Monoid RegExp where
>   mempty = RSeq []
>   (RSeq x) `mappend` (RSeq y) = RSeq (x ++ y)
>   (RSeq x) `mappend` y = RSeq (x ++ [y])
>   x `mappend` (RSeq y) = RSeq ([x] ++ y)
>   x `mappend` y = RSeq [x,y]
