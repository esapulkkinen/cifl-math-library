>{-# LANGUAGE Safe #-}
>module Math.Tools.Show where

>showsBinaryOp :: (Show a) => Int -> String -> Int -> a -> a -> ShowS
>showsBinaryOp i op p x y = showParen (p > i) $
>                             showsPrec (i+1) x . showString op . showsPrec (i+1) y

>punctuate_list :: (Show a) => String -> [a] -> ShowS
>punctuate_list _ [] = showString ""
>punctuate_list _ [c] = shows c
>punctuate_list s (c:r) = shows c . showString s . punctuate_list s r
