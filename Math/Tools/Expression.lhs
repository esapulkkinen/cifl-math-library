>{-# LANGUAGE Safe #-}
>module Math.Tools.Expression where
>import Math.Tools.Id

>class Expression e where
>   freevars :: e -> [Id]
