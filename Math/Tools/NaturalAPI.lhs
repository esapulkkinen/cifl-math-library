>{-# OPTIONS_HADDOCK hide #-}
>module Math.Tools.NaturalAPI where
>import Math.Tools.PrettyP

>class Naturalistic n where
>      zero_natu :: n
>      plus_natu :: n -> n -> n
