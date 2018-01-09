>{-# LANGUAGE FlexibleInstances #-}
>module Math.Tools.NaturalNumber where
>import Math.Tools.FixedPoint
>import Math.Tools.Orthogonal

>class HasNaturals o where
>   successor :: o -> o

>instance HasNaturals (Rec Maybe) where
>   successor = In . Just

