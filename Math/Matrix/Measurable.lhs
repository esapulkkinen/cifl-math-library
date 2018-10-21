>{-# OPTIONS_HADDOCK hide,prune #-}
>module Math.Matrix.Measurable where
>import Prelude hiding (id,(.))
>import Control.Category
>
>data Measurable a b = Measurable {
>   measure_function :: a -> b,
>   measure_property :: ((b -> Bool) -> Bool) -> (a -> Bool) -> Bool }
>
>instance Category Measurable where
>  id = Measurable id id
>  (Measurable f f') . (Measurable g g') = Measurable (f . g) (g' . f')
