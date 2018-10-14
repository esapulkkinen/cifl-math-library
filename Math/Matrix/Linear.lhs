>{-# LANGUAGE GADTs #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Matrix.Linear where
>import Prelude hiding (id,(.))
>import Control.Category
>import Math.Tools.NaturalTransformation
>import Math.Matrix.Interface
>import Math.Matrix.Matrix

>data Lin a b where
>   Linear :: (Scalar a ~ Scalar b) => (a -> b) -> Lin a b
>   BiLin  :: (Scalar a ~ Scalar b, Scalar b ~ Scalar c) 
>          => (a -> b -> c) -> Lin (a,b) c
>   LinPlus :: Lin a b -> Lin a b -> Lin a b
>   LinProd :: Scalar a -> Lin a b -> Lin a b
  
>runLinear :: (Scalar a ~ Scalar b, VectorSpace a, VectorSpace b) => Lin a b -> a -> b
>runLinear (Linear f) x = f x
>runLinear (BiLin f) (x,y) = f x y
>runLinear (LinPlus f g) a = runLinear f a %+ runLinear g a
>runLinear (LinProd k f) a = k %* runLinear f a

>instance Category Lin where
>  id = Linear id
>  (Linear f) . (Linear g)   = Linear (f . g)
>  (LinPlus x y) . z@(Linear _) = LinPlus (x . z) (y . z)
>  (LinProd k x) . z@(Linear _) = LinProd k (x . z)
>  f . (LinPlus x y) = LinPlus (f . x) (f . y)
>  f . (LinProd k x) = LinProd k (f . x)
