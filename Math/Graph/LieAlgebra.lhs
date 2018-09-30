>{-# OPTIONS_HADDOCK hide #-}
>module Math.Graph.LieAlgebra where
>import Data.Set
>import Data.Monoid
>import Math.Matrix.Interface

>data LieGraph m a = LieGraph { la_elements :: Set a, la_morphism :: m -> Endo a }
>
>liegraph_representation :: (VectorSpace a) => Endo a -> Endo a -> Endo a
>liegraph_representation (Endo g) (Endo f) = Endo $ \m -> g (f m) %- f (g m)

