>{-# OPTIONS_HADDOCK hide #-}
>module Math.Graph.LeftAction where
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.Monoid
>import Math.Graph.Reversible
>
>data LAct m a = LAct { lelements :: Set a, laction :: m -> a -> a }

leftActionG :: Set (a -> b) -> LAct m a -> Graph m (a -> b)
leftActionG x lg = Graph x action
  where action f alfa = \t -> f (laction lg alfa t)
