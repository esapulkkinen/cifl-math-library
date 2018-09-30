>{-# LANGUAGE FlexibleInstances, TypeOperators #-}
>{-# OPTIONS_HADDOCK hide #-}
>-- | Module provides action set category
>module Math.Graph.ActionSet where
>import Prelude hiding ((.),id)
>import Math.Graph
>import Math.Graph.Action
>import Math.Tools.CoFunctor
>import Control.Category
>import Control.Arrow
>import Math.Tools.NaturalTransformation hiding (unyoneda)
>import qualified Math.Tools.NaturalTransformation as NT

>data ASet a b = ASet { runASet :: (Action a) :~> (Action b) }

>instance Category ASet where
>   id = ASet id_trans
>   (ASet f) . (ASet g) = ASet (f `NT.vert` g)

>arr_aset :: (a -> b) -> ASet a b
>arr_aset = ASet . yoneda . Action

>liftASet :: Action b a -> ASet a b
>liftASet = ASet . yoneda

>actionOf :: ASet  a b -> Action b a
>actionOf = unyoneda . runASet
