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

>newtype ASet a b = ASet { runASet :: ((:<-:) a) :~> ((:<-:) b) }

>instance Category ASet where
>   id = ASet idTrans
>   (ASet f) . (ASet g) = ASet (f `NT.vert` g)

>arr_aset :: (a -> b) -> ASet a b
>arr_aset = ASet . yoneda . Action

>liftASet :: b :<-: a -> ASet a b
>liftASet = ASet . yoneda

>actionOf :: ASet  a b -> b :<-: a
>actionOf = unyoneda . runASet

