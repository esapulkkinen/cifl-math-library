>{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
>module Math.Tools.StateMachine where
>import Prelude hiding ((.),id)
>import Control.Category
>import Math.Tools.Arrow
>import Math.Tools.Isomorphism
>import Math.Tools.CoFunctor

Lawvere&Rosebrugh: Set for Mathematics

>data Action s alpha = Action { readCharacter :: alpha -> s -> s }

>readChar :: Action String Char
>readChar = Action (:)

>instance CoFunctor (Action c) where
>   inverse_image f (Action g) = Action (g . f)

>binary_op :: (s -> s -> s) -> Action s alpha -> Action s alpha -> Action s alpha
>binary_op op (Action f) (Action g) = Action $ \x s -> f x s `op` g x s

>monoid_op :: ((a -> a) -> (b -> b) -> c -> c)
>          -> Action a d -> Action b d -> Action c d
>monoid_op op (Action f) (Action g) = Action $ \x -> f x `op` g x

>data ST alpha a b = ST (a -> b) (Action a alpha -> Action b alpha)

>isoAct :: a :==: b -> (alpha -> a -> a) -> alpha -> b -> b
>isoAct i f j s = isomorphism_epimorphism i $ f j $ isomorphism_section i s

>unyoneda :: ST (a -> a) a b -> Action b (a -> a)
>unyoneda (ST _ g) = g (Action id)

>runAction :: ST alpha a b -> Action a alpha -> Action b alpha
>runAction (ST _ g) = g

>instance BiArrow (ST alpha) where
>   f <-> g = isoToST (f <-> g)


>isoToST :: a :==: b -> ST alpha a b
>isoToST i = ST (isomorphism_epimorphism i) $ \ (Action f) -> Action (isoAct i f)

>instance ArrowTransformation (:==:) (ST alpha) where
>  mapA = isoToST

>instance Category (ST alpha) where
>   id = ST id id
>   (ST f f2) . (ST g g2) = ST (f . g) (f2 . g2)

>write :: Action String String
>write = Action (++)

