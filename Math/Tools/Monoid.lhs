>{-# LANGUAGE GADTs, FlexibleInstances #-}
>module Math.Tools.Monoid where
>import Prelude hiding (id,(.))
>import Control.Applicative
>import Control.Category
>import Control.Arrow
>import Math.Tools.Arrow

>newtype Elementwise a = Elementwise a

>instance (Semigroup a, Applicative f) => Semigroup (Elementwise (f a)) where
>   (Elementwise x) <> (Elementwise y) = Elementwise $ liftA2 (<>) x y
>
>instance (Monoid a, Applicative f) => Monoid (Elementwise (f a)) where
>   mempty = Elementwise $ pure mempty
>   mappend = (<>)

>data MonoidT arr a b where
>    Mon :: arr a a -> MonoidT arr a a

>data GroupT arr a b where
>    GroupT :: arr a a -> arr a a -> GroupT arr a a

>instance (Category arr) => Category (MonoidT arr) where
>   id = Mon id
>   (Mon f) . (Mon g) = Mon (f . g)

>instance (Category arr) => Category (GroupT arr) where
>   id = GroupT id id
>   (GroupT f f') . (GroupT g g') = GroupT (f . g) (g' . f')

>instance (Category arr) => Groupoid (GroupT arr) where
>   invertA (GroupT f g) = GroupT g f


