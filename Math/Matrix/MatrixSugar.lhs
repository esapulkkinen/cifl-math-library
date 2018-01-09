>module Math.Matrix.MatrixSugar where
>import Control.Monad
>import Math.Tools.Adjunction
>import Math.Matrix.Interface
>import Math.Matrix.Matrix
>import Math.Matrix.Simple
>
>bindadj :: (Adjunction f g) => (f a, g b) -> (a -> b -> c) -> c
>bindadj (x,y) f = counit $ cells $ matrix f x y

>adjapply :: (Adjunction f g) => f (b -> c) -> g b -> c
>adjapply x y = (x,y) `bindadj` id

>adjreduce :: (Adjunction f g) => f a -> g b -> (a,b)
>adjreduce x y = (x,y) `bindadj` (,)

inverseIndex :: (Adjunction f g) => (a :&: f b) c -> (a,b) -> g c
inverseIndex m = m <!> (rightAdjunct, leftAdjunct)

>inverseBind2 :: (Monad m, Adjunction f m) =>
>   (a,b) -> (f a -> f b -> m c) -> m c
>inverseBind2 (x,y) f = join $ join $ cells $ matrix f (unit x) (unit y)

>inverseBind :: (Monad m, Adjunction f m) =>
>   a -> (f a -> m b) -> m b
>inverseBind x f = join $ fmap f $ unit x

>(>===) :: (Monad m) => (m a, m b) -> (a -> b -> c) -> m c
>(x,y) >=== f = join $ cells $ matrix f x y

>(>>>=) :: (Monad m) => (m a, m b) -> (a -> b -> m c) -> m c
>(x,y) >>>= f = join (join (cells $ matrix f x y))

>(>>==) :: (Monad f, Monad g, Indexable f)
>       => (f a, g b) -> (a -> b -> (f :*: g) c) -> (f :*: g) c
>(x,y) >>== f = join (matrix f x y)
