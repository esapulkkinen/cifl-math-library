>module Math.Matrix.Append where
>import Math.Number.Stream
>import Math.Matrix.Vector1

>instance AppendableVector Vector1 Stream where
>  type (Vector1 :+: Stream) = Stream
>  (Vector1 x) |> s = x `Pre` s

>splittable_codiagonal :: (Functor f, Functor (Vector1 :+: f), Functor (Vector1 :+: g),
>                SplittableVector Vector1 f, SplittableVector Vector1 g)
>            => ((Vector1 :+: f) :*: (Vector1 :+: g)) a -> (a,g a, f a, (f :*: g) a)
>splittable_codiagonal m = (a <!> (vector_element,vector_element),vector_element (cells b),
>                           fmap vector_element (cells c), d)
>   where ((a,b),(c,d)) = split_matrix m

>prefix_vector :: (AppendableVector Vector1 n) => a -> n a -> (Vector1 :+: n) a
>prefix_vector d rest = Vector1 d |> rest

>prefix_matrix :: (Applicative n, AppendableVector Vector1 m,
>                  AppendableVector Vector1 n) => 
>                  a -> (m a, n a) -> (n :*: m) a
>                  -> ((Vector1 :+: n) :*: (Vector1 :+: m)) a
>prefix_matrix d (row,col) (Matrix m) = Matrix $ 
>   (d `prefix_vector` row) `prefix_vector` liftA2 prefix_vector col m
