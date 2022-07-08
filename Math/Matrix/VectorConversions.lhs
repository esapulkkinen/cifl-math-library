>{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies #-}
>module Math.Matrix.VectorConversions where
>import Math.Number.StreamInterface
>import Math.Matrix.Interface
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Simple
>import Math.Matrix.Points
>import Math.Number.Stream

>vector1_to_vec1 :: Vector1 a -> OneD -> a
>vector1_to_vec1 (Vector1 x) = svec1 x

>vector2_to_vec2 :: Vector2 a -> TwoD -> a
>vector2_to_vec2 (Vector2 x y) = svec2 x y

>vector4_to_vec4 :: Vector4 a -> FourD -> a
>vector4_to_vec4 (Vector4 x y z t) = svec4 x y z t

>toSimple21 :: (ConjugateSymmetric a, Num a) => (Vector2 :*: Vector1) a -> (TwoD :&: OneD) a
>toSimple21 m = linear $ Matrix $ m <!> (vector2_to_vec2, vector1_to_vec1)

>toSimple22 :: (ConjugateSymmetric a, Num a) => (Vector2 :*: Vector2) a -> (TwoD :&: TwoD) a
>toSimple22 m = linear $ Matrix $ m <!> (vector2_to_vec2, vector2_to_vec2)

>toSimple12 :: (ConjugateSymmetric a, Num a) => (Vector1 :*: Vector2) a -> (OneD :&: TwoD) a
>toSimple12 m = linear $ Matrix $ m <!> (vector1_to_vec1, vector2_to_vec2)

>instance AppendableVector Vector2 Stream where
>  type (Vector2 :+: Stream) = Stream
>  (Vector2 x y) |> s = x `Pre` y `Pre` s
