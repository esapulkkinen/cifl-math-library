>module Math.Number.VectorExponentials where
>import Math.Matrix.Interface
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Matrix.Covector
>import Math.Matrix.Linear
>import Math.Number.Stream
>import Math.Number.R

>instance (Num a) => FiniteDimensional (Vector4 a) where
>   finite (Matrix (Covector f)) = Vector4
>                                    (f (covector tcoord4))
>                                    (f (covector xcoord4))
>                                    (f (covector ycoord4))
>                                    (f (covector zcoord4))

>instance MetricSpace (Vector4 R) where
>   distance x y = norm (x %- y)

>matrix_exponential3 :: (Closed b, ConjugateSymmetric b, Fractional b)
>   => Matrix3 b -> Matrix3 b
>matrix_exponential3 m = Stream.ssum $ 
>    liftA2 (\x y -> fmap (/y) x)
>               (let v = Pre identity3 (fmap (m `matrix_multiply3`) v) in v)
>               Stream.factorial

>-- | <http://en.wikipedia.org/wiki/Hyperbolic_function hyperbolic function>
>instance (Floating a, Closed a, ConjugateSymmetric a) => Floating ((Vector3 :*: Vector3) a) where
>   pi = error "Matrix pi is not implemented."
>   exp = matrix_exponential3
>   log = error "Matrix log is not implemented."
>   cos = error "Matrix cosine is not implemented."
>   sin = error "Matrix sine is not implemented."
>   tan = error "Matrix tangent is not implemented."
>   sinh x = (exp x - exp (negate x)) / 2
>   cosh x = (exp x + exp (negate x)) / 2
>   tanh x = sinh x / cosh x
>   acos = error "Matrix acos is not implemented."
>   asin = error "Matrix asin is not implemented."
>   atan = error "Matrix atan is not implemented."
>   asinh = error "Matrix asinh is not implemented."
>   acosh = error "Matrix acosh is not implemented."
>   atanh = error "Matrix atanh is not implemented."
