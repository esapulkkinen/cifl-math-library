>module Math.Number.MetricSpace where
>import Math.Matrix.Vector3
>import Math.Number.R
>import Math.Matrix.Interface

>instance MetricSpace (Vector3 R) where
>  distance (Vector3 x y z) (Vector3 x' y' z') = sqrt $
>       (x'-x)*(x'-x) + (y'-y)*(y'-y) + (z'-z)*(z'-z)
