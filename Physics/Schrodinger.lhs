>module Physics.Schrodinger where
>import Math.Matrix.Interface
>import Math.Number.Interface
>import Math.Matrix.Vector4
>import Math.Matrix.Vector3
>import Math.Matrix.Covector
>import Data.Complex
>import Math.Number.DimensionalAnalysis

>d_per_dt f (Vector4 t x y z) = partial1_4 ff t x y z
>   where ff t' x' y' z' = f (Vector4 t' x' y' z')

>hbar :: Complex (Quantity a)
>hbar = reduced_planck_constant :+ 0


>schrodinger ::
> (Show a, RealFloat a) 
> => (Vector4 (Complex (Quantity a)) -> Complex (Quantity a))
> -> (Vector3 (Complex (Quantity a)) -> Complex (Quantity a))
> -> Complex (Quantity a)
> -> Vector4 (Complex (Quantity a)) -> Complex (Quantity a)
>schrodinger gamma v m xt@(Vector4 t x y z) = let
>  left_side = ((0 :+ 1) %* hbar) * d_per_dt gamma xt
>  right_side = - (hbar * hbar / (2* amount m)) %* (∇·∇) gamma xt %+ v r
>  r = Vector3 x y z
> in left_side %- right_side
