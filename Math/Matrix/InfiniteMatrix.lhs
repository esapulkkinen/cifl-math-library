>module Math.Matrix.InfiniteMatrix where
>import Math.Number.Stream

>-- | matrix representation as an infinite binary tree.
>data InfMatrix a = InfMatrix {
> infmatrix_diagonal :: Stream a,
> infmatrix_upper_triangular_matrix :: InfMatrix a,
> infmatrix_lower_triangular_matrix :: InfMatrix a }

