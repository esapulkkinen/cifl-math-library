>{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
>module Math.Matrix.EigenValue where
>import Math.Matrix.Interface
>
>joinEigen :: (EigenDecomposable m a) => m (m a) -> m a
>joinEigen f = eigenvalues $ Matrix f

>eigenSolution :: (StandardBasis (m a), Fractional a, FiniteSquareMatrix m a) => (m :*: m) a -> a
>eigenSolution (mat :: (m :*: m) a) = trace mat /
>    fromRational (fromIntegral (length (unit_vectors :: [m a])))
