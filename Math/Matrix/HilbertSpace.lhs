>{-# LANGUAGE Safe, FlexibleContexts #-}
>module Math.Matrix.HilbertSpace where
>import Math.Matrix.Interface
> 
>-- | Hilbert space ideas from K Chandrasekhara Rao: Functional Analysis
>
>-- | Grahm-Schmidt orthogonalization process.
>orthogonalize :: (Floating (Scalar a), Num a, InnerProductSpace a, VectorSpace a) => [a] -> [a]
>orthogonalize lst = orthogonalize_impl lst []
>  where orthogonalize_impl (c:cr) []  = orthogonalize_impl cr [(1 / innerproductspace_norm c) %* c]
>        orthogonalize_impl (c:cr) lst
>            = orthogonalize_impl cr ((c - sum [(c %. e) %* e | e <- lst]) : lst)
>        orthogonalize_impl [] lst = lst

