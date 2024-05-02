>{-# LANGUAGE GADTs, TypeOperators, TypeFamilies #-}
>module Math.Matrix.Interpreter where
>import Math.Matrix.Interface
>import Math.Number.NumericExpression
>
>instance VectorSpace (MatrixExpr a) where
>   type Scalar (MatrixExpr a) = NumExpr MatrixExpr a
>   vzero = VVectorSpaceExpr VZero
>   vnegate x = VVectorSpaceExpr (VNegate x)
>   a %+ b = VVectorSpaceExpr (VPlus a b)
>   k %* x = VVectorSpaceExpr (VScalar k x)
> 
>data MatrixExpr a where
>  VVectorSpaceExpr :: (VectorSpace (v a)) => VectorSpaceExpr v a -> MatrixExpr (v a)
>  VInnerProductExpr :: (InnerProductSpace v) => MatrixExpr v -> MatrixExpr v -> MatrixExpr (Scalar v)
>  VVectorProdMatExpr :: (LinearTransform m n a) => MatrixExpr (n a) -> MatrixExpr ((m :*: n) a) -> MatrixExpr (m a)
>  VMatProdVectorExpr :: (LinearTransform m n a) => MatrixExpr ((m :*: n) a) -> MatrixExpr (m a) -> MatrixExpr (n a)
>  VDistanceExpr :: (MetricSpace s) => MatrixExpr s -> MatrixExpr s -> MatrixExpr (Scalar s)
>  VNorm :: (NormedSpace s) => MatrixExpr s -> MatrixExpr (Scalar s)
>  Vec1Expr :: MatrixExpr a -> MatrixExpr (Vector1 a)
>  Vec2Expr :: MatrixExpr a -> MatrixExpr a -> MatrixExpr (Vector2 a)
>  Vec3Expr :: MatrixExpr a -> MatrixExpr a -> MatrixExpr a -> MatrixExpr (Vector3 a)
>  Vec4Expr :: MatrixExpr a -> MatrixExpr a -> MatrixExpr a -> MatrixExpr a -> MatrixExpr (Vector4 a)
>  MatExpr :: (Functor m, Functor n)
>      => (MatrixExpr a -> MatrixExpr b -> MatrixExpr c)
>      -> m (MatrixExpr a) -> n (MatrixExpr b) -> MatrixExpr ((m :*: n) c)


>matrixEval :: MatrixExpr a -> a
>matrixEval (VVectorSpaceExpr e) = eval e
>matrixEval (VInnerProductExpr a b) = matrixEval a %. matrixEval b
>matrixEval (VVectorProdMatExpr v m) = matrixEval v <*>> matrixEval m
>matrixEval (VMatProdVectorExpr m v) = matrixEval m <<*> matrixEval v
>matrixEval (VDistanceExpr a b) = distance (matrixEval a) (matrixEval b)
>matrixEval (VNorm a) = norm (matrixEval a)
>matrixEval (Vec1Expr a) = Vector1 (matrixEval a)
>matrixEval (Vec2Expr a b) = Vector2 (matrixEval a) (matrixEval b)
>matrixEval (Vec3Expr a b c) = Vector3 (matrixEval a) (matrixEval b) (matrixEval c)
>matrixEval (Vec4Expr a b c d) = Vector4 (matrixEval a) (matrixEval b) (matrixEval c) (matrixEval d)
>matrixEval (MatExpr f a b) = matrix (\a' b' -> matrixEval (f a' b')) a b

