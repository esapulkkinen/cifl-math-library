>module Math.Number.Symbolic where
>import Math.Number.R
> 
>data Expr a where
>  EFromRational :: Expr Rational -> Expr R
>  EIntegral :: (Expr R, Expr R) -> Expr (R -> R) -> Expr R
>  ELambda :: (Expr a -> Expr b) -> Expr (a -> b)
>  EApp    :: Expr (a -> b) -> Expr a -> Expr b
>  EAdd    :: Expr a -> Expr a -> Expr a
>  ENegate :: Expr a -> Expr a
>  ELess :: Expr R -> Expr R -> Expr Bool
>  
