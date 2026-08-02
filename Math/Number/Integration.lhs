>module Math.Number.Integration where
>import Math.Number.R

>data Term r = TApply (r -> r) (Term r) -- f(x)
>   | TDerivativeApply (Diff (Term r)) (Diff (Term r)) (Term r) -- (df/dx)(E)
>   | TVariableX -- x
>   | TConstant r -- c
>   | TSum [Term r] -- f_0(x) + ... + f_k(x)
>   | TProduct [Term r] -- f_0(x)*...*f_k(x)
>   | TNegate (Term r)  -- -f(x)

>data Diff a = DF { df_per_dx :: a, function :: a } -- df = f'(x)dx. 
>          | DiffMultiply a (Diff a) -- f(x)dg = f(x)g'(x)dx
>          | DiffDivide (Diff a) a   -- dg/f(x) = (g'(x)/f(x))dx
>          | DiffPlus [Diff a] -- df_0 + ... + df_k


> -- integral(f(x)dx, C) = F(x) + C, where [dF/dx](x) = f(x)
>symbolicIntegral :: Diff (Term R) -> Term R -> Term R
>symbolicIntegral dz@(DF _ _) c = TSum [function dz, c]
>symbolicIntegral (DiffPlus lst) c = TSum (map (`symbolicIntegral` c) lst)
