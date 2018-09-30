>{-# LANGUAGE GADTs #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Lambda where
>import Math.Tools.PrettyP

>data LambdaExpr where
>   Var :: Integer -> LambdaExpr 
>   Lambda :: [LambdaExpr] -> LambdaExpr -> LambdaExpr
>   App :: LambdaExpr -> LambdaExpr -> LambdaExpr

>eval :: [LambdaExpr] -> LambdaExpr -> LambdaExpr
>eval ctx (Var i)
>   | i >= 0 && fromIntegral i < length ctx = ctx !! fromIntegral i
>   | otherwise = Var i
>eval ctx (App (Lambda ctx' b) arg) = eval (eval ctx arg : ctx' ++ ctx) b
>eval ctx (App z@(Var i) arg)
>   | i >= 0 && fromIntegral i < length ctx = eval ctx $ App (eval ctx z) arg
>   | otherwise = App (eval ctx z) (eval ctx arg)
>eval ctx (App f arg) = eval ctx (App (eval ctx f) arg)
>eval ctx (Lambda ctx' e) = Lambda (ctx' ++ ctx) e

>instance Show LambdaExpr where
>   show expr = render $ pp expr

>instance LeveledPpShow LambdaExpr where
>   exprLevel (Var _) = 2
>   exprLevel (App _ _) = 1
>   exprLevel (Lambda _ _) = 0

>instance PpShow LambdaExpr where
>   pp (Var i) = pp '@' <> pp i
>   pp (Lambda n e) = pp '\\' <> pp_list n <> pp "->" <> pp_level e 1
>   pp (App f x) = pp_level f 1 <> pp ' ' <> pp_level x 2
