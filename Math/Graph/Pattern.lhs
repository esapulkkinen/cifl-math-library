>{-# LANGUAGE FlexibleContexts #-}
>module Math.Graph.Pattern where
>import Math.Graph.GraphMonoid
>import Math.Graph.Reversible
>import Data.Monoid
>import Data.List

>data GraphExpr a =
>     EdgeG { edge_name :: a, source_vertex :: a, target_vertex :: a }
>   | UnionG [GraphExpr a]
>   | IntersectionG [GraphExpr a]
> deriving (Read)

>instance (Eq a, Show a) => Show (GraphExpr a) where
>  show (EdgeG x a b)
>    | isVertex x a b = show x
>    | otherwise = show x ++ ":" ++ show a ++ " -> " ++ show b
>  show (UnionG lst) = "[" ++ concat (intersperse "," (map show lst)) ++ "]"
>  show (IntersectionG lst) = "{" ++ concat (intersperse "," (map show lst)) ++ "}"

>isVertex s a b | s == a && s == b = True
>               | otherwise = False

>eval_graphexpr :: (Show a, Ord a) => GraphExpr a -> Graph Three a
>eval_graphexpr (EdgeG x s t) = edgeG x s t
>eval_graphexpr (UnionG lst) = foldr unionG emptyG $ map eval_graphexpr lst
>eval_graphexpr (IntersectionG lst) = foldr1 intersectionG $ map eval_graphexpr lst
