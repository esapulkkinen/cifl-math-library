>{-# LANGUAGE Safe #-}
>module Math.Graph.Tex where
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad
>import Data.Set

>packageImports = "\\usepackage{tikz}"

>data TexToken = EnvToken { envName :: String,
>                           envOpts :: [String],
>                           contents :: [TexToken] }
>              | NodesToken { nodesList :: [TikzNode] }
>              | EdgesToken { edgesList :: [TikzEdge] }

>data NodeType = NodeType {
>   style_name :: String,
>   style_def :: String
> }

>data TikzNode = TikzNode {
>  node_type :: NodeType,
>  node_name :: String
> }
>data TikzEdge = TikzEdge {
>   edge_name :: String,
>   start_node_name :: String,
>   end_node_name :: String,
>   start_node_direction :: NodeDirection,
>   end_node_direction :: NodeDirection
> }
> 


>instance Show TexToken where
>   show (EnvToken e o c) =
>     "\\begin{" ++ e ++ "}[" ++ concatMap (++ ',') o ++ "]\n"
>     ++ concatMap (\x -> show x ++ "\n") c
>     ++ "\\end{" ++ e ++ "}\n"

>tikzEnv :: [String] -> TexToken -> TexToken
>tikzEnv opts cont = EnvToken "tikzpicture" opts cont

>graphToTex :: (Monad m) => InGraphM mon e m TexToken
>graphToTex = do
>  v <- gvertices
>  e <- glinks
>  let links = Set.map linkToTex e
>      vertices = Set.map vertexToTex e
>  return $ tikzEnv [] $ NodesToken $ [

>linkToTex :: 
