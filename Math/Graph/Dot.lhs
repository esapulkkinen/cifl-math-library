>{-# LANGUAGE Trustworthy #-}
>module Math.Graph.Dot where
>import Data.Text.Lazy hiding (map)
>import Data.Set (Set)
>import qualified Data.Set as Set
>import Data.GraphViz.Types
>import Data.GraphViz.Types.Canonical
>import Data.GraphViz.Attributes
>import Data.GraphViz.Printing
>import Math.Graph.GraphMonoid
>import Math.Graph.InGraphMonad

>convertToDotGraph :: (Monad m, Ord n,Show n, GraphMonoid mon) => InGraphM mon n m (DotGraph n)
>convertToDotGraph = do
>   vertices <- verticesM
>   links    <- linksM
>   let nodes = map (\v -> DotNode v [shape MSquare]) $ Set.toList vertices
>   edges <- mapM (\(e,s,t) -> return $ DotEdge s t [toLabel (show e)])
>                 $ Set.toList links
>   let dotstmts = DotStmts { attrStmts = [],subGraphs = [],
>                             nodeStmts = nodes,edgeStmts = edges }
>   let dotgraph = DotGraph { strictGraph = False,directedGraph = True,
>                             graphID = Nothing,graphStatements = dotstmts }
>   return dotgraph

>renderDotToString :: (PrintDot n) => DotGraph n -> String
>renderDotToString dg = unpack $ renderDot $ unqtDot dg
