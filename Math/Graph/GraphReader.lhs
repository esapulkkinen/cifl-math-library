>{-# LANGUAGE Safe #-}
>module Math.Graph.GraphReader where
>import Math.Graph.GraphMonoid
>import qualified Data.Set as Set
>import Data.Set (Set)
>import Data.Tree
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad

>dfsM :: (Monad m, Ord e, GraphMonoid mon) => e -> InGraphM mon e m (Forest e)
>dfsM root = do edgeset <- edgesStartingFromM root
>               let edgelist = Set.toList edgeset
>               childnodes <- mapM targetM edgelist              
>               subtrees <- mapM dfsM childnodes
>               return $ zipWith Node childnodes subtrees
