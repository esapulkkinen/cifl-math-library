>{-# LANGUAGE DeriveFoldable, DeriveTraversable, TypeFamilies #-}
>module Math.Tools.Tree where
>import Control.Applicative
>import qualified Data.Foldable as F
>import Data.Traversable
>import Data.List
>import Control.Monad
>import Math.Tools.Visitor

>data Tree e n = Node { root_label :: n, children :: [(e,Tree e n)] }
>  deriving (F.Foldable, Traversable)

>type Path e = [e]
>type Forest n e = [Tree e n]

>tlookup :: (Show e, Eq e, Monad m) => Tree e n -> Path e -> m (Tree e n)
>tlookup z (c:cr) | Just e <- lookup c (children z) = tlookup e cr
>                 | otherwise = fail $ "cannot find:" ++ show c
>tlookup z [] = return z

>has_children :: Tree e n -> Bool
>has_children (Node _ lst) = null lst

>map_tree :: (e -> e') -> (n -> n') -> Tree e n -> Tree e' n'
>map_tree f g (Node n lst) = Node (g n) $ map mapper lst
>   where mapper (e,lst') = (f e, map_tree f g lst')

>instance Functor (Tree e) where { fmap f = map_tree id f }
>instance Visitor (Tree e n) where
>   data Fold (Tree e n) a = TreeFold (n -> [a] -> a) (e -> a -> a)
>   visit z@(TreeFold n e) (Node l lst) = n l visited_edges
>      where visited_edges = map (\ (edge,st) -> e edge (visit z st)) lst

>map_edges :: (e -> e') -> Tree e n -> Tree e' n
>map_edges f = map_tree f id

>instance (Show n, Show e) => Show (Tree e n) where
>  show (Node n lst) = show n ++ if null lst then "" else ":" ++ show_edgelist lst
>    where show_edge (e,t) = show e ++ "=" ++ show t
>          show_edgelist []  = ""
>          show_edgelist lst = "(" ++ (intercalate "," $ map show_edge lst) ++ ")"

>instance (Monoid e) => Applicative (Tree e) where
>   pure = leaf
>   (Node f lst) <*> (Node x lst') = Node (f x) $
>      liftA2 (\ (e,x) (e',x') -> (mappend e e', x <*> x')) lst lst'

>instance (Monoid n, Monoid e) => Monoid (Tree e n) where
>   mempty = Node mempty []
>   mappend (Node x lst) (Node y lst') = Node (mappend x y) $
>      liftA2 (\ (e,t) (e',t') -> (mappend e e', mappend t t')) lst lst'

>leaf :: n -> Tree e n
>leaf x = Node x []

>node :: n -> [(e, Tree e n)] -> Tree e n
>node = Node

>level :: Integer -> Forest n e -> [n]
>level 0 = roots
>level i = level (i-1) . next_level

>roots :: Forest n e -> [n]
>roots = map root_label

>next_level :: Forest n e -> Forest n e
>next_level = concatMap forest_below

>edges_below_roots :: Forest n e -> [e]
>edges_below_roots = concatMap edges_below

>edges_below  :: Tree e n -> [e]
>edges_below (Node _ lst) = map fst lst

>forest_below :: Tree e n -> Forest n e
>forest_below (Node _ lst) = map snd lst

>edge_count :: Tree e n -> Int
>edge_count = visit edgeCountFold

>node_count :: Tree e n -> Int
>node_count = visit nodeCountFold

>leaves :: Tree e n -> [n]
>leaves = visit leavesFold

>depth :: Tree e n -> Int
>depth = visit depthFold

>all_nodes :: Tree e n -> [n]
>all_nodes = visit nodesFold

>all_edges :: Tree e n -> [e]
>all_edges = visit edgesFold

>edgeCountFold :: Fold (Tree e n) Int
>edgeCountFold = TreeFold (const sum) (const succ)

>nodeCountFold :: Fold (Tree e n) Int
>nodeCountFold = TreeFold (const (succ . sum)) (const id)

>depthFold :: Fold (Tree e n) Int
>depthFold = TreeFold (const (foldr max 0)) (const succ)

>nodesFold :: Fold (Tree e n) [n]
>nodesFold = TreeFold (\n lst -> n : concat lst) (const id)

>edgesFold :: Fold (Tree e n) [e]
>edgesFold = TreeFold (const concat) (:)

>leavesFold :: Fold (Tree e n) [n]
>leavesFold = TreeFold (\n ch -> if null ch then [n] else concat ch) (const id)


>edges_starting_from :: Tree e n -> [e]
>edges_starting_from (Node _ lst) = map fst lst

>child_forest :: Tree e n -> Forest n e
>child_forest (Node _ lst) = map snd lst

>find_child :: (Monad m, Eq e, Show e) => e -> Tree e n -> m (Tree e n)
>find_child e (Node _ lst) | Just s <- lookup e lst = return s
>                          | otherwise = fail $ "child not found:" ++ show e

>find_subtree :: (Monad m, Show e, Eq e) => Path e -> Tree e n -> m (Tree e n)
>find_subtree = flip $ foldM (flip find_child)




