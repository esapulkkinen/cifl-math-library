>{-# LANGUAGE DeriveTraversable, TypeFamilies, PatternSynonyms #-}
>module Math.Tools.Tree where
>import Control.Arrow
>import Control.Applicative
>import qualified Data.Foldable as F
>import Data.Traversable
>import Data.List
>import Data.Binary
>import Control.Monad
>import Math.Tools.Visitor
>import qualified Text.PrettyPrint as Pretty
>import Math.Tools.PrettyP
>import Math.Matrix.Interface

>data Tree e n = Node { root_label :: n, children :: [(e,Tree e n)] }
>  deriving (F.Foldable, Traversable)

>type Path e = [e]
>type Forest n e = [Tree e n]

>pattern LeafNode :: n -> Tree e n
>pattern LeafNode n = Node n []
>pattern Edge :: n -> e -> Tree e n -> Tree e n
>pattern Edge n e t = Node n [(e,t)]
>pattern BinNode :: n -> Tree Bool n -> Tree Bool n -> Tree Bool n
>pattern BinNode n c1 c2 = Node n [(False,c1), (True, c2)]

>joinLists ::  (Ord e) => (a -> a -> a) -> [(e,a)] -> [(e,a)] -> [(e,a)]
>joinLists combine z@((e,t):r) z2@((f,t'):r')
>  | e == f = (e,combine t t') : joinLists combine r r'
>  | e < f  = (e,t) : joinLists combine r z2
>  | otherwise = (f,t') : joinLists combine z r'
>joinLists _ [] lst = lst
>joinLists _ lst [] = lst

>instance (VectorSpace n, Ord e) => VectorSpace (Tree e n) where
>  type Scalar (Tree e n) = Scalar n
>  vzero = leaf vzero
>  vnegate (Node n lst) = Node (vnegate n) $ map (second vnegate) lst
>  (Node n lst) %+ (Node n' lst') = Node (n %+ n') $ joinLists (%+) lst lst'
>  k %* (Node n lst) = Node (k %* n) [(x, k %* t) | (x,t) <- lst]

>instance (Num n, Ord e, Semigroup e) => Num (Tree e n) where
>   (Node n lst) + (Node m lst') = Node (n + m) $ joinLists (+) lst lst'
>   (Node n lst) - (Node m lst') = Node (n - m) $ joinLists (-) lst lst'
>   (Node n lst) * (Node m lst') = Node (n * m) $
>      [(e1 <> e2, t1*t2) | (e1,t1) <- lst, (e2,t2) <- lst' ]
>   negate (Node n lst) = Node (negate n) $ map (second (arr negate)) lst
>   abs (Node n lst) = Node (abs n) $ map (second (arr abs)) lst
>   signum (Node n lst) = Node (signum n) $ map (second (arr signum)) lst
>   fromInteger i = Node (fromInteger i) []

>instance (Binary e, Binary n) => Binary (Tree e n) where
>   put (Node x lst) = put x >> put lst
>   get = do { x <- get ; Node x <$> get }


>tlookup :: (Show e, Eq e, MonadFail m) => Tree e n -> Path e -> m (Tree e n)
>tlookup z (c:cr) | Just e <- lookup c (children z) = tlookup e cr
>                 | otherwise = fail $ "cannot find:" ++ show c
>tlookup z [] = return z

>hasChildren :: Tree e n -> Bool
>hasChildren (Node _ lst) = not (null lst)

>mapTree :: (e -> e') -> (n -> n') -> Tree e n -> Tree e' n'
>mapTree f g (Node n lst) = Node (g n) $ map mapper lst
>   where mapper (e,lst') = (f e, mapTree f g lst')

>instance Functor (Tree e) where { fmap = mapTree id }
>instance Visitor (Tree e n) where
>   data Fold (Tree e n) a = TreeFold (n -> [a] -> a) (e -> a -> a)
>   visit z@(TreeFold n e) (Node l lst) = n l visited_edges
>      where visited_edges = map (\ (edge,st) -> e edge (visit z st)) lst

>instance Builder (Tree e n) where
>   data Unfold (Tree e n) a = TreeUnfold (a -> (n,[(e,a)]))
>   build z@(TreeUnfold m) x = let (n,r) = m x in Node n $
>           map (second (build z)) r

>mapEdges :: (e -> e') -> Tree e n -> Tree e' n
>mapEdges f = mapTree f id

>instance (PpShow n, PpShow e) => PpShow (Tree e n) where
>  pp (Node n lst) = pp n <> if null lst then Pretty.empty else pp ':' <> ppList (map pp_edge lst)
>    where pp_edge (e,t) = pp e <> pp '=' <> pp t

>instance (Show n, Show e) => Show (Tree e n) where
>  show (Node n lst) = show n ++ if null lst then "" else ":" ++ show_edgelist lst
>    where show_edge (e,t) = show e ++ "=" ++ show t
>          show_edgelist []  = ""
>          show_edgelist lst = "(" ++ intercalate "," (map show_edge lst) ++ ")"

>instance (Monoid e) => Applicative (Tree e) where
>   pure = leaf
>   (Node f lst) <*> (Node x lst') = Node (f x) $
>      liftA2 (\ (e,xx) (e',x') -> (mappend e e', xx <*> x')) lst lst'

>instance (Semigroup n, Semigroup e) => Semigroup (Tree e n) where
>   (Node x lst) <> (Node y lst') = Node (x <> y) $
>      liftA2 (\ (e,t) (e',t') -> (e <> e', t <> t')) lst lst'

>instance (Monoid n, Monoid e) => Monoid (Tree e n) where
>   mempty = Node mempty []
>   mappend = (<>)

 (Node x lst) (Node y lst') = Node (mappend x y) $
      liftA2 (\ (e,t) (e',t') -> (mappend e e', mappend t t')) lst lst'

>leaf :: n -> Tree e n
>leaf = LeafNode

>singleton :: n -> (e,n) -> Tree e n
>singleton x (p,y) = Node x [(p, leaf y)]

>-- | 
>-- The result tree contains as many levels as the length of list.
>-- each level has just one node.
>depthTree :: n -> [(e,n)] -> Tree e n
>depthTree r ((p,x):xr) = Node r [(p, depthTree x xr)]
>depthTree r [] = Node r []

>treeNode :: n -> [(e, Tree e n)] -> Tree e n
>treeNode = Node

>treeLevel :: Integer -> Forest n e -> [n]
>treeLevel 0 = roots
>treeLevel i = treeLevel (i-1) . nextLevel

>roots :: Forest n e -> [n]
>roots = map root_label

>treeSumLevels :: (Num n) => Tree e n -> [n]
>treeSumLevels = treeFoldLevels Prelude.sum

>forestSumLevels :: (Num n) => Forest n e -> [n]
>forestSumLevels = forestFoldLevels Prelude.sum

>treeLevels :: (Monoid n) => Tree e n -> [n]
>treeLevels = treeFoldLevels mconcat

>treeFoldLevels :: ([n] -> n) -> Tree e n -> [n]
>treeFoldLevels f t = forestFoldLevels f [t]

>forestFoldLevels :: ([n] -> n) -> Forest n e -> [n]
>forestFoldLevels _ [] = []
>forestFoldLevels f lst = (f $ roots lst)
> : forestFoldLevels f (nextLevel lst)

>nextLevel :: Forest n e -> Forest n e
>nextLevel = concatMap forestBelow

>edgesBelowRoots :: Forest n e -> [e]
>edgesBelowRoots = concatMap edgesBelow

>edgesBelow  :: Tree e n -> [e]
>edgesBelow (Node _ lst) = map fst lst

>forestBelow :: Tree e n -> Forest n e
>forestBelow (Node _ lst) = map snd lst

>edgeCount :: Tree e n -> Int
>edgeCount = visit edgeCountFold

>nodeCount :: Tree e n -> Int
>nodeCount = visit nodeCountFold

>treeLeaves :: Tree e n -> [n]
>treeLeaves = visit leavesFold

>treeDepth :: Tree e n -> Int
>treeDepth = visit depthFold

>treeNodes :: Tree e n -> [n]
>treeNodes = visit nodesFold

>treeEdges :: Tree e n -> [e]
>treeEdges = visit edgesFold
> 
>treePaths :: (Monoid e) => Tree e n -> [e]
>treePaths = visit pathsFold

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

>pathsFold :: (Monoid e) => Fold (Tree e n) [e]
>pathsFold = TreeFold (\_ lst -> mempty : concat lst)
>                     (\e lst -> map (e <>) lst)

>leavesFold :: Fold (Tree e n) [n]
>leavesFold = TreeFold (\n ch -> if null ch then [n] else concat ch) (const id)


>edgesStartingFrom :: Tree e n -> [e]
>edgesStartingFrom (Node _ lst) = map fst lst

>childForest :: Tree e n -> Forest n e
>childForest (Node _ lst) = map snd lst

>findChild :: (MonadFail m, Eq e, Show e) => e -> Tree e n -> m (Tree e n)
>findChild e (Node _ lst) | Just s <- lookup e lst = return s
>                          | otherwise = fail $ "child not found:" ++ show e

>treeFindSubtree :: (MonadFail m, Show e, Eq e) => Path e -> Tree e n -> m (Tree e n)
>treeFindSubtree = flip $ foldM (flip findChild)




