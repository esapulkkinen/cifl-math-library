>{-# LANGUAGE TypeFamilies, LambdaCase #-}
>module Math.Tools.LeafTree where
>import qualified Prelude as P
>import Prelude hiding (zipWith)
>import Math.Tools.Tree
>import Math.Tools.Visitor
>import Math.Tools.Adjunction
>import Control.Applicative
>import Data.List hiding (zipWith)
>import Math.Tools.Orthogonal
>import Data.Monoid
>import Data.Binary

>data LeafTree a = Leaf { leaf :: a } 
>                | SubTree { subtrees :: [LeafTree a] }

>instance (Binary a) => Binary (LeafTree a) where
>   put (Leaf x) = put True >> put x
>   put (SubTree lst) = put False >> put lst
>   get = do s <- get
>            if s then get >>= (return . Leaf)
>                 else get >>= (return . SubTree)

>instance Applicative LeafTree where
>  pure x = Leaf x
>  (Leaf f) <*> (Leaf x) = Leaf (f x)
>  (SubTree lst) <*> z = SubTree (map (<*> z) lst)
>  z <*> (SubTree lst) = SubTree (map (z <*>) lst)

>instance Alternative LeafTree where
>  empty = SubTree empty
>  (SubTree lst) <|> (SubTree lst') = SubTree $ lst <|> lst'
>  z@(Leaf _) <|> _ = z
>  _ <|> z@(Leaf _) = z

>zipWith :: (a -> b -> c) -> LeafTree a -> LeafTree b -> LeafTree c
>zipWith f (SubTree lst) (SubTree lst') = SubTree $ P.zipWith (zipWith f) lst lst'
>zipWith f z@(Leaf _) (SubTree lst)  = SubTree $ map (\i -> zipWith f z i) lst
>zipWith f (SubTree lst) z@(Leaf _)     = SubTree $ map (\i -> zipWith f i z) lst 
>zipWith f (Leaf x) (Leaf y)       = Leaf $ f x y

>instance (Semigroup a) => Semigroup (LeafTree a) where
>   (<>) = zipWith (<>)

>instance (Monoid a) => Monoid (LeafTree a) where
>  mempty  = SubTree empty
>  mappend = zipWith mappend

>instance (Show a) => Show (LeafTree a) where
>   show (Leaf x) = show x
>   show (SubTree lst) = "[" ++ concat (intersperse "," (map show lst)) ++ "]"

>instance Monad LeafTree where
>  return = Leaf
>  (Leaf x) >>= f = f x
>  (SubTree lst) >>= f = SubTree (map (>>= f) lst)
>  fail msg = Leaf (error msg)

>in_subtree :: Int -> LeafTree a -> LeafTree a
>in_subtree 0 (Leaf x) = Leaf x
>in_subtree i (SubTree lst) | i <= length lst = lst !! i
>                           | otherwise = fail "path does not exist"
>in_subtree _ _ = fail "path does not exist"

>isLeaf :: LeafTree a -> Bool
>isLeaf (Leaf _) = True
>isLeaf (SubTree _) = False

>instance Functor LeafTree where
>  fmap f = \case
>    (Leaf x) -> Leaf (f x)
>    (SubTree lst) -> SubTree $ map (fmap f) lst

>instance Visitor (LeafTree e) where
>  data Fold (LeafTree e) a = LeafTreeFold (e -> a) ([a] -> a)
>  visit (LeafTreeFold lf _) (Leaf n) = lf n
>  visit z@(LeafTreeFold _ st) (SubTree lst) = st (map (visit z) lst)

>leavesTreeFold :: Fold (Tree e n) (LeafTree n)
>leavesTreeFold = TreeFold (\n ch -> if null ch then Leaf n else SubTree ch) (const id)
