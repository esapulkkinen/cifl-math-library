>{-# LANGUAGE Safe,FlexibleInstances, ScopedTypeVariables #-}
>-- | The module contains only Show and PpShow instances for graphs.
>module Math.Graph.Show where
>import Text.PrettyPrint (empty, (<+>), punctuate)
>import Data.Monoid hiding ((<>))
>import qualified Data.Set as Set
>import qualified Math.Tools.PrettyP as PrettyP
>import Math.Tools.PrettyP
>import Data.Monoid
>import Data.Set (Set)
>import Data.Binary
>import Control.Monad.Trans.Class
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad
>import Math.Graph.TransformationMonoid
>import Math.Graph.GraphMonoid

>instance (Ord a, PpShow a) => Show (Graph Three a) where { show = pPrint }
>instance (Ord a, PpShow a) => Show (Graph Four a) where { show = pPrint }

>instance (Binary a, Ord a) => Binary (Graph Three a) where
>  put g = inGraphM g $ do
>    vertices <- verticesM
>    edges <- linksM
>    lift $ put vertices
>    lift $ put edges
>  get = do
>    (vertices :: Set a) <- get
>    (edges :: Set (a,a,a)) <- get
>    return $ edgesG $ map (\ (a,b,c) -> (a,(b,c))) $ Set.toList edges

>instance (Ord a, PpShow a) => PpShow (Graph Three a) where
>  pp g = maybe (pp "[]") (\ (v,e) -> pp "(" <> (PrettyP.pp_list v <+> pp ";" <+> PrettyP.pp_list e) <> pp ')') $ inGraphM g $ do
>                edges <- linksM
>                vertices <- verticesM
>                return $ (Set.toList vertices,(map (\ (e,x,y) -> pp e <+> pp '=' <+> pp x <+> pp "->" <+> pp y) $ Set.toList edges))

>instance (Ord a, PpShow a) => PpShow (Graph Four a) where
>  pp g = maybe (pp "[]") (\ (v,e) -> pp "(" <> (PrettyP.pp_list v <+> pp ";" <+> PrettyP.pp_list e) <> pp ')') $ inGraphM g $ do
>                edges <- reversibleLinksM
>                vertices <- verticesM
>                return $ (Set.toList vertices,(map (\ ((e,re),(x,y)) -> pp e <+> (if (e /= re) then (pp "/" <+> pp re) else empty) <+> pp '=' <+> pp x <+> pp "<->" <+> pp y) $ Set.toList edges))

