>{-# LANGUAGE Safe,FlexibleInstances #-}
>-- | The module contains only Show and PpShow instances for graphs.
>module Math.Graph.Show where
>import Data.Monoid hiding ((<>))
>import qualified Data.Set as Set
>import qualified Math.Tools.PrettyP as PrettyP
>import Math.Tools.PrettyP
>import Data.Monoid
>import Data.Set (Set)
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad
>import Math.Graph.TransformationMonoid
>import Math.Graph.GraphMonoid

>instance (Ord a, PpShow a) => Show (Graph Three a) where { show = pPrint }
>instance (Ord a, PpShow a) => Show (Graph Four a) where { show = pPrint }

>instance (Ord a, PpShow a) => PpShow (Graph Three a) where
>  pp g = maybe (pp "[]") (\ (v,e) -> pp "(" <> (PrettyP.pp_list v <+> pp ";" <+> PrettyP.pp_list e) <> pp ')') $ inGraphM g $ do
>                edges <- linksM
>                vertices <- verticesM
>                return $ (Set.toList vertices,(map (\ (e,x,y) -> pp e <+> pp '=' <+> pp x <+> pp "->" <+> pp y) $ Set.toList edges))

>instance (Ord a, PpShow a) => PpShow (Graph Four a) where
>  pp g = maybe (pp "[]") (\ (v,e) -> pp "(" <> (PrettyP.pp_list v <+> pp ";" <+> PrettyP.pp_list e) <> pp ')') $ inGraphM g $ do
>                edges <- reversibleLinksM
>                vertices <- verticesM
>                return $ (Set.toList vertices,(map (\ ((e,re),(x,y)) -> pp e <+> (if (e /= re) then (pp "/" <+> pp re) else PrettyP.empty) <+> pp '=' <+> pp x <+> pp "<->" <+> pp y) $ Set.toList edges))

