>{-# LANGUAGE Safe, MultiParamTypeClasses #-}
>module Math.Graph.Interface where
>import Control.Arrow
>import qualified Data.Set as Set
>import Data.Set (Set)

>class (GraphMonad m e) => LabeledGraphMonad m lbl e where
>   glabelOf :: e -> m lbl
>   gfind :: lbl -> m e

>class (Arrow arr) => GraphArrow arr e where
>  gisVertexA :: arr e Bool
>  gsourceA   :: arr e e
>  gtargetA   :: arr e e
>  gelementsA :: arr () (Set e)
>  gverticesA :: arr () (Set e)
>  gedgesA    :: arr () (Set e)
>  gedgesStartingFromA :: arr e (Set e)
>  gedgesEndingToA :: arr e (Set e)
>  glinksA    :: arr () (Set (e,e,e))

>class (GraphArrow arr e) => ReversibleGraphArrow arr e where
>  ginverseA  :: arr e e
>  greversibleLinksA :: arr () (Set ((e,e),(e,e)))

>class (Monad m) => GraphMonad m e where
>  gisVertex :: e -> m Bool
>  gsource   :: e -> m e
>  gtarget   :: e -> m e
>  gelements :: m (Set e)
>  gvertices :: m (Set e)
>  gedges    :: m (Set e)
>  gedgesStartingFrom :: e -> m (Set e)
>  gedgesEndingTo     :: e -> m (Set e)
>  glinks :: m (Set (e,e,e))

>class (GraphMonad m e) => ReversibleGraphMonad m e where
>  ginverse :: e -> m e
>  greversibleLinks :: m (Set ((e,e),(e,e)))

