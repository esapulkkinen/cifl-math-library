>{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
>module Math.Graph.Labeled where
>import Control.Monad.Trans.Class
>import Control.Monad.Trans.Reader
>import qualified Data.Map as Map
>import Data.Map (Map)
>import Math.Tools.Arrow
>import Math.Tools.Isomorphism
>import Math.Graph.Interface
>import Math.Graph.GraphMonoid
>import Math.Graph.Reversible
>import Math.Graph.InGraphMonad

>data LGraph lbl mon a = LGraph { lgraph_basegraph :: Graph mon a,
>                                 lgraph_labels :: a :==: lbl }
>
>labelsFromMaps :: (Ord a, Ord lbl, Monad m) => Map lbl a -> Map a lbl
>              -> Graph mon a -> m (LGraph lbl mon a)
>labelsFromMaps m n g = orderedMapIso n m >>= (return . LGraph g)

>labelOf :: LGraph lbl m a -> a -> lbl
>labelOf = runIso . lgraph_labels
>
>elementByLabel :: LGraph lbl m a -> lbl -> a
>elementByLabel = runIsoInverse . lgraph_labels
>
>data InLGraphM mon lbl e m a = InLGraphM {
>   runInLGraphM :: ReaderT (LGraph lbl mon e) m a }
>
>
>liftReaderLGraph :: (Monad m) => InLGraphM mon lbl e m a -> ReaderT (LGraph lbl mon e) m a
>liftReaderLGraph m = ask >>= \g -> lift (inLabeledGraphM g m)
>
>instance (Monad m, Ord e, GraphMonoid mon) => GraphMonad (ReaderT (LGraph lbl mon e) m) e where
>   gisVertex = liftReaderLGraph . gisVertex
>   gsource = liftReaderLGraph . gsource
>   gtarget = liftReaderLGraph . gtarget
>   gelements = liftReaderLGraph gelements
>   gvertices = liftReaderLGraph gvertices
>   gedges = liftReaderLGraph gedges
>   gedgesStartingFrom = liftReaderLGraph . gedgesStartingFrom
>   gedgesEndingTo = liftReaderLGraph . gedgesEndingTo
>   glinks = liftReaderLGraph glinks
>
>instance (Monad m, Ord e, ReversibleGraphMonoid mon) => ReversibleGraphMonad (ReaderT (LGraph lbl mon e) m) e where
>   ginverse = liftReaderLGraph . ginverse
>   greversibleLinks = liftReaderLGraph greversibleLinks

>instance (Monad m, Ord e, GraphMonoid mon) => LabeledGraphMonad (ReaderT (LGraph lbl mon e) m) lbl e where
>   glabelOf e = ask >>= (return . (`labelOf` e))
>   gfind lbl = ask >>= (return . (`elementByLabel` lbl))


>instance (Monad m, Ord e, GraphMonoid mon) => LabeledGraphMonad (InLGraphM mon lbl e m) lbl e where
>   glabelOf e = InLGraphM $ glabelOf e
>   gfind lbl = InLGraphM $ gfind lbl

>inLabeledGraphM :: LGraph lbl mon e -> InLGraphM mon lbl e m a -> m a
>inLabeledGraphM g m = (runInLGraphM m) `runReaderT` g

>liftLabelM :: (Monad m) => InGraphM mon e m a -> InLGraphM mon lbl e m a
>liftLabelM igm = InLGraphM $ do
>   lgraph <- ask
>   lift (runReaderT (runInGraphM igm) (lgraph_basegraph lgraph))

>instance (Functor m) => Functor (InLGraphM mon lbl e m) where
>   fmap f (InLGraphM x) = InLGraphM $ fmap f x

>instance (Applicative m) => Applicative (InLGraphM mon lbl e m) where
>   pure = InLGraphM . pure
>   (InLGraphM f) <*> (InLGraphM x) = InLGraphM (f <*> x)

>instance (Monad m) => Monad (InLGraphM mon lbl e m) where
>   return = InLGraphM . return
>   (InLGraphM f) >>= g = InLGraphM (f >>= (runInLGraphM . g))

>instance MonadTrans (InLGraphM mon lbl e) where
>   lift = InLGraphM . lift
>
>
>instance (Ord e, Monad m, GraphMonoid mon) => GraphMonad (InLGraphM mon lbl e m) e where
>   gisVertex  = liftLabelM . gisVertex 
>   gsource = liftLabelM . gsource 
>   gtarget = liftLabelM . gtarget 
>   gelements = liftLabelM gelements
>   gvertices = liftLabelM gvertices
>   gedges = liftLabelM gedges
>   gedgesStartingFrom = liftLabelM . gedgesStartingFrom
>   gedgesEndingTo = liftLabelM . gedgesEndingTo
>   glinks = liftLabelM glinks
>
>instance (Ord e, Monad m, ReversibleGraphMonoid mon) => ReversibleGraphMonad (InLGraphM mon lbl e m) e where
>   ginverse = liftLabelM . ginverse 
>   greversibleLinks = liftLabelM greversibleLinks
