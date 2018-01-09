>{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}
>module Math.Tools.Reference where
>import Data.IORef
>import qualified Data.STRef as STRef
>import Control.Monad.ST
>import Control.Concurrent.MVar
>import Control.Concurrent.STM
>import Control.Concurrent.STM.TVar
>import Control.Concurrent.STM.TChan
>import Control.Concurrent.STM.TQueue

>-- | Idea for this interface came from Haskell OpenGL library
>-- from package Graphics.Rendering.OpenGL.GL.StateVar.
>-- <http://lambda.haskell.org/platform/doc/current/packages/OpenGL-2.8.0.0/doc/html/index.html>
>class (Monad m) => ReferenceMonad r m where
>   new  :: a -> m (r a)
>   ($=) :: r a -> a -> m ()
>   get  :: r a -> m a

>class (Monad m) => ReferenceComonad r m where
>   readRef :: r a -> m a
>   duplicateRef :: r a -> m (r (r a))
>   extendRef :: (r a -> m b) -> r a -> m (r b)
>   duplicateRef = extendRef return
>   
>extendRefDefault :: (Traversable r, ReferenceComonad r m)
>                 => (r a -> m b) -> r a -> m (r b)
>extendRefDefault f x = duplicateRef x >>= \rra -> mapM f rra

>data Ref m r a = (ReferenceMonad r m) => MkRef (r a)

>modify :: (ReferenceMonad r m) => r a -> (a -> a) -> m ()
>modify r f = get r >>= (\v -> r $= f v) 

>instance ReferenceMonad TVar STM where
>   new = newTVar
>   ($=) = writeTVar
>   get  = readTVar

>instance ReferenceMonad TVar IO where
>   new = newTVarIO
>   r $= x = atomically (writeTVar r x)
>   get = readTVarIO

>instance ReferenceMonad TMVar STM where
>   new  = newTMVar
>   ($=) = putTMVar
>   get  = takeTMVar

>instance ReferenceMonad TChan STM where
>   new x = newTChan >>= \ch -> writeTChan ch x >> return ch
>   ($=) = writeTChan
>   get = readTChan

>instance ReferenceMonad TQueue STM where
>   new x = newTQueue >>= \ch -> writeTQueue ch x >> return ch
>   ($=) = writeTQueue
>   get = readTQueue

>instance (ReferenceMonad r m) => ReferenceMonad (Ref m r) m where
>   new x = new x >>= (return . MkRef)
>   ($=) (MkRef r) x = r $= x
>   get (MkRef r) = get r

>instance ReferenceMonad IORef IO where
>   new = newIORef
>   ($=) = writeIORef
>   get  = readIORef

>instance ReferenceMonad (STRef.STRef s) (ST s) where
>   new = STRef.newSTRef
>   ($=) = STRef.writeSTRef
>   get  = STRef.readSTRef

>instance ReferenceMonad MVar IO where
>   new = newMVar
>   ($=) = putMVar
>   get  = takeMVar

