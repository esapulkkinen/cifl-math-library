>{-# LANGUAGE UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
>module Math.Tools.IdentifierMonad where
>import Prelude hiding (catch)
>import Control.Exception
>import Control.Monad.Reader
>import Control.Monad.Writer
>import Control.Monad.Except

>data IdentifierM m a = IdentifierM { 
>     runIdentifierM :: Integer -> m (a,Integer)
>   }

>instance (Monad m) => Functor (IdentifierM m) where
>   fmap f (IdentifierM g) = IdentifierM $ \i -> g i >>= \ (a,j) -> return (f a,j)

>instance (Monad m) => Applicative (IdentifierM m) where
>   pure x = IdentifierM $ \i -> return (x,i)
>   (IdentifierM f) <*> (IdentifierM x) = IdentifierM $ \i -> x i >>= \ (xv,j) -> f j >>= \ (fv,k) -> return (fv xv,k)

>instance Semigroup Integer where
>  (<>) = (+)

>instance Monoid Integer where
>  mempty = 0
>  mappend = (+)

>instance (MonadError IOError m) => MonadReader Integer (IdentifierM m) where
>   ask = IdentifierM (\i -> return (i,i))
>   local f (IdentifierM g) = IdentifierM $ g . f

>instance (MonadError IOError m) => MonadWriter Integer (IdentifierM m) where
>   tell v = IdentifierM (\_ -> return ((),v))
>   listen (IdentifierM f) = IdentifierM $ \v -> do (v',i) <- f v
>                                                   return ((v',i),i)
>   pass (IdentifierM f) = IdentifierM $ \i -> do ((r,g),j) <- f i
>                                                 return (r, g j)

>instance MonadTrans IdentifierM where
>   lift m = IdentifierM (\i -> m >>= \v -> return (v,i))


>catchIM :: (Exception e) => IdentifierM IO a -> (e -> IdentifierM IO a) -> IdentifierM IO a
>catchIM (IdentifierM f) h = IdentifierM (\i -> f i 
>                                                `catch` \e -> runIdentifierM (h e) i)
>                                                  

>instance (MonadError IOError m) => MonadError IOError (IdentifierM m) where
>   throwError x = IdentifierM (\_ -> throwError x)
>   catchError (IdentifierM f) g = IdentifierM (\i -> do
>                 f i `catchError` (\e -> runIdentifierM (g e) i))

let IdentifierM g' = g e in g')

>instance (MonadError IOError m) => Monad (IdentifierM m) where
>   return x = IdentifierM (\ i -> return (x,i))
>   (IdentifierM x) >>= f = IdentifierM (\i -> do (x',v) <- x i
>                                                 let IdentifierM g = f x'
>                                                 g v)
>   fail msg = throwError (userError msg)

>incIdentifier :: (Monad m) => IdentifierM m Integer
>incIdentifier = IdentifierM (\i -> return (i,succ i))

