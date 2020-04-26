>{-# LANGUAGE Safe #-}
>module Math.Tools.Exception where
>import Control.Exception.Base
>import Control.Monad.IO.Class

>class (MonadIO m) => ExceptionalMonad m where
>   throwM :: (Exception e) => e -> m a

>instance ExceptionalMonad IO where
>   throwM = throwIO


