>module Math.Tools.Maybe where

>guardM :: (Monad m) => Bool -> a -> m a
>guardM cond a = if cond then return a else fail "does not satisfy condition"

>runMaybe :: (Monad m) => Maybe a -> m a
>runMaybe = maybe (fail "Computation resulted in Nothing") return

