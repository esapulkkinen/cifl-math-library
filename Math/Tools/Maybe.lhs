>module Math.Tools.Maybe where

>guardM :: (Monad m) => Bool -> a -> m a
>guardM cond a = if cond then return a else fail "does not satisfy condition"