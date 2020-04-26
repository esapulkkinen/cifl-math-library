>module Math.Tools.Assert where
>import GHC.Stack
> 
>assert :: HasCallStack => Bool -> a -> a
>assert cond a = if cond then a else error "assertion failed"

>require :: HasCallStack => String -> Bool -> a -> a
>require msg cond a = if cond then a else error ("assertion failed: " ++ msg)

>assertM :: (HasCallStack, Monad m) => Bool -> m a -> m a
>assertM cond m = if cond then m else fail $ "assertion failed: \n" ++ prettyCallStack callStack

>requireM :: (HasCallStack, Monad m) => String -> Bool -> m a -> m a
>requireM msg cond m = if cond then m else fail $ "assertion failed: "
>                           ++ msg ++ "\n" ++ prettyCallStack callStack

>preconditionM :: (HasCallStack, Monad m) => Bool -> m a -> m a
>preconditionM cond m = withFrozenCallStack (requireM "precondition failed" cond m)
