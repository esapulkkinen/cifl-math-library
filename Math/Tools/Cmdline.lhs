>module Math.Tools.Cmdline where
>import Control.Monad
>import Control.Monad.IO.Class
>import Control.Exception.Base
>import Control.Applicative
>import Data.Typeable
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Math.Tools.Map
>import Math.Tools.Exception
>import System.Environment

>data ParseExceptions = CannotParseException String
>                     | IncompleteParseException String String
>                     | AmbiguousParseException [(String,String)]
>  deriving (Show,Typeable)


>instance Exception ParseExceptions

>data CmdlineParsingMT m a = CmdlineParsingMT {
>   runCmdlineParsingMT :: Map String String -> m a
>   }

>instance (Functor m) => Functor (CmdlineParsingMT m) where
>   fmap f (CmdlineParsingMT x) = CmdlineParsingMT $ fmap f . x

>instance (Monad m) => Monad (CmdlineParsingMT m) where
>   return x = CmdlineParsingMT $ const (return x)
>   (CmdlineParsingMT f) >>= g = CmdlineParsingMT $ \m -> f m >>= \a' -> runCmdlineParsingMT (g a') m

>instance (Applicative m) => Applicative (CmdlineParsingMT m) where
>   pure x = CmdlineParsingMT $ const (pure x)
>   (CmdlineParsingMT f) <*> (CmdlineParsingMT x) = CmdlineParsingMT $ \m -> f m <*> x m

>data LookupExceptions = NotFoundException String
>  deriving (Show,Typeable)

>instance Exception LookupExceptions

>instance (MonadIO m) => MonadIO (CmdlineParsingMT m) where
>   liftIO io = CmdlineParsingMT $ const $ liftIO io

>instance (Alternative m) => Alternative (CmdlineParsingMT m) where
>   (CmdlineParsingMT f) <|> (CmdlineParsingMT g) = CmdlineParsingMT $ \m -> f m <|> g m
>   empty = CmdlineParsingMT (const empty)

>instance (MonadIO m) => ExceptionalMonad (CmdlineParsingMT m) where
>   throwM e = liftIO (throwIO e)

>hasOption :: (Monad m) => String -> CmdlineParsingMT m Bool
>hasOption i = CmdlineParsingMT $ \m -> return (i `Map.member` m)

>lookupOption :: (ExceptionalMonad m, Read a, Show a) => String -> CmdlineParsingMT m a
>lookupOption i = lookupOptionString i >>= readerParse

>lookupOptionString :: (ExceptionalMonad m) => String -> CmdlineParsingMT m String
>lookupOptionString i = CmdlineParsingMT $ \m -> maybe (throwM $ NotFoundException i)
>                                                return (Map.lookup i m)

>readOption :: (Read b, Show b) => Map String String -> String -> IO b
>readOption opts i = lookupMapM opts i >>= readerParse

>readerParse :: (Show a,Read a, ExceptionalMonad m) => String -> m a
>readerParse str = case reads str of
>   [] -> throwM $ CannotParseException $ str
>   [(v,[])] -> return v
>   [(v,r)]  -> throwM $ IncompleteParseException str r
>   r -> throwM $ AmbiguousParseException $ map (\ (a,b) -> (show a,b)) r

>data CommandLineException = MissingCommandLineArgument String
>                          | InvalidCommandLineArgument String
>   deriving (Show, Typeable)

>instance Exception CommandLineException

>splitArgs :: (ExceptionalMonad m)
>          => [String] -> Map String String -> m (Map String String)
>splitArgs (('-':'-':arg):val:re) m = splitArgs re m >>= (return . Map.insert arg val)
>splitArgs ['-':'-':arg] m = throwM $ MissingCommandLineArgument $ arg
>splitArgs [] m = return m
>splitArgs (c:_) _ = throwM $ InvalidCommandLineArgument $ c

>parseCmdline :: IO (Map String String)
>parseCmdline = do
>   args <- getArgs
>   splitArgs args Map.empty
