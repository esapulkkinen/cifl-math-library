>{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, ImplicitParams, Rank2Types, ViewPatterns, OverloadedLists, OverloadedStrings #-}
>module Math.Tools.Cmdline where
>import Data.Text
>import qualified Data.Text as Text
>import Control.Monad
>import Control.Monad.IO.Class
>import Control.Exception.Base
>import qualified Control.Applicative as Applicative
>import Data.Typeable
>import Data.Data
>import GHC.Generics
>import Data.Map (Map)
>import qualified Data.Map as Map
>import Math.Tools.Map hiding (empty)
>import Math.Tools.Exception
>import System.Environment

>data ParseExceptions = CannotParseException !Text
>                     | IncompleteParseException !Text !Text
>                     | AmbiguousParseException [(Text,Text)]
>  deriving (Show, Typeable, Data, Generic)


>instance Exception ParseExceptions

>data CmdlineParsingMT m a = CmdlineParsingMT {
>   runCmdlineParsingMT :: Map Text Text -> m a
>   }

>instance (Functor m) => Functor (CmdlineParsingMT m) where
>   fmap f (CmdlineParsingMT x) = CmdlineParsingMT $ fmap f . x

>instance (Monad m) => Monad (CmdlineParsingMT m) where
>   return x = CmdlineParsingMT $ const (return x)
>   (CmdlineParsingMT f) >>= g = CmdlineParsingMT $ \m -> f m >>= \a' -> runCmdlineParsingMT (g a') m

>instance (Applicative m) => Applicative (CmdlineParsingMT m) where
>   pure x = CmdlineParsingMT $ const (pure x)
>   (CmdlineParsingMT f) <*> (CmdlineParsingMT x) = CmdlineParsingMT $ \m -> f m <*> x m

>data LookupExceptions = NotFoundException !Text
>  deriving (Show,Typeable, Data, Generic)

>instance Exception LookupExceptions

>instance (MonadIO m) => MonadIO (CmdlineParsingMT m) where
>   liftIO io = CmdlineParsingMT $ const $ liftIO io

>instance (Applicative.Alternative m) => Applicative.Alternative (CmdlineParsingMT m) where
>   (CmdlineParsingMT f) <|> (CmdlineParsingMT g) = CmdlineParsingMT $ \m -> f m Applicative.<|> g m
>   empty = CmdlineParsingMT (const Applicative.empty)

>instance (MonadIO m) => ExceptionalMonad (CmdlineParsingMT m) where
>   throwM e = liftIO (throwIO e)

>hasOption :: (Monad m) => Text -> CmdlineParsingMT m Bool
>hasOption i = CmdlineParsingMT $ \m -> return (i `Map.member` m)

>lookupOption :: (ExceptionalMonad m, Read a, Show a) => Text -> CmdlineParsingMT m a
>lookupOption i = lookupOptionText i >>= readerParse

>lookupOptionText :: (ExceptionalMonad m) => Text -> CmdlineParsingMT m Text
>lookupOptionText i = CmdlineParsingMT $ \m -> maybe (throwM $ NotFoundException i)
>                                                return (Map.lookup i m)

>readOption :: (Read b, Show b) => Map Text Text -> Text -> IO b
>readOption opts i = lookupMapM opts i >>= readerParse

>readerParse :: (Show a,Read a, ExceptionalMonad m) => Text -> m a
>readerParse str = case reads $ unpack str of
>   [] -> throwM $ CannotParseException $ str
>   [(v,[])] -> return v
>   [(v,r)]  -> throwM $ IncompleteParseException str (pack r)
>   r -> throwM $ AmbiguousParseException $ fmap (\ (a,b) -> (pack (show a),pack b)) r

>data CommandLineException = MissingCommandLineArgument !Text
>                          | InvalidCommandLineArgument !Text
>   deriving (Show, Typeable, Data, Generic)

>instance Exception CommandLineException

>splitArgs :: (ExceptionalMonad m)
>          => [Text] -> Map Text Text -> m (Map Text Text)
>splitArgs ((stripPrefix "--" -> Just arg):val:re) m = splitArgs re m >>= (return . Map.insert arg val)
>splitArgs [(stripPrefix "--" -> Just arg)] m = throwM $ MissingCommandLineArgument $ arg
>splitArgs [] m = return m
>splitArgs (c:_) _ = throwM $ InvalidCommandLineArgument $ c

>parseCmdline :: IO (Map Text Text)
>parseCmdline = do
>   args <- getArgs
>   splitArgs (fmap pack args) Map.empty

>withOptions :: Map Text Text -> ((?cmdlineoptions :: Map Text Text) => IO a) -> IO a
>withOptions opts action = let ?cmdlineoptions = opts in action

>findOption :: (?cmdlineoptions :: Map Text Text, Read b, Show b) => Text -> IO b
>findOption = readOption ?cmdlineoptions
