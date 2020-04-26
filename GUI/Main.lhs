>{-# LANGUAGE PackageImports, ImplicitParams, OverloadedStrings #-}
>module Main where
>import GUI.XClient
>import GUI.XProto
>import Control.Concurrent
>import qualified Control.Concurrent.Chan as Chan
>import "network" Network.Socket
>import Math.Tools.Cmdline
>import Math.Tools.Maybe
>import Data.Map
>import Control.Exception
>import Data.Text

>errorHandler :: Either SomeException a -> IO a
>errorHandler (Left e) = fail $ "Error: " ++ show e
>errorHandler (Right v) = return v

>rootWinId = 1

>initialize :: Chan XRequest -> Chan XRequestResponse -> IO ()
>initialize reqmv respmv = do
>   writeChan reqmv (XCreateWindow rootWinId 0 8 0 0 0 800 600 1 0 [])
>   XRRCreateWindow <- readChan respmv
>   writeChan reqmv (XMapWindow rootWinId)
>   XRRMapWindow <- readChan respmv
>   return ()

>handleEvent :: XRequestResponse -> Chan XRequest -> IO ()
>handleEvent resp reqmv = do
>   putStrLn (show resp)
>   return ()

>mainloop reqmv respmv = do
>   resp <- readChan respmv
>   handleEvent resp reqmv
>   mainloop reqmv respmv

>main = withSocketsDo $ do
>  args <- parseCmdline
>  putStrLn $ show args
>  dpy <- runMaybe (Data.Map.lookup "display" args)
>  reqmv <- newChan
>  respmv <- newChan
>  let debug_option = maybe False (== "true") $ Data.Map.lookup "debug" args
>  tid <- forkFinally (let ?debug = debug_option in xproto_connector (unpack dpy) reqmv respmv) errorHandler
>  initialize reqmv respmv
>  mainloop reqmv respmv
