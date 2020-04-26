>{-# LANGUAGE RecursiveDo, ScopedTypeVariables, OverloadedStrings, ImplicitParams #-}
>module GUI.XClient where
>import GUI.XProto
>import Control.Concurrent.Chan
>import Control.Exception
>import Control.Monad
>import Data.Binary
>import Data.ByteString
>import Network.Socket hiding (recv,sendAll)
>import Network.Socket.ByteString.Lazy (recv,sendAll)

>resolve :: String -> Int -> IO SockAddr
>resolve host port = do
>   let hints = defaultHints { addrSocketType = Stream }
>   addr:_ <- getAddrInfo (Just hints) (Just host) (Just "6000")
>   return $ addrAddress addr

>parseDpyString :: String -> IO AddrInfo
>parseDpyString dpy = do
>  let (lst1,lst2) = Prelude.span (== ':') dpy
>      host = Prelude.takeWhile ( /= ':') lst1
>      port :: PortNumber = read lst2
>      sockaddr = SockAddrUnix ("/tmp/.X11-unix/X" ++ show port)
>      inetaddr = SockAddrInet port
>  if host /= "" then do
>        inetsockaddr <- resolve host 6000
>        return $ AddrInfo [] AF_INET Stream defaultProtocol inetsockaddr Nothing
>   else return $ AddrInfo [] AF_UNIX Stream defaultProtocol sockaddr Nothing


>-- | <http://hackage.haskell.org/package/network-3.1.0.1/docs/Network-Socket.html>
>client_connect :: String -> (Socket -> IO ()) -> IO ()
>client_connect dpystring talk = withSocketsDo $ do
>  addr <- parseDpyString dpystring
>  bracket (open addr) closing talk
> where open addr = do
>             sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
>             connect sock $ addrAddress addr
>             return sock
>       closing sock = do
>             shutdown sock ShutdownBoth
>             close sock
>debug :: (?debug :: Bool) => Bool
>debug = ?debug
>xproto_connector :: (?debug :: Bool) => String 
> -> Chan XRequest
> -> Chan XRequestResponse
> -> IO ()
>xproto_connector dpystring reqmv respmv = do
>              client_connect dpystring $ \sock -> do
>                 status <- init sock (ConnectionSetup 11 0 [] [])
>                 Prelude.putStrLn (show status)
>                 loop sock
> where init :: (?debug :: Bool) => Socket -> XSetupRequest -> IO XSetupResponseSuccess
>       init sock req = do
>         when debug $ Prelude.putStrLn (show req)
>         let initbytes = encode req
>         sendAll sock initbytes
>         resbytes <- recv sock 10240
>         when debug $ Prelude.putStrLn (show resbytes)
>         let result = decode resbytes
>         when debug $ Prelude.putStrLn (show result)
>         return result
>       loop sock = mdo
>         msg <- readChan reqmv
>         when debug $ Prelude.putStrLn (show msg)
>         let msgbytes = encode msg
>         when debug $ Prelude.putStrLn (show msgbytes)
>         sendAll sock msgbytes
>         resbytes <- recv sock 10240
>         when debug $ Prelude.putStrLn (show resbytes)
>         let resp = decode_response (opcode msg) resbytes
>         when debug $ Prelude.putStrLn (show resp)
>         writeChan respmv resp
>         loop sock
>        `catch` \(e :: IOException) -> do
>           Prelude.putStrLn ("X protocol connector: " ++ show e)
