>{-# LANGUAGE PackageImports, ImplicitParams, Rank2Types #-}
>module Math.Graph.FileSystem where
>import Math.Graph.Reversible
>import Math.Graph.GraphMonoid
>import Math.Tools.Isomorphism
>import Math.Tools.Arrow
>import System.Directory
>import Control.Monad

>getCurrentDirectoryGraph :: IO (Graph Four FilePath)
>getCurrentDirectoryGraph = do
>   root <- getCurrentDirectory
>   root' <- canonicalizePath root
>   getDirectoryGraphOf root'

>getFileGraphOf :: FilePath -> IO (Graph Four FilePath)
>getFileGraphOf p = return $ vertexG p

>getDirectoryGraphOf :: FilePath -> IO (Graph Four FilePath)
>getDirectoryGraphOf root = do
>   contents <- getDirectoryContents root
>   putStrLn $ show contents
>   results <- flip mapM contents $ \p -> do
>      isDir <- doesDirectoryExist p
>      isFile <- doesFileExist p
>      if isDir then getDirectoryGraphOf p
>       else if isFile then getFileGraphOf p
>             else return emptyG
>   
>   foldM (\g n -> return $ flip runIso n $ mapG (((root ++ "/") ++) <-> deletePrefix root)) emptyG results

>deletePrefix :: FilePath -> FilePath -> FilePath
>deletePrefix lst@(c:cr) (d:dr) | c == d = deletePrefix cr dr
>                               | c == '/' = cr
>                               | otherwise = lst
>deletePrefix [] [] = []
>deletePrefix [] lst = lst

>currentDirectory :: (?currentDirectory :: FilePath) => IO FilePath
>currentDirectory = return ?currentDirectory

>inDirectory :: FilePath -> ((?currentDirectory :: FilePath) => IO a) -> IO a
>inDirectory path action = let ?currentDirectory = path in action
