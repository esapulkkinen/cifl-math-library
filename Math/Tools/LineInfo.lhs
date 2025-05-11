>{-# LANGUAGE Safe #-}
>{-# LANGUAGE DeriveDataTypeable #-}
>module Math.Tools.LineInfo where
>import Data.Monoid
>import Data.Typeable
>import Data.Binary
>import qualified Text.PrettyPrint as Pretty
>import qualified Math.Tools.PrettyP as PrettyP
>import Math.Tools.PrettyP
>import Math.Tools.Isomorphism

>data LineInfo = LineInfo { liFile :: !String,
>			    liRow  :: {-# UNPACK #-} !Int,
>			    liColumn :: {-# UNPACK #-} !Int }
>              | NoLineInfo
>   deriving (Eq,Ord,Typeable)

>instance Binary LineInfo where
>   put (LineInfo f r c) = put f >> put r >> put c
>   get = do { f <- get ; r <- get ; c <- get ; return (LineInfo f r c) }

>lineinfoIso :: Iso LineInfo (Either (String,Int,Int) ())
>lineinfoIso = Iso openli closeli
>   where openli (LineInfo f r c) = Left (f,r,c)
>         openli NoLineInfo = Right ()          
>         closeli (Left (f,r,c)) = LineInfo f r c
>         closeli (Right ()) = NoLineInfo

>class Located e where
>   locationOf :: e -> LineInfo

>instance Located LineInfo where
>   locationOf = id

>instance Semigroup LineInfo where
>   (<>) = sumLineinfo

>instance Monoid LineInfo where
>   mempty = emptyLineInfo
>   mappend = (<>)

>instance Show LineInfo where
>   show NoLineInfo = ""
>   show (LineInfo f r c) = show f ++ ":" ++ show r ++ ":" ++ show c

>instance PpShow LineInfo where
>         pp NoLineInfo = Pretty.empty
>	  pp (LineInfo f r c) = Pretty.fcat [pp f,pp ':', pp r, pp ':', pp c]

>emptyLineInfo :: LineInfo
>emptyLineInfo = NoLineInfo

>sumLineinfo :: LineInfo -> LineInfo -> LineInfo
>sumLineinfo NoLineInfo x = x
>sumLineinfo x _ = x

>newFile :: String -> LineInfo -> LineInfo
>newFile f NoLineInfo = LineInfo { liFile = f, liRow = 1, liColumn = 0 }
>newFile f li = li { liFile = f, liRow = 1, liColumn = 0 }

>nextLine :: LineInfo -> LineInfo
>nextLine li@(LineInfo {}) = li { liRow = liRow li + 1, liColumn = 0 }
>nextLine NoLineInfo = NoLineInfo

>prevLine :: LineInfo -> LineInfo
>prevLine li@(LineInfo {}) = li { liRow = liRow li - 1, liColumn = 0 }
>prevLine NoLineInfo = NoLineInfo

>nextlineIso :: Iso LineInfo (LineInfo,Int)
>nextlineIso = Iso (\li -> (nextLine li, liColumn li))
>                  (\ (li,c) -> li { liRow = liRow li - 1, liColumn = c })

>addToLine :: Int -> LineInfo -> LineInfo
>addToLine i li@(LineInfo{}) = li { liRow = liRow li + i, liColumn = 0 }
>addToLine i NoLineInfo = NoLineInfo

>nextColumn :: LineInfo -> LineInfo
>nextColumn li@(LineInfo {}) = li { liColumn = liColumn li + 1 }
>nextColumn NoLineInfo = NoLineInfo

>prevColumn :: LineInfo -> LineInfo
>prevColumn li@(LineInfo {}) = li { liColumn = liColumn li - 1 }
>prevColumn NoLineInfo = NoLineInfo

>nextColumnIso :: Iso LineInfo LineInfo
>nextColumnIso = Iso nextColumn prevColumn

>addToColumn :: Int -> LineInfo -> LineInfo
>addToColumn x li@(LineInfo {}) = li { liColumn = liColumn li + x }
>addToColumn x NoLineInfo = NoLineInfo

>addToColumnIso :: Int -> Iso LineInfo LineInfo
>addToColumnIso i = Iso (addToColumn i) (addToColumn (negate i))
