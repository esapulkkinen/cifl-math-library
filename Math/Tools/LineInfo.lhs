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

>data LineInfo = LineInfo { li_file :: !String,
>			    li_row  :: {-# UNPACK #-} !Int,
>			    li_column :: {-# UNPACK #-} !Int }
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
>   location_of :: e -> LineInfo

>instance Located LineInfo where
>   location_of = id

>instance Semigroup LineInfo where
>   (<>) = sum_lineinfo

>instance Monoid LineInfo where
>   mempty = emptyLineInfo
>   mappend = sum_lineinfo

>instance Show LineInfo where
>   show NoLineInfo = ""
>   show (LineInfo f r c) = show f ++ ":" ++ show r ++ ":" ++ show c

>instance PpShow LineInfo where
>         pp NoLineInfo = Pretty.empty
>	  pp (LineInfo f r c) = Pretty.fcat [pp f,pp ':', pp r, pp ':', pp c]

>emptyLineInfo :: LineInfo
>emptyLineInfo = NoLineInfo

>sum_lineinfo :: LineInfo -> LineInfo -> LineInfo
>sum_lineinfo NoLineInfo x = x
>sum_lineinfo x _ = x

>new_file :: String -> LineInfo -> LineInfo
>new_file f NoLineInfo = LineInfo { li_file = f, li_row = 1, li_column = 0 }
>new_file f li = li { li_file = f, li_row = 1, li_column = 0 }

>next_line :: LineInfo -> LineInfo
>next_line li@(LineInfo {}) = li { li_row = li_row li + 1, li_column = 0 }
>next_line NoLineInfo = NoLineInfo

>prev_line :: LineInfo -> LineInfo
>prev_line li@(LineInfo {}) = li { li_row = li_row li - 1, li_column = 0 }
>prev_line NoLineInfo = NoLineInfo

>nextlineIso :: Iso LineInfo (LineInfo,Int)
>nextlineIso = Iso (\li -> (next_line li, li_column li))
>                  (\ (li,c) -> li { li_row = li_row li - 1, li_column = c })

>add_to_line :: Int -> LineInfo -> LineInfo
>add_to_line i li@(LineInfo{}) = li { li_row = li_row li + i, li_column = 0 }
>add_to_line i NoLineInfo = NoLineInfo

>next_column :: LineInfo -> LineInfo
>next_column li@(LineInfo {}) = li { li_column = li_column li + 1 }
>next_column NoLineInfo = NoLineInfo

>prev_column :: LineInfo -> LineInfo
>prev_column li@(LineInfo {}) = li { li_column = li_column li - 1 }
>prev_column NoLineInfo = NoLineInfo

>nextColumnIso :: Iso LineInfo LineInfo
>nextColumnIso = Iso next_column prev_column

>add_to_column :: Int -> LineInfo -> LineInfo
>add_to_column x li@(LineInfo {}) = li { li_column = li_column li + x }
>add_to_column x NoLineInfo = NoLineInfo

>add_to_columnIso :: Int -> Iso LineInfo LineInfo
>add_to_columnIso i = Iso (add_to_column i) (add_to_column (negate i))
