>{-# LANGUAGE Safe #-}
>{-# LANGUAGE OverloadedStrings #-}
>{-# LANGUAGE DeriveDataTypeable #-}
>module Math.Tools.Id where
>import Text.PrettyPrint (fcat, (<+>))
>import qualified Text.PrettyPrint as Pretty
>import Data.Typeable
>import Data.Data
>import Data.Monoid
>import Math.Tools.PrettyP
>import Math.Tools.LineInfo
>import Math.Tools.Universe

>data Id = MkId { idName :: String, idInfo :: !LineInfo }
>   deriving (Typeable)

>mkId :: String -> LineInfo -> Id
>mkId = MkId

>newId :: String -> Id
>newId x = mkId x emptyLineInfo

>allIds :: [Id]
>allIds = do
>  i <- ([0..] :: [Integer])
>  return $ newId ("v_" ++ show i)
 
>instance Universe Id where
>   allElements = allIds

>instance Located Id where
>   locationOf (MkId _ li) = li

>instance Show Id where
>   showsPrec _ z = showString (idName z) {- . showChar '@' . shows (idInfo z) -}

>instance PpShow Id where
>	  pp (MkId n _) = pp n

>instance Eq Id where
>	  (MkId n _) == (MkId n' _) = n == n'

>instance Ord Id where
>	  (MkId x _) <= (MkId y _) = x <= y

>lineDesc :: Id -> Pretty.Doc
>lineDesc i = "Line" <+> (pp (locationOf i) <> ":")

>lineOf :: Id -> Int
>lineOf = liRow . idInfo

>{- imported from Tools.LineInfo
>data LineInfo = NewLineInfo { line :: !Int, lineStr :: Doc }

>instance PpShow LineInfo where
>	  pp x = pp "at line" <+> pp (line x) <> pp ':' $$
>		 lineStr x

>instance Ord LineInfo where
>	  x <= y = line x <= line y

>instance Show LineInfo where
>	  showsPrec _ x = shows (line x)

>instance Eq LineInfo where
>	  (NewLineInfo l1 _) == (NewLineInfo l2 _) = l1 == l2

>emptyLineInfo :: LineInfo
>emptyLineInfo = NewLineInfo 1 empty
>-}
