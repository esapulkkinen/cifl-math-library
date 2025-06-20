>{-# LANGUAGE Trustworthy, TypeSynonymInstances, FlexibleInstances #-}
>{-# LANGUAGE OverloadedStrings #-}
>module Math.Tools.PrettyP (
>   PpShow(..), LeveledPpShow(..), enclose,
>   parenthesize, render, bracketize, verticalize, pPrint, ppList, render132,
>   alternativitize, PpShowF(..), PpShowVerticalF(..)
> -- | module extends Text.PrettyPrint module from GHC library.
> -- Text.PrettyPrint is (c) 2001 The University of Glasgow under BSD license.
> ) where
>
> import qualified Text.PrettyPrint as Pretty
> import qualified GHC.Stack as Stack
> import Data.Monoid 
> import Data.Set (Set)
> import Data.Map (Map)
> import Data.Word
> import Data.Ratio
> import Data.String
> import Data.Complex
> import qualified Data.Text as Text
> import qualified Data.Set as Set
> import qualified Data.Map as Map
> import Math.Tools.Universe

> type Doc = Pretty.Doc

> class PpShow a where
>      pp :: a -> Doc

> class PpShowF f where
>   ppf :: (PpShow a) => f a -> Doc
  
Some things that are useful to remember from Pretty:

brackets :: Doc -> Doc
parens   :: Doc -> Doc
braces   :: Doc -> Doc
punctuate :: Doc -> [Doc] -> [Doc]
hsep :: [Doc] -> Doc     list version of <+>
vcat :: [Doc] -> Doc     list version of $$
cat  :: [Doc] -> Doc     either hcat or vcat
sep  :: [Doc] -> Doc     either hsep or vcat
fcat :: [Doc] -> Doc     paragraph fill version of cat
fsep :: [Doc] -> Doc     paragraph fill version of sep
nest :: Int -> Doc -> Doc
hang :: Doc -> Int -> Doc -> Doc

> pPrint :: PpShow a => a -> String -- useful in ghci
> pPrint x = render (pp x)

> render :: Doc -> String
> render = Pretty.fullRender Pretty.PageMode 80 1.1 produceOutput ""
 
> render132 :: Doc -> String
> render132 = Pretty.fullRender Pretty.PageMode 132 1.1 produceOutput ""

> produceOutput :: Pretty.TextDetails -> String -> String
> produceOutput (Pretty.Chr c) s = c:s
> produceOutput (Pretty.Str n) s = n ++ s 
> produceOutput (Pretty.PStr n) s = n ++ s
 
> enclose :: PpShow a => a -> Doc
> enclose x = "'" <> pp x <> "'"
 
> parenthesize :: PpShow a => [a] -> Doc
> parenthesize lst = Pretty.parens $ Pretty.nest 4 $ Pretty.fcat $ Pretty.punctuate (pp ',') (map pp lst)


> bracketize :: PpShow a => [a] -> Doc
> bracketize lst = Pretty.brackets $ Pretty.nest 4 $ Pretty.fsep $ Pretty.punctuate (pp ';') (map pp lst)
 
> alternativitize :: PpShow a => [a] -> Doc
> alternativitize lst = Pretty.brackets $ Pretty.nest 4 $ Pretty.fsep $ Pretty.punctuate (pp '|') (map pp lst)
 
> verticalize :: PpShow a => [a] -> Doc
> verticalize lst = Pretty.vcat $ map (Pretty.nest 4 . pp) lst
 
> class (PpShowF f) => PpShowVerticalF f where
>   ppfVertical :: (PpShow a) => f a -> Doc

> instance PpShowVerticalF [] where
>   ppfVertical = verticalize

> instance PpShowVerticalF Complex where
>   ppfVertical (x :+ y) = pp x Pretty.$$ ":+" Pretty.<+> pp y

> ppList :: (PpShow a) => [a] -> Doc
> ppList x = Pretty.brackets $ Pretty.nest 8 (Pretty.fsep (Pretty.punctuate (pp ',') (map pp x)))
  
 
> instance PpShowF [] where
>	  ppf = ppList

instance PpShow a => Show a where
	  showsPrec _ x = showString $ render (pp x)

> class PpShow a => LeveledPpShow a where
>      exprLevel :: a -> Int
>      exprLevel x = 1
>      pp_level :: a -> Int -> Doc
>      pp_level x lim | exprLevel x < lim  = Pretty.parens (pp x)
>                     | otherwise          = pp x


instance PpShow a => PpShow [a] where
	  pp x = pp '[' <> fsep (punctuate (pp ',') (map pp x)) <> pp ']'

> instance PpShow IOError where
>	  pp x = pp (show x)

> instance PpShow Bool where
>	  pp True = "true"
>	  pp False = "false"

> instance PpShow Word8 where
>    pp c = pp (show c)

> instance PpShow () where
>	  pp _ = "()"

> instance PpShow Doc where
>	  pp x = x

> instance PpShow String where
>	  pp = Pretty.ptext
>
> instance PpShow Text.Text where
>         pp = Pretty.ptext . Text.unpack

> instance PpShow Char where
>	  pp = Pretty.char

> instance PpShow Int where
>	  pp = Pretty.int



Will cause looping!

instance PpShow (f (Rec f)) => PpShow (Rec f) where
	  pp (In x) = pp x


> instance PpShow Integer where
>	  pp = Pretty.integer
    
> instance (PpShow a, Num a, Ord a) => PpShow (Complex a) where
>  pp (x :+ y) = pp x <> (if y >= 0 then pp '+' else pp '-') <> pp (abs y) <> pp 'i'

> instance PpShowF Complex where
>  ppf (x :+ y) = pp x Pretty.<+> ":+" Pretty.<+> pp y


> instance (Integral a,PpShow a) => PpShow (Ratio a) where
>  pp r = pp (numerator r) <> pp '/' <> pp (denominator r)

> instance PpShow Float where
>   	  pp = Pretty.float

> instance PpShow Double where
>	  pp = Pretty.double

> instance (PpShow a, PpShow b) => PpShow (a,b) where
>	  pp (x,y) = Pretty.parens $ Pretty.fsep [pp x <> pp ',', Pretty.nest 4 $ pp y]
> instance (PpShow a, PpShow b, PpShow c) => PpShow (a,b,c) where
>	  pp (x,y,z) = Pretty.parens $ Pretty.fsep [pp x <> pp ',', Pretty.nest 4 $ pp y <> pp ',', Pretty.nest 4 $ pp z]
> instance (PpShow a, PpShow b, PpShow c,PpShow d) => PpShow (a,b,c,d) where
>	  pp (x,y,z,z') = Pretty.parens $ Pretty.fsep [pp x <> pp ',', Pretty.nest 4 $ pp y <> pp ',',
>					  Pretty.nest 4 $ pp z <> pp ',', Pretty.nest 4 $ pp z']

> instance PpShow a => PpShow (Maybe a) where
>	  pp Nothing = "Nothing"
>	  pp (Just x) = pp x

> instance (PpShow a, PpShow b) => PpShow (Either a b) where
>	  pp (Left x) = "Left" <> Pretty.parens (pp x)
>	  pp (Right x) = "Right" <> Pretty.parens (pp x)

> instance (PpShow a) => PpShow (Set a) where
>   pp = bracketize . Set.toList

> instance (PpShow i, PpShow e) => PpShow (Map i e) where
>   pp = parenthesize . Map.assocs

> instance PpShow [Integer] where
>   pp lst = ppList lst

> instance (PpShow a) => PpShow [(Integer,a)] where
>   pp lst = ppList lst

instance PpShow [(Integer,Integer)] where
   pp [] = pp "[]"
   pp lst = pp '[' <> ppf lst <> pp ']'


> instance PpShow Stack.CallStack where
>   pp s = pp (show s)

> instance (Universe a) => PpShowVerticalF ((->) a) where
>   ppfVertical f = Pretty.vcat $ (take 10 $ lst) ++ [if null (drop 10 lst) then Pretty.empty else pp (".." :: String)]
>     where lst = [pp (f j) | j <- allElements]

> instance (Universe a) => PpShowF ((->) a) where
>   ppf f = Pretty.hsep (take 10 lst) <> if null lst2 then Pretty.empty else pp (".." :: String)
>     where lst = [pp (f i) | i <- allElements]
>           lst2 = drop 10 lst
