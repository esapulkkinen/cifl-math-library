>{-# LANGUAGE Safe, TypeSynonymInstances, FlexibleInstances #-}
>module Math.Tools.PrettyP (
>   PpShow(..), LeveledPpShow(..), enclose,
>   parenthesize, render, bracketize, verticalize, pPrint, pp_list, render132,
>   alternativitize, PpShowF(..), PpShowVerticalF(..)
> -- | module extends Text.PrettyPrint module from GHC library.
> -- Text.PrettyPrint is (c) 2001 The University of Glasgow under BSD license.
> ) where
>
> import Text.PrettyPrint as Pretty hiding (render, (<>))
> import GHC.Stack
> import Data.Monoid 
> import Data.Set (Set)
> import Data.Map (Map)
> import Data.Word
> import Data.Ratio
> import Data.Complex
> import qualified Data.Set as Set
> import qualified Data.Map as Map

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
> render = Pretty.fullRender PageMode 80 1.1 produceOutput ""
 
> render132 :: Doc -> String
> render132 = Pretty.fullRender PageMode 132 1.1 produceOutput ""

> produceOutput :: TextDetails -> String -> String
> produceOutput (Chr c) s = c:s
> produceOutput (Str n) s = n ++ s 
> produceOutput (PStr n) s = n ++ s
 
> enclose :: PpShow a => a -> Doc
> enclose x = pp "'" <> pp x <> pp "'"
 
> parenthesize :: PpShow a => [a] -> Doc
> parenthesize lst = parens $ nest 4 $ fcat $ punctuate (pp ',') (map pp lst)
 
> bracketize :: PpShow a => [a] -> Doc
> bracketize lst = brackets $ nest 4 $ fsep $ punctuate (pp ';') (map pp lst)
 
> alternativitize :: PpShow a => [a] -> Doc
> alternativitize lst = brackets $ nest 4 $ fsep $ punctuate (pp '|') (map pp lst)
 
> verticalize :: PpShow a => [a] -> Doc
> verticalize lst = vcat $ map (nest 4 . pp) lst
 
> class (PpShowF f) => PpShowVerticalF f where
>   ppf_vertical :: (PpShow a) => f a -> Doc
  
 
> instance PpShowVerticalF [] where
>   ppf_vertical = verticalize
 
> pp_list :: (PpShow a) => [a] -> Doc
> pp_list x = brackets $ nest 8 (fsep (punctuate (pp ',') (map pp x)))
  
 
> instance PpShowF [] where
>	  ppf = pp_list

instance PpShow a => Show a where
	  showsPrec _ x = showString $ render (pp x)

> class PpShow a => LeveledPpShow a where
>      exprLevel :: a -> Int
>      exprLevel x = 1
>      pp_level :: a -> Int -> Doc
>      pp_level x lim | exprLevel x < lim  = parens (pp x)
>                     | otherwise          = pp x


instance PpShow a => PpShow [a] where
	  pp x = pp '[' <> fsep (punctuate (pp ',') (map pp x)) <> pp ']'

> instance PpShow IOError where
>	  pp x = pp (show x)

> instance PpShow Bool where
>	  pp True = pp "true"
>	  pp False = pp "false"

> instance PpShow Word8 where
>    pp c = pp (show c)

> instance PpShow () where
>	  pp _ = pp "()"

> instance PpShow Doc where
>	  pp x = x

> instance PpShow String where
>	  pp = ptext

> instance PpShow Char where
>	  pp = char

> instance PpShow Int where
>	  pp = int



Will cause looping!

instance PpShow (f (Rec f)) => PpShow (Rec f) where
	  pp (In x) = pp x


> instance PpShow Integer where
>	  pp = integer
    
> instance (PpShow a, Num a, Ord a) => PpShow (Complex a) where
>  pp (x :+ y) = pp x <> (if y >= 0 then pp '+' else pp '-') <> pp (abs y) <> pp 'i'

> instance PpShowF Complex where
>  ppf (x :+ y) = pp x <+> pp ":+" <+> pp y


> instance (Integral a,PpShow a) => PpShow (Ratio a) where
>  pp r = pp (numerator r) <> pp '/' <> pp (denominator r)

> instance PpShow Float where
>   	  pp = float

> instance PpShow Double where
>	  pp = double

> instance (PpShow a, PpShow b) => PpShow (a,b) where
>	  pp (x,y) = parens $ fsep [pp x <> pp ',', nest 4 $ pp y]
> instance (PpShow a, PpShow b, PpShow c) => PpShow (a,b,c) where
>	  pp (x,y,z) = parens $ fsep [pp x <> pp ',', nest 4 $ pp y <> pp ',', nest 4 $ pp z]
> instance (PpShow a, PpShow b, PpShow c,PpShow d) => PpShow (a,b,c,d) where
>	  pp (x,y,z,z') = parens $ fsep [pp x <> pp ',', nest 4 $ pp y <> pp ',',
>					  nest 4 $ pp z <> pp ',', nest 4 $ pp z']

> instance PpShow a => PpShow (Maybe a) where
>	  pp Nothing = pp "Nothing"
>	  pp (Just x) = pp x

> instance (PpShow a, PpShow b) => PpShow (Either a b) where
>	  pp (Left x) = pp "Left" <> parens (pp x)
>	  pp (Right x) = pp "Right" <> parens (pp x)

> instance (PpShow a) => PpShow (Set a) where
>   pp = bracketize . Set.toList

> instance (PpShow i, PpShow e) => PpShow (Map i e) where
>   pp = parenthesize . Map.assocs

> instance PpShow [Integer] where
>   pp lst = pp_list lst

> instance (PpShow a) => PpShow [(Integer,a)] where
>   pp lst = pp_list lst

instance PpShow [(Integer,Integer)] where
   pp [] = pp "[]"
   pp lst = pp '[' <> ppf lst <> pp ']'


> instance PpShow CallStack where
>   pp s = pp (show s)
