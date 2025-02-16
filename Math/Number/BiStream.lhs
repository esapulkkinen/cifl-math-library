>{-# LANGUAGE TypeOperators, OverloadedStrings #-}
>module Math.Number.BiStream where
>import Text.PrettyPrint (punctuate,hsep, (<+>), vcat)
>import qualified Text.PrettyPrint as Pretty
>import Math.Tools.PrettyP
>import Math.Tools.CoMonad
>import Data.List (intersperse, reverse)
>import Math.Matrix.Interface
>import Math.Number.Stream (naturals, nonzeroNaturals, streamDiagonal)
>import qualified Math.Number.Stream as Stream
>import Math.Number.StreamInterface

>-- | This data type is a bidirectional version of a stream.
>data BiStream a = BiStream { bifst :: Stream a, bisnd :: Stream a }

>instance PpShowF BiStream where
>   ppf (BiStream a b) = ("..." <> hsep (punctuate (pp ',') (reverse $ Stream.take 5 $ fmap pp a)))
>                       <+> ";" <+> (hsep (punctuate (pp ',') (Stream.take 5 $ fmap pp b)) <> "...") 

>instance PpShowVerticalF BiStream where
>   ppfVertical (BiStream a b) = vcat ((reverse $ Stream.take 5 (fmap pp a)) ++ Stream.take 5 (fmap pp b))

>instance (PpShow a) => PpShow (BiStream a) where
>   pp (BiStream a b) = ("..." <> hsep (punctuate (pp ',') (reverse $ Stream.take 5 $ fmap pp a)))
>                          <+> ";" <+>
>                          (hsep (punctuate (pp ',') (Stream.take 5 $ fmap pp b))
>                          <> "...")

>instance (Show a) => Show (BiStream a) where
>   show (BiStream a b) = "..." ++ concat (intersperse "," $ reverse $ Stream.take 10 $ (fmap show a)) ++
>      ";" ++ (concat $ intersperse "," $ Stream.take 10 (fmap show b)) ++ "..."

>integersBistream :: (Num a) => BiStream a
>integersBistream = BiStream (fmap negate nonzeroNaturals) naturals


>integerPairsMatrix :: (Num a) => (BiStream :*: BiStream) (a,a)
>integerPairsMatrix = matrix (,) integersBistream integersBistream
>   
>fromIntegerStream :: (Integer -> a) -> BiStream a
>fromIntegerStream f = fmap f integersBistream

>bistreamIndex :: BiStream a -> Integer -> a
>bistreamIndex (BiStream x r) i | i < 0 = shead (Stream.drop (abs (succ i)) x)
>                                | otherwise = shead (Stream.drop i r)

>bistreamDiagonal :: (BiStream :*: BiStream) a -> BiStream a
>bistreamDiagonal (Matrix x) = BiStream (streamDiagonalImpl nn) (streamDiagonalImpl pp)
>   where nn = Matrix $ fmap bifst (bifst x)
>         pp = Matrix $ fmap bisnd (bisnd x)
>
>bistreamCrossDiagonal :: (BiStream :*: BiStream) a -> BiStream a
>bistreamCrossDiagonal (Matrix x) = BiStream (streamDiagonalImpl np) (streamDiagonalImpl pn) 
>  where pn = Matrix $ fmap bisnd (bifst x)
>        np = Matrix $ fmap bifst (bisnd x)

>instance Functor BiStream where
>   fmap f (BiStream a b) = BiStream (fmap f a) (fmap f b)

