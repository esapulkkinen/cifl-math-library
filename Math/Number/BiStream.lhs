>{-# LANGUAGE TypeOperators #-}
>module Math.Number.BiStream where
>import Text.PrettyPrint (punctuate,hsep, (<+>), vcat)
>import qualified Text.PrettyPrint as Pretty
>import Math.Tools.PrettyP
>import Math.Tools.CoMonad
>import Data.List (intersperse, reverse)
>import Math.Matrix.Interface
>import Math.Number.Stream (Stream, naturals, nonzero_naturals, stream_diagonal)
>import qualified Math.Number.Stream as Stream

>-- | This data type is a bidirectional version of a stream.
>data BiStream a = BiStream { bifst :: Stream a, bisnd :: Stream a }

>instance PpShowF BiStream where
>   ppf (BiStream a b) = (pp "..." <> hsep (punctuate (pp ',') (reverse $ Stream.take 5 $ fmap pp a)))
>                       <+> pp ";" <+> (hsep (punctuate (pp ',') (Stream.take 5 $ fmap pp b)) <> pp "...") 

>instance PpShowVerticalF BiStream where
>   ppf_vertical (BiStream a b) = vcat ((reverse $ Stream.take 5 (fmap pp a)) ++ Stream.take 5 (fmap pp b))

>instance (PpShow a) => PpShow (BiStream a) where
>   pp (BiStream a b) = (pp "..." <> hsep (punctuate (pp ',') (reverse $ Stream.take 5 $ fmap pp a)))
>                          <+> pp ";" <+>
>                          (hsep (punctuate (pp ',') (Stream.take 5 $ fmap pp b))
>                          <> pp "...")

>instance (Show a) => Show (BiStream a) where
>   show (BiStream a b) = "..." ++ concat (intersperse "," $ reverse $ Stream.take 10 $ (fmap show a)) ++
>      ";" ++ (concat $ intersperse "," $ Stream.take 10 (fmap show b)) ++ "..."

>integers_bistream :: (Num a) => BiStream a
>integers_bistream = BiStream (fmap negate nonzero_naturals) naturals


>integer_pairs_matrix :: (Num a) => (BiStream :*: BiStream) (a,a)
>integer_pairs_matrix = matrix (,) integers_bistream integers_bistream
>   
>fromIntegerStream :: (Integer -> a) -> BiStream a
>fromIntegerStream f = fmap f integers_bistream

>bistream_index :: BiStream a -> Integer -> a
>bistream_index (BiStream x r) i | i < 0 = Stream.shead (Stream.drop (abs (succ i)) x)
>                                | otherwise = Stream.shead (Stream.drop i r)

>bistream_diagonal :: (BiStream :*: BiStream) a -> BiStream a
>bistream_diagonal (Matrix x) = BiStream (stream_diagonal nn) (stream_diagonal pp)
>   where nn = Matrix $ fmap bifst (bifst x)
>         pp = Matrix $ fmap bisnd (bisnd x)
>
>bistream_cross_diagonal :: (BiStream :*: BiStream) a -> BiStream a
>bistream_cross_diagonal (Matrix x) = BiStream (stream_diagonal np) (stream_diagonal pn) 
>  where pn = Matrix $ fmap bisnd (bifst x)
>        np = Matrix $ fmap bifst (bisnd x)

>instance Functor BiStream where
>   fmap f (BiStream a b) = BiStream (fmap f a) (fmap f b)

