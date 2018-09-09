>{-# LANGUAGE Safe #-}
>module Math.Tools.Show where
>import Data.Char
>import Control.Monad
>import Text.ParserCombinators.ReadPrec
>import Text.Read
> 
>showsBinaryOp :: (Show a) => Int -> String -> Int -> a -> a -> ShowS
>showsBinaryOp i op p x y = showParen (p > i) $
>                             showsPrec (i+1) x . showString op . showsPrec (i+1) y

>punctuate_list :: (Show a) => String -> [a] -> ShowS
>punctuate_list _ [] = showString ""
>punctuate_list _ [c] = shows c
>punctuate_list s (c:r) = shows c . showString s . punctuate_list s r

>choose :: [(String,ReadPrec a)] -> ReadPrec a
>choose m = choice $ map (\(a,b) -> string a >> b) m

>string :: String -> ReadPrec ()
>string lst = mapM (\ch -> get >>= \v -> guard (v==ch)) lst >> return ()

>whitespace :: ReadPrec ()
>whitespace = do str <- look
>                let lst = takeWhile isSpace str
>                mapM (const get) lst
>                return ()

>optional_whitespace :: ReadPrec ()
>optional_whitespace = whitespace <++ return ()
