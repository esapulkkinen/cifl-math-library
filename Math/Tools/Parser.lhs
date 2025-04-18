>{-# LANGUAGE Rank2Types, FlexibleInstances, OverloadedStrings #-}
>module Math.Tools.Parser where
>import Prelude hiding (null, take, span, length, take)
>import Text.PrettyPrint (Doc, (<+>), empty)
>import Data.Text
>import qualified Data.Text as Text
>import Control.Applicative
>import Control.Monad
>import Math.Tools.ParserInterface
>import Math.Tools.LineInfo
>import Math.Tools.PrettyP

>type Error = Doc

>newtype ParserM i a = ParserM {
>     runParseM :: forall c. (a -> i -> LineInfo -> c)
>                         -> (Error -> LineInfo -> c)
>                         -> i -> LineInfo -> c
>   }

>instance Functor (ParserM i) where
>   fmap f p = pure f <*> p

>instance Alternative (ParserM i) where
>   empty = mzero
>   (<|>) = mplus

>instance Applicative (ParserM i) where
>   pure x = ParserM (\f _ -> f x)
>   f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

>instance Monad (ParserM i) where
>   return = pure
>   (ParserM f) >>= g = ParserM (\res err -> 
>                        f (\res2 -> runParseM (g res2) res err) err)

>instance MonadFail (ParserM i) where
>   fail msg = ParserM (\_ err _ li -> err (pp msg) li)

>instance MonadPlus (ParserM i) where
>   mzero = ParserM (\_ err _ li -> err "Unsupported alternative" li)
>   (ParserM f) `mplus` (ParserM g) = 
>      ParserM (\s err i li -> f s (\_ _ -> g s err i li) i li)

>instance ParserCombinators (ParserM Text) where
>   readChar = readChar'
>   eof = eof'
>   newline = newline'
>   readWhile = readWhile'
>   oneOf = oneOf'
>   require = require'
>   optional = optional'
>   getRemainingInput = getRemainingInput'
>   getLineInfo = getLineInfo'
>   syntaxError = syntaxError'

>optional' :: ParserM i a -> ParserM i (Maybe a)
>optional' f = (f >>= (return . Just)) `mplus` return Nothing

>syntaxError' :: Doc -> ParserM i a
>syntaxError' d = ParserM $ \f err i li -> err d li

>readChar' :: ParserM Text Char
>readChar' = ParserM $ \f err i li -> case uncons i of
>                        Just (c,cr) -> f c cr (nextColumn li)
>                        Nothing -> err "Expected any character, found EOF" li

>oneOf' :: (Char -> Bool) -> ParserM Text Char
>oneOf' check = ParserM $ \f err lst li -> case uncons lst of
>   (Just (c,cr)) -> if check c then f c cr (nextColumn li)
>                               else err ("Bad character:" <> enclose c) li
>   Nothing -> err "Expected a character, found EOF" li

>eof' :: ParserM Text ()
>eof' = ParserM $ \f err i li -> case null i of
>                   True -> f () Text.empty li
>                   False -> err ("Expected EOF, found:" <+> bracketize (unpack $ take 10 i)) li

>newline' :: ParserM Text ()
>newline' = ParserM $ \f err i li -> case uncons i of
>                       Just ('\n',cr) -> f () cr (nextLine li)
>                       Just (_,_) -> err "Expected a newline" li
>                       Nothing -> err "Unexpected end of file" li

>readWhile' :: (Char -> Bool) -> ParserM Text Text
>readWhile' ch = ParserM $ \f err i li -> let (s,r) = span ch i
>                            in if null s then err ("unexpected character in input:" <+> bracketize (unpack $ take 10 r)) li
>                                         else f s r (addToColumn (length s) li)

>require' :: Char -> ParserM Text Char
>require' c = ParserM $ \f err i li -> case uncons i of
>                          Just (c',cr) -> if c' == c then f c' cr (nextColumn li)
>                                                    else err (unexpected c c') li
>                          Nothing -> err ("Unexpected EOF, expected" <+> enclose c) li
>    where unexpected c c' = "Unexpected character: expected"
>                                    <+> (enclose c <> pp ',')
>                                    <+> ("found:" <> enclose c')
>getRemainingInput' :: ParserM a a
>getRemainingInput' = ParserM $ \f _ i li -> f i i li

>getLineInfo' :: ParserM a LineInfo
>getLineInfo' = ParserM $ \f _ i li -> f li i li
