>{-# LANGUAGE Rank2Types, FlexibleInstances #-}
>module Math.Tools.Parser where
>import Text.PrettyPrint (Doc, (<+>))
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
>   pure x = return x
>   f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

>instance Monad (ParserM i) where
>   return x = ParserM (\f _ -> f x)
>   (ParserM f) >>= g = ParserM (\res err -> 
>                        f (\res2 -> runParseM (g res2) res err) err)
>   fail msg = ParserM (\_ err _ li -> err (pp msg) li)

>instance MonadPlus (ParserM i) where
>   mzero = ParserM (\_ err _ li -> err (pp "Unsupported alternative") li)
>   (ParserM f) `mplus` (ParserM g) = 
>      ParserM (\s err i li -> f s (\_ _ -> g s err i li) i li)

>instance ParserCombinators (ParserM String) where
>   readChar = readChar'
>   eof = eof'
>   newline = newline'
>   readWhile = readWhile'
>   one_of = one_of'
>   require = require'
>   optional = optional'
>   getRemainingInput = getRemainingInput'
>   getLineInfo = getLineInfo'
>   syntaxError = syntaxError'

>optional' :: ParserM i a -> ParserM i (Maybe a)
>optional' f = (f >>= (return . Just)) `mplus` return Nothing

>syntaxError' :: Doc -> ParserM i a
>syntaxError' d = ParserM $ \f err i li -> err d li

>readChar' :: ParserM [c] c
>readChar' = ParserM $ \f err i li -> case i of
>                        (c:cr) -> f c cr (next_column li)
>                        [] -> err (pp "Expected any character, found EOF") li

>one_of' :: (PpShow c) => (c -> Bool) -> ParserM [c] c
>one_of' check = ParserM $ \f err (c:cr) li -> if check c then f c cr (next_column li) else err (pp "Bad character:" <> enclose c) li

>eof' :: (PpShow c) => ParserM [c] ()
>eof' = ParserM $ \f err i li -> case i of
>                   [] -> f () [] li
>                   str -> err (pp "Expected EOF, found:" <+> bracketize (take 10 str)) li

>newline' :: ParserM String ()
>newline' = ParserM $ \f err i li -> case i of
>                       ('\n':cr) -> f () cr (next_line li)
>                       (_:_) -> err (pp "Expected a newline") li
>                       [] -> err (pp "Unexpected end of file") li

>readWhile' :: (PpShow c) => (c -> Bool) -> ParserM [c] [c]
>readWhile' ch = ParserM $ \f err i li -> let (s,r) = span ch i
>                            in if null s then err (pp "unexpected character in input:" <+> bracketize (take 10 r)) li
>                                         else f s r (add_to_column (length s) li)

>require' :: (PpShow c, Eq c) => c -> ParserM [c] c
>require' c = ParserM $ \f err i li -> case i of
>                          (c':cr) -> if c' == c then f c' cr (next_column li)
>                                                else err (pp "Unexpected character: expected" <+> (enclose c <> pp ',') <+> (pp "found:" <> enclose c')) li
>                          [] -> err (pp "Unexpected EOF, expected" <+> enclose c) li

>getRemainingInput' :: ParserM a a
>getRemainingInput' = ParserM $ \f _ i li -> f i i li

>getLineInfo' :: ParserM a LineInfo
>getLineInfo' = ParserM $ \f _ i li -> f li i li
