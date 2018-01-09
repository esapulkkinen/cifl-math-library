>{-# LANGUAGE GADTs,TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
>module Math.Tools.ParseMonad where
>import Debug.Trace
>import Math.Tools.PrettyP
>import Control.Applicative
>import Control.Monad.Except
>import Control.Monad.Reader
>import Control.Monad.Writer hiding ((<>))
>import Math.Tools.LineInfo
>import Math.Tools.Visitor
>import Math.Tools.ParserInterface

>data ParseResult a = OkParse String LineInfo a
>		    | FailParse LineInfo Doc
>                   | SyntaxError LineInfo Doc

>parseResultToMonad :: (Monad m) => ParseResult a -> m a
>parseResultToMonad (OkParse _ _ a) = return a
>parseResultToMonad (FailParse li d) = fail $ render $ pp li <> pp ':' <> d
>parseResultToMonad (SyntaxError li d) = fail $ render $ pp li <> pp ':' <> d

>instance Functor ParseResult where
>  fmap f (OkParse s li x) = OkParse s li (f x)
>  fmap _ (FailParse li d) = FailParse li d
>  fmap _ (SyntaxError li d) = SyntaxError li d

>instance Visitor (ParseResult a) where
>   data Fold (ParseResult a) b = ParseResultFold (String -> LineInfo -> a -> b)
>                                                 (LineInfo -> Doc -> b)
>                                                 (LineInfo -> Doc -> b)
>   visit (ParseResultFold f _ _) (OkParse s li x) = f s li x
>   visit (ParseResultFold _ g _) (FailParse li d) = g li d
>   visit (ParseResultFold _ _ h) (SyntaxError li d) = h li d


>syntaxError' :: Doc -> ParseM a
>syntaxError' d = ParseM (\ _ li -> SyntaxError li d)

>data ParseM a where
>     ParseM :: (String -> LineInfo -> ParseResult a) -> ParseM a


>instance Functor ParseM where
>  fmap f (ParseM g) = ParseM (\s li -> fmap f (g s li))

>instance MonadReader (String,LineInfo) ParseM where
>     ask = ParseM $ \str li -> OkParse str li (str,li)
>     local f (ParseM g) = ParseM $ \str li -> let (str',li') = f (str,li)
>                                               in g str' li'

>instance MonadWriter (String,LineInfo) ParseM where
>    tell (s,li) = ParseM $ \ _ _ -> OkParse s li ()
>    listen (ParseM f) = ParseM $ \ s li -> case f s li of
>                                 (OkParse s' li' v') -> OkParse s' li' (v',(s',li'))
>                                 (FailParse li1 d1) -> FailParse li1 d1
>                                 (SyntaxError li2 d2) -> SyntaxError li2 d2
>                                              
>    pass (ParseM f) = ParseM $ \s li -> case f s li of
>                         (OkParse s' li' (x,f')) -> let
>                               (s'',li'') = f' (s',li')
>                             in OkParse s'' li'' x
>                         (FailParse li1 d1) -> FailParse li1 d1
>                         (SyntaxError li2 d2) -> SyntaxError li2 d2

>debug :: String -> ParseM a -> ParseM a
>debug str ~(ParseM expr) = ParseM ( \lst li -> trace (str ++ ", at:" ++ take 10 lst)
>                                           (expr lst li))

>instance ParserCombinators ParseM where
>  readChar = readChar'
>  eof = eof'
>  newline = newline'
>  readWhile = readWhile'
>  one_of = one_of'
>  optional = optional'
>  require = require'
>  getRemainingInput = getRemainingInput'
>  getLineInfo = getLineInfo'
>  syntaxError = syntaxError'

>readChar' :: ParseM Char
>readChar' = ParseM (\ lst li -> case lst of
>                       (c:cr) -> OkParse cr (next_column li) c
>                       [] -> FailParse li (pp "Unexpected end of file"))

>optional' :: ParseM a -> ParseM (Maybe a)
>optional' f = (f >>= (return . Just)) 
>             `mplus` return Nothing

>readWhile' :: (Char -> Bool) -> ParseM String
>readWhile' f = ParseM (\lst li -> let (s,r) = span f lst
>                  in if null s then FailParse li (err r)
>                               else OkParse r (add_to_column (length s) li) s)
>     where err r = pp "unexpected character in input:" 
>                   <> if null r then pp "<eof>" else pp_list [head r]


readWhile :: (Char -> Bool) -> (String -> ParseM a) -> ParseM a
readWhile f cont = ParseM (\ lst li -> let (s,r) = span f lst
                                           ~(ParseM x) = cont s
                                        in case s of
                                          [] -> FailParse li (err r) 
                                          _ -> x r (add_to_column (length s) li))

>eof' :: ParseM ()
>eof' = ParseM (\ lst li -> case lst of
>                          [] -> OkParse lst li ()
>                          _ -> FailParse li (pp "expected eof, found:'"
>                                            <> (pp $ take 10 lst)))

>one_of' :: (Char -> Bool) -> ParseM Char
>one_of' p = ParseM (\ lst li -> case lst of
>                               (c:cr) | p c -> OkParse cr (next_column li) c
>                                      | otherwise -> FailParse li (pp "one_of: character '" <> pp c <> pp "' is not one of the expected characters")
>                               [] -> FailParse li (pp "one_of: Unexpected end of file"))

one_of :: (Char -> Bool) -> (Char -> ParseM a) -> ParseM a
one_of p f = ParseM (\ lst li -> case lst of
                        (c:cr) | p c -> let ~(ParseM x) = f c 
                                         in x cr (next_column li)
                               | otherwise -> 
                        [] -> FailParse li (pp "one_of: Unexpected end of file"))

>require' :: Char -> ParseM Char
>require' c = ParseM (\ lst li -> case lst of
>                      (c':cr) | c == c' -> OkParse cr (next_column li) c
>                              | otherwise -> FailParse li (expected c c')
>                      [] -> FailParse li (pp "require: Unexpected end of file"))
>   where expected cx c' = (pp "expected '" <> pp cx <> pp "'")
>                      <+> (pp "found: '" <> pp c' <> pp "'")


>newline' :: ParseM ()
>newline' = ParseM (\ lst li -> case lst of
>                     ('\n':cr) -> OkParse cr (next_line li) ()
>                     (c:_) -> FailParse li (pp "not a newline:" <> pp "'" <> pp c <> pp "'")
>                     [] -> FailParse li (pp "Unexpected end of file"))

>runParseM :: ParseM a -> String -> LineInfo -> ParseResult a
>runParseM ~(ParseM x) = x

>getRemainingInput' :: ParseM String
>getRemainingInput' = ParseM (\s li -> OkParse s li s)

>getLineInfo' :: ParseM LineInfo
>getLineInfo' = ParseM (\s li -> OkParse s li li)

>instance (Show x) => Show (ParseResult x) where
>   show (OkParse _ _ x) = show x
>   show (FailParse li d) = show li ++ ":" ++ show d
>   show (SyntaxError li d) = "Syntax error:" ++ show li ++ ":" ++ show d

>instance (PpShow x) => PpShow (ParseResult x) where
>   pp (OkParse _ _ x) = pp x
>   pp (FailParse li d) = fcat[pp li,pp ':',d]
>   pp (SyntaxError li d) = fcat[pp "Syntax error:",pp li,pp ':',d]

>instance PpShowF ParseResult where
>	  ppf (OkParse _ _ x) = pp x
>	  ppf (FailParse li d) = fcat[pp li,pp ':',d]
>         ppf (SyntaxError li d) = fcat[pp "Syntax error:",pp li,pp ':',d]

instance Monad ParseResult where
	  return = OkParse "" emptyLineInfo
	  (OkParse _ _ x) >>= f = f x
	  (FailParse li x) >>= _ = FailParse li x
         (SyntaxError li x) >>= _ = SyntaxError li x

instance MonadPlus ParseResult where
   mzero = FailParse emptyLineInfo (pp "Unsupported alternative")
   (OkParse _ li1 x) `mplus` (OkParse _ _ y) = SyntaxError li1 (pp "Ambiguous grammar")
   (OkParse s li x) `mplus` _ = OkParse s li x
   _ `mplus` OkParse s li r = OkParse s li r
   (SyntaxError li x) `mplus` (SyntaxError _ y) = SyntaxError li (x $$ y)
   (SyntaxError li x) `mplus` _ = SyntaxError li x
   _ `mplus` (SyntaxError li x) = SyntaxError li x
   (FailParse li x) `mplus` (FailParse li' y) = FailParse li (x $$ y)
   _ `mplus` (FailParse li x) = FailParse li x
   (FailParse li x) `mplus` _ = FailParse li x

instance MonadError (LineInfo,Doc) ParseResult where
	  throwError (li,msg) = FailParse li msg
	  catchError (OkParse s li a) _ = OkParse s li a
	  catchError (FailParse li msg) f = f (li,msg)
         catchError (SyntaxError li msg) f = f (li,msg)

>instance Applicative ParseM where
>   pure x = return x
>   f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

>instance Monad ParseM where
>	  return x = ParseM (\s li -> OkParse s li x)
>	  ~(ParseM x) >>= f = ParseM (\s li -> case x s li of
>                               (OkParse s' li' v) -> runParseM (f v) s' li'
>                               (FailParse li2 e) -> FailParse li2 e
>                               (SyntaxError li3 e) -> SyntaxError li3 e)
>         fail str = ParseM (\_ li -> FailParse li (pp str))

>instance Alternative ParseM where
>   empty = mzero
>   (<|>) = mplus

>instance MonadPlus ParseM where
>   mzero = ParseM (\r li -> FailParse li (pp "Unsupported alternative:" 
>                                            <> pp (take 15 r)))
>   ~(ParseM x) `mplus` ~(ParseM y) = ParseM (\s li -> case x s li of
>                                      (OkParse s' lio r) -> OkParse s' lio r
>                                      (FailParse li2 d2) -> case y s li of
>                                            (OkParse s'' lz r') -> OkParse s'' lz r'
>                                            (FailParse li3 d3) -> if li2 >= li3 then FailParse li2 d2 else FailParse li3 d3
>                                            (SyntaxError li3 d3) -> SyntaxError li3 d3
>                                      (SyntaxError li2 d2) -> case y s li of
>                                            (OkParse s' lz r'') -> OkParse s' lz r''
>                                            (SyntaxError li3 d3) -> if li2 >= li3 then SyntaxError li2 d2 else SyntaxError li3 d3
>                                            (FailParse {}) -> SyntaxError li2 d2)

>instance MonadError Doc ParseM where
>	  throwError z = ParseM (\ _ li -> FailParse li z)
>	  catchError ~(ParseM x) f = ParseM (\ s li -> case x s li of
>                          (FailParse li' z) -> runParseM (f z) s li'
>                          (SyntaxError  li' z) -> runParseM (f z) s li'
>                          (OkParse s' li2 z) -> OkParse s' li2 z)
