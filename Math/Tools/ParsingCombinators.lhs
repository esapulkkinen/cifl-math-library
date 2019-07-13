>{-# LANGUAGE TypeOperators #-}
>module Math.Tools.ParsingCombinators where
>import Prelude hiding ((.),id)
>import Control.Category
>import Control.Monad.Fix (fix)
>import Math.Tools.Isomorphism
>import Math.Tools.LineInfo
>import qualified Text.PrettyPrint as Pretty
>import Math.Tools.PrettyP
>import Math.Tools.Arrow

>type Doc = Pretty.Doc

>errorMessage :: ParseResult a -> Maybe Doc
>errorMessage (Parsed _ _ (Right d)) = Just d
>errorMessage _ = Nothing

>data ParseResult a = Parsed { unparsedString :: String, 
>                               lineInfoOf ::  !LineInfo,
>                               parsedValue ::  Either a Doc }

>okparse s li v = Parsed s li (Left v)
>syntaxerror s li d = Parsed s li (Right d)

>mapValue :: (Either a Doc) :==: (Either a Doc) -> (ParseResult a) :==: (ParseResult a)
>mapValue f = embedIso open (id <**> f)

>mapLineInfo :: LineInfo :==: LineInfo -> (ParseResult a) :==: (ParseResult a)
>mapLineInfo f = embedIso open ((id <**> f) <**> id)
                 
>mapString :: String :==: String -> (ParseResult a) :==: (ParseResult a)
>mapString f = embedIso open ((f <**> id) <**> id)              

>mapError :: Doc :==: Doc -> (ParseResult a) :==: (ParseResult a)
>mapError f = embedIso open (id <**> (id <||> f))
              
>open :: (ParseResult a) :==: ((String,LineInfo),(Either a Doc))
>open = Iso open' close'
>  where open' (Parsed s li d) = ((s,li),d)  
>        close' ((s,li),d) = Parsed s li d

>instance Functor ParseResult where
>   fmap f (Parsed s li (Left x)) = Parsed s li (Left (f x))
>   fmap _ (Parsed s li (Right d)) = Parsed s li (Right d)

>data ParsingA i o = ParsingA { 
>    runParsingA  :: (ParseResult i) :==: (ParseResult o)
>  }

instance FailureArrow ParsingA Doc where

instance ScopedFailureArrow ParsingA 

>instance Isomorphic ParsingA where
>   iso (ParsingA i) = Iso (fwd $ runIso i) (fwd $ runIsoInverse i)
>     where fwd x v = case x (Parsed "" emptyLineInfo (Left v)) of
>                     (Parsed s li (Left v')) -> v'
>                     (Parsed s li (Right d)) -> error "Syntax error"

>instance Category ParsingA where
>   id = ParsingA id
>   (ParsingA f) . (ParsingA g) = ParsingA (f . g)

>instance Groupoid ParsingA where
>   invertA (ParsingA f) = ParsingA (invertA f)

>instance BiArrow ParsingA where
>   f <-> g = ParsingA $ mapIso (f <-> g)

>nextColumn :: ParsingA i i
>nextColumn = ParsingA (mapLineInfo nextColumnIso)

>readChar :: ParsingA () Char
>readChar = ParsingA (Iso readChar' showChar') >>> nextColumn
>   where readChar' (Parsed (c:cr) li (Left ())) = okparse cr li c
>         readChar' (Parsed [] li (Left ())) = syntaxerror [] li (pp "Unexpected end of file")
>         readChar' (Parsed lst li (Right d)) = syntaxerror lst li d
>         showChar' (Parsed cr li (Left c)) = okparse (c:cr) li ()
>         showChar' (Parsed [] li (Right _)) = okparse [] li ()
>         showChar' (Parsed lst li (Right d)) = syntaxerror lst li d

newline = (readChar <**> returnIso '\n') >>> equalIso


>eof :: ParsingA () ()
>eof = ParsingA (Iso readEof showEof)
>   where readEof (Parsed [] li (Left _)) = okparse [] li ()
>         readEof (Parsed z li (Left _))  = syntaxerror z li (pp "Expected eof")
>         readEof z@(Parsed _ _ (Right _)) = z
>         showEof (Parsed [] li (Right d)) = syntaxerror [] li d
>         showEof (Parsed z li (Right d)) = okparse z li ()
>         showEof (Parsed z li (Left v)) = okparse z li v

>readWhile :: (Char -> Bool) -> ParsingA () String
>readWhile pred = ParsingA (Iso readWhile' showWhile')
>  where readWhile' (Parsed z li (Right d)) = syntaxerror z li d
>        readWhile' (Parsed lst li (Left inp)) = if null s then syntaxerror r li err
>                                                    else okparse r newcol s
>                 where (s,r) = span pred lst
>                       err = pp "Unexpected character in input:" <> ch
>                       ch = if null r then pp "<eof>" else pp_list [head r]
>                       newcol = add_to_column (length s) li
>        showWhile' (Parsed [] li (Right err)) = syntaxerror [] li err
>        showWhile' (Parsed z@(c:_) li (Right err)) 
>                        | pred c = syntaxerror z li err
>                        | otherwise = okparse z li ()
>        showWhile' (Parsed r newcol (Left s)) = okparse (s++r) oldcol ()
>               where oldcol = add_to_column (negate (length s)) newcol


