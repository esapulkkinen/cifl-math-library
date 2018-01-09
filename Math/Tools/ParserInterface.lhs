>module Math.Tools.ParserInterface where
>import Math.Tools.LineInfo
>import Math.Tools.PrettyP

>class (Monad p) => ParserCombinators p where
>  readChar   :: p Char
>  eof :: p ()
>  newline :: p ()
>  readWhile :: (Char -> Bool) -> p String
>  one_of :: (Char -> Bool) -> p Char
>  require   :: Char -> p Char
>  optional  :: p a -> p (Maybe a)
>  getRemainingInput :: p String
>  getLineInfo       :: p LineInfo
>  syntaxError :: Doc -> p a