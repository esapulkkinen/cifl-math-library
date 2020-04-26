>module Math.Tools.ParserInterface where
>import qualified Text.PrettyPrint as Pretty
>import Math.Tools.LineInfo
>import Math.Tools.PrettyP
>import Data.Text

>class (Monad p) => ParserCombinators p where
>  readChar   :: p Char
>  eof :: p ()
>  newline :: p ()
>  readWhile :: (Char -> Bool) -> p Text
>  one_of :: (Char -> Bool) -> p Char
>  require   :: Char -> p Char
>  optional  :: p a -> p (Maybe a)
>  getRemainingInput :: p Text
>  getLineInfo       :: p LineInfo
>  syntaxError :: Pretty.Doc -> p a
