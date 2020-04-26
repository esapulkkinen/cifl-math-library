>{-# LANGUAGE LambdaCase, DeriveDataTypeable, DeriveGeneric, OverloadedStrings, OverloadedLists #-}
>module Math.Tools.Lexer where
>import Prelude hiding (span, length,any, null)
>import GHC.Generics
>import Data.Data
>import Data.Text (Text, cons, uncons,span, length, unpack, pack,any, null)
>import qualified Data.Text.Read as TRead
>import qualified Data.Text as Text
>import Data.Typeable
>import Data.Char
>import Math.Tools.ParseMonad
>import Math.Tools.LineInfo
>import Math.Tools.PrettyP
>data Token = EOF
>           | Number !Integer
>           | FloatNumber !Double
>           | StringLit !Text
>           | Identifier !Text
>           | Operator !Text
>           | WhiteSpace !Text
>           | Linefeed
>           | Comma
>           | Semicolon
> --          | OpenParen | CloseParen
>           | OpenBrace | CloseBrace
>           | OpenBracket | CloseBracket
>           | OpenParenthesis | CloseParenthesis
>   deriving (Eq, Typeable, Data, Generic)

>instance PpShow Token where
>  pp EOF = pp ("EOF" :: Text)
>  pp (Number n) = pp n
>  pp (FloatNumber n) = pp n
>  pp (StringLit t) = pp t
>  pp (Identifier i) = pp i
>  pp (Operator t) = pp t
>  pp (WhiteSpace t) = pp t
>  pp Linefeed = pp ("\n" :: Text)
>  pp Comma = pp ','
>  pp Semicolon = pp ';'
> -- pp OpenParen = pp '('
> -- pp CloseParen = pp ')'
>  pp OpenBrace = pp '{'
>  pp CloseBrace = pp '}'
>  pp OpenBracket = pp '['
>  pp CloseBracket = pp ']'
>  pp OpenParenthesis = pp '('
>  pp CloseParenthesis = pp ')'

>tokenLexer :: t -> Int -> (t -> ParseM a) -> ParseM a
>tokenLexer tok len cont = ParseM $ \r li -> runParseM (cont tok) r (add_to_column len li)

>lexer :: (Token -> ParseM a) -> ParseM a
>lexer cont = ParseM $ \inp -> case uncons inp of
>    Nothing -> tokenLexer EOF 0 cont `runParseM` Text.empty
>    Just ('\n',r) -> \line -> runParseM (cont Linefeed) r $! next_line line
>    Just ('"',r) | (str,s') <- span (/= '"') r ->
>          tokenLexer (StringLit str) (length str + 1) cont `runParseM` (Text.tail s')
>    Just ('{',r) -> tokenLexer OpenBrace 1 cont `runParseM` r
>    Just ('}',r) -> tokenLexer CloseBrace 1 cont `runParseM` r
>    Just ('[',r) -> tokenLexer OpenBracket 1 cont `runParseM` r
>    Just (']',r) -> tokenLexer CloseBracket 1 cont `runParseM` r
>    Just ('(',r) -> tokenLexer OpenParenthesis 1 cont `runParseM` r
>    Just (')',r) -> tokenLexer CloseParenthesis 1 cont `runParseM` r
>    Just (',',r) -> tokenLexer Comma 1 cont `runParseM` r
>    Just (';',r) -> tokenLexer Semicolon 1 cont `runParseM` r
>    Just ('0',r1) | Just ('x',r) <- Text.uncons r1, (numstr,s') <- Text.span isHexDigit r
>                -> tokenLexer (Number $ hexToInt numstr 0) (Text.length numstr) cont `runParseM` s'
>    Just ('0',r) | (numstr,s') <- Text.span isOctDigit r, not (Text.null s' || Text.head s' == '.')
>                -- condition is to avoid floating point numbers be interpreted as octal.
>                -> tokenLexer (Number $ octToInt numstr 0) (Text.length numstr) cont `runParseM` s'
>    Just ('-',r) | (tok,s') <- span (\x -> isDigit x || x == '.') r ->
>              tokenLexer (val tok) (length tok + 1) cont `runParseM` s'
>               where val tok | any (=='.') tok, Right (d,_) <- TRead.double tok = FloatNumber (negate d)
>                             | Right (d,_) <- TRead.decimal tok = Number (negate d)
>                             | otherwise = error "bug in lexer"
>    Just (c,r) | isIdentifierChar c,
>                 (tok,s') <- span isIdentifierContinuationChar (cons c r) ->
>                   tokenLexer (Identifier tok) (length tok) cont `runParseM` s'
>               | isOperatorChar c,
>                 (tok,s') <- span isOperatorChar (cons c r) ->
>                   tokenLexer (Operator tok) (length tok) cont `runParseM` s'
>               | isDigit c
>                 , (tok,s') <- span (\x -> isDigit x || x == '.') (cons c r)
>                 , any (=='.') tok
>                 , Right (d,s'') <- TRead.double tok
>                 , null s'' ->
>                     tokenLexer (FloatNumber d) (length tok) cont `runParseM` s'
>               | isDigit c
>                 , (tok,s') <- span isDigit (cons c r)
>                 , Right (d,s'') <- TRead.decimal tok
>                 , null s'' -> tokenLexer (Number d) (length tok) cont `runParseM` s'
>               | isSpace c, (tok,s') <- span isSpace (cons c r) ->
>                   tokenLexer (WhiteSpace tok) (length tok) cont `runParseM` s'
>               | otherwise -> fail $ "cannot classify character: '" ++ show c ++ "'"

>isOperatorChar :: Char -> Bool
>isOperatorChar ch = ch `elem` ("!@#$%&/=?+^~*-" :: String)

>isIdentifierChar :: Char -> Bool
>isIdentifierChar ch = isLetter ch
>
>isIdentifierContinuationChar :: Char -> Bool
>isIdentifierContinuationChar ch = isLetter ch || isNumber ch || ch == '_'

>hexToInt :: Text -> Integer -> Integer
>hexToInt str = case uncons str of
>   Nothing -> id
>   Just (c,cr) | c `elem` ("abcdef" :: String) -> hexToInt cr .
>                                     (\i -> i + toInteger (ord c - ord 'a')) . (*16)
>               | c `elem` ("ABCDEF" :: String) -> hexToInt cr .
>                         (\i -> i + toInteger (ord c - ord 'A')) . (*16)
>               | otherwise -> hexToInt cr .
>                             (\i -> i + toInteger (ord c - ord '0')) . (*16)

>octToInt :: Text -> Integer -> Integer
>octToInt str = case uncons str of
>       Nothing -> id
>       Just (c,cr) -> octToInt cr . (\i -> i + toInteger (ord c - ord '0')) . (*8)
