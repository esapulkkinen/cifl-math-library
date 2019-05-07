>{-# LANGUAGE LambdaCase, DeriveDataTypeable, DeriveGeneric #-}
>module Math.Tools.Lexer where
>import GHC.Generics
>import Data.Data
>import Data.Typeable
>import Data.Char
>import Math.Tools.ParseMonad
>import Math.Tools.LineInfo

>data Token = EOF
>           | Number Integer
>           | FloatNumber Double
>           | StringLit String
>           | Identifier String
>           | Operator String
>           | WhiteSpace String
>           | Linefeed
>           | Comma
>           | Semicolon
>           | OpenParen | CloseParen
>           | OpenBrace | CloseBrace
>           | OpenBracket | CloseBracket
>           | OpenParenthesis | CloseParenthesis
>   deriving (Eq, Typeable, Data, Generic)

>tokenLexer :: t -> Int -> (t -> ParseM a) -> ParseM a
>tokenLexer tok len cont = ParseM $ \r li -> runParseM (cont tok) r (add_to_column len li)

>lexer :: (Token -> ParseM a) -> ParseM a
>lexer cont = ParseM $ \case
>    [] -> tokenLexer EOF 0 cont `runParseM` []
>    ('\n':r) -> \line -> runParseM (cont Linefeed) r $! next_line line
>    ('"':r) | (str,s') <- span (/= '"') r ->
>          tokenLexer (StringLit str) (length str + 1) cont `runParseM` (tail s')
>    ('{':r) -> tokenLexer OpenBrace 1 cont `runParseM` r
>    ('}':r) -> tokenLexer CloseBrace 1 cont `runParseM` r
>    ('[':r) -> tokenLexer OpenBracket 1 cont `runParseM` r
>    (']':r) -> tokenLexer CloseBracket 1 cont `runParseM` r
>    ('(':r) -> tokenLexer OpenParenthesis 1 cont `runParseM` r
>    (')':r) -> tokenLexer CloseParenthesis 1 cont `runParseM` r
>    (',':r) -> tokenLexer Comma 1 cont `runParseM` r
>    (';':r) -> tokenLexer Semicolon 1 cont `runParseM` r
>    ('0':'x':r) | (numstr,s') <- span isHexDigit r
>                -> tokenLexer (Number $ hexToInt numstr 0) (length numstr) cont `runParseM` s'
>    ('0':r) | (numstr,s') <- span isOctDigit r, not (null s' || head s' == '.')
>                -- condition is to avoid floating point numbers be interpreted as octal.
>                -> tokenLexer (Number $ octToInt numstr 0) (length numstr) cont `runParseM` s'
>    ('-':r) | (tok,s') <- span (\x -> isDigit x || x == '.') r ->
>              tokenLexer (val tok) (length tok + 1) cont `runParseM` s'
>               where val tok | '.' `elem` tok = FloatNumber (negate $ read tok)
>                             | otherwise      = Number      (negate $ read tok)
>    (c:r) | isIdentifierChar c,
>            (tok,s') <- span isIdentifierContinuationChar (c:r) ->
>              tokenLexer (Identifier tok) (length tok) cont `runParseM` s'
>          | isOperatorChar c,
>             (tok,s') <- span isOperatorChar (c:r) ->
>               tokenLexer (Operator tok) (length tok) cont `runParseM` s'
>          | isDigit c, (tok,s') <- span (\x -> isDigit x || x == '.') (c:r), '.' `elem` tok ->
>              tokenLexer (FloatNumber (read tok)) (length tok) cont `runParseM` s'
>          | isDigit c, (tok,s') <- span isDigit (c:r) ->
>              tokenLexer (Number (read tok)) (length tok) cont `runParseM` s'
>          | isSpace c, (tok,s') <- span isSpace (c:r) ->
>              tokenLexer (WhiteSpace tok) (length tok) cont `runParseM` s'
>          | otherwise -> fail $ "cannot classify character: '" ++ show c ++ "'"

>isOperatorChar :: Char -> Bool
>isOperatorChar ch = ch `elem` "!@#$%&/=?+^~*-"

>isIdentifierChar :: Char -> Bool
>isIdentifierChar ch = isLetter ch
>
>isIdentifierContinuationChar :: Char -> Bool
>isIdentifierContinuationChar ch = isLetter ch || isNumber ch || ch == '_'

>hexToInt :: String -> Integer -> Integer
>hexToInt [] = id
>hexToInt (c:cr)
>   | c `elem` "abcdef" = hexToInt cr .
>                         (\i -> i + toInteger (ord c - ord 'a')) . (*16)
>   | c `elem` "ABCDEF" = hexToInt cr .
>                         (\i -> i + toInteger (ord c - ord 'A')) . (*16)
>   | otherwise = hexToInt cr .
>                 (\i -> i + toInteger (ord c - ord '0')) . (*16)

>octToInt :: String -> Integer -> Integer
>octToInt [] = id
>octToInt (c:cr) = octToInt cr . (\i -> i + toInteger (ord c - ord '0')) . (*8)
