>{-# LANGUAGE LambdaCase, TypeOperators, FlexibleInstances, TemplateHaskell, ScopedTypeVariables, GADTs, FlexibleContexts, Trustworthy #-}
>-- | This module provides QuasiQuoter syntactic sugar for matrices.
>-- based on GHC's "Language.Haskell.TH" quasi quoters.
>--
>-- To use these you should use:
>-- @{-# LANGUAGE QuasiQuotes #-}@
>--
>-- Or corresponding compiler option "-XQuasiQuotes".
>--
>-- The syntax is:
>-- 
>-- @[mat3x4|1 2 3 4;3 4 5 6;5 6 7 8|] :: (Vector3 :*: Vector4) Integer@
>-- 
>-- @[double2x2|3.4 5.6;6.7 8.8|] :: (Vector2 :*: Vector2) Double@
>--
>-- The semicolons can be replaced with new line character to produce
>-- the matrix layout in the code, e.g.:
>-- 
>-- >  [mat4x4|1 1 1 1
>-- >          2 2 2 2
>-- >          3 3 3 3
>-- >          4 4 4 4|]
>-- 
>-- The code supports matrices up to 4x4 matrices.
>-- For larger matrices, you should provide a vector type,
>-- and use e.g.
>-- 
>-- @mat7x3 = parseMat (const () :: Vec 7 (Vec 3 Integer) -> ())@
>-- 
>-- @dbl7x2 = parseDbl (const () :: Vec 13 (Vec 2 Double) -> ())@
>-- 
>module Math.Matrix.QuasiQuoter where
>import Control.Monad
>import Data.Text
>import Math.Tools.Lexer
>import Math.Tools.ParseMonad
>import Math.Tools.LineInfo
>import Math.Tools.ParserInterface
>import Math.Matrix.Interface
>import Math.Matrix.Matrix
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Matrix.Simple
>import qualified Language.Haskell.TH as TH
>import qualified Language.Haskell.TH.Quote as Quote
>import qualified Language.Haskell.TH.Syntax as Syntax

>notQuote :: b -> Syntax.Q a
>notQuote = const $ fail "not supported"
>quoter :: (String -> Syntax.Q Syntax.Exp) -> Quote.QuasiQuoter
>quoter f = Quote.QuasiQuoter f notQuote notQuote notQuote

>intmat :: Quote.QuasiQuoter
>intmat = Quote.QuasiQuoter quoteExpr notQuote notQuote notQuote

>quoteExpr :: String -> Syntax.Q Syntax.Exp
>quoteExpr s = parseNumberMatrix (pack s) >>= Syntax.lift

>-- | Quasiquoters for various matrix sizes
>mat1x1,mat2x1,mat3x1,mat4x1 :: Quote.QuasiQuoter
>mat1x1 = quoter parse1x1mat
>mat2x1 = quoter parse2x1mat
>mat3x1 = quoter parse3x1mat
>mat4x1 = quoter parse4x1mat
>-- | Quasiquoters for various matrix sizes
>mat1x2,mat2x2,mat3x2,mat4x2 :: Quote.QuasiQuoter
>mat1x2 = quoter parse1x2mat
>mat2x2 = quoter parse2x2mat
>mat3x2 = quoter parse3x2mat
>mat4x2 = quoter parse4x2mat

>-- | Quasiquoters for various matrix sizes
>mat1x3,mat2x3,mat3x3,mat4x3 :: Quote.QuasiQuoter
>mat1x3 = quoter parse1x3mat
>mat2x3 = quoter parse2x3mat
>mat3x3 = quoter parse3x3mat
>mat4x3 = quoter parse4x3mat
>-- | Quasiquoters for various matrix sizes
>mat1x4,mat2x4,mat3x4,mat4x4 :: Quote.QuasiQuoter
>mat1x4 = quoter parse1x4mat 
>mat2x4 = quoter parse2x4mat 
>mat3x4 = quoter parse3x4mat 
>mat4x4 = quoter parse4x4mat 

>double1x1 = quoter parse1x1dbl
>double2x1 = quoter parse2x1dbl
>double3x1 = quoter parse3x1dbl
>double4x1 = quoter parse4x1dbl
>double1x2 = quoter parse1x2dbl
>double2x2 = quoter parse2x2dbl
>double3x2 = quoter parse3x2dbl
>double4x2 = quoter parse4x2dbl
>double1x3 = quoter parse1x3dbl
>double2x3 = quoter parse2x3dbl
>double3x3 = quoter parse3x3dbl
>double4x3 = quoter parse4x3dbl
>double1x4 = quoter parse1x4dbl 
>double2x4 = quoter parse2x4dbl 
>double3x4 = quoter parse3x4dbl 
>double4x4 = quoter parse4x4dbl 


>parseMat :: (CoordinateSpace (Scalar (f (g a))),
>            CoordinateSpace (f (g a)),
>            Syntax.Lift (f (g a)),
>            Scalar (Scalar (f (g a))) ~ Integer)
>           => (f (g a) -> ()) -> String -> Syntax.Q Syntax.Exp
>parseMat (t :: f (g a) -> ()) s = do
>   s' <- parseNumberMatrix (pack s)
>   let res :: f (g a)
>       res = s' <!> (listVector,listVector)
>   TH.appE [| Matrix |] (Syntax.lift res)

>parseDbl :: (CoordinateSpace (Scalar (f (g a))),
>            CoordinateSpace (f (g a)),
>            Syntax.Lift (f (g a)),
>            Scalar (Scalar (f (g a))) ~ Double)
>           => (f (g a) -> ()) -> String -> Syntax.Q Syntax.Exp
>parseDbl (t :: f (g a) -> ()) s = do
>   s' <- parseDoubleMatrix (pack s)
>   let res :: f (g a)
>       res = s' <!> (listVector,listVector)
>   TH.appE [| Matrix |] (Syntax.lift res)

>parse1x4mat = parseMat (const () :: Vector1 (Vector4 Integer) -> ())
>parse2x4mat = parseMat (const () :: Vector2 (Vector4 Integer) -> ())
>parse3x4mat = parseMat (const () :: Vector3 (Vector4 Integer) -> ())
>parse4x4mat = parseMat (const () :: Vector4 (Vector4 Integer) -> ())
>parse1x3mat = parseMat (const () :: Vector1 (Vector3 Integer) -> ())
>parse2x3mat = parseMat (const () :: Vector2 (Vector3 Integer) -> ())
>parse3x3mat = parseMat (const () :: Vector3 (Vector3 Integer) -> ())
>parse4x3mat = parseMat (const () :: Vector4 (Vector3 Integer) -> ())
>parse1x2mat = parseMat (const () :: Vector1 (Vector2 Integer) -> ())
>parse2x2mat = parseMat (const () :: Vector2 (Vector2 Integer) -> ())
>parse3x2mat = parseMat (const () :: Vector3 (Vector2 Integer) -> ())
>parse4x2mat = parseMat (const () :: Vector4 (Vector2 Integer) -> ())
>parse1x1mat = parseMat (const () :: Vector1 (Vector1 Integer) -> ())
>parse2x1mat = parseMat (const () :: Vector2 (Vector1 Integer) -> ())
>parse3x1mat = parseMat (const () :: Vector3 (Vector1 Integer) -> ())
>parse4x1mat = parseMat (const () :: Vector4 (Vector1 Integer) -> ())

>parse1x4dbl = parseDbl (const () :: Vector1 (Vector4 Double) -> ())
>parse2x4dbl = parseDbl (const () :: Vector2 (Vector4 Double) -> ())
>parse3x4dbl = parseDbl (const () :: Vector3 (Vector4 Double) -> ())
>parse4x4dbl = parseDbl (const () :: Vector4 (Vector4 Double) -> ())
>parse1x3dbl = parseDbl (const () :: Vector1 (Vector3 Double) -> ())
>parse2x3dbl = parseDbl (const () :: Vector2 (Vector3 Double) -> ())
>parse3x3dbl = parseDbl (const () :: Vector3 (Vector3 Double) -> ())
>parse4x3dbl = parseDbl (const () :: Vector4 (Vector3 Double) -> ())
>parse1x2dbl = parseDbl (const () :: Vector1 (Vector2 Double) -> ())
>parse2x2dbl = parseDbl (const () :: Vector2 (Vector2 Double) -> ())
>parse3x2dbl = parseDbl (const () :: Vector3 (Vector2 Double) -> ())
>parse4x2dbl = parseDbl (const () :: Vector4 (Vector2 Double) -> ())
>parse1x1dbl = parseDbl (const () :: Vector1 (Vector1 Double) -> ())
>parse2x1dbl = parseDbl (const () :: Vector2 (Vector1 Double) -> ())
>parse3x1dbl = parseDbl (const () :: Vector3 (Vector1 Double) -> ())
>parse4x1dbl = parseDbl (const () :: Vector4 (Vector1 Double) -> ())


>   
>parseStringMatrix :: (Monad m) => Text -> m (([] :*: []) Text) 
>parseStringMatrix = parseToMatrix stringLitP
>
>stringLitP (StringLit tok) = return tok
>stringLitP _ = fail "Matrix element is not a string"

>parseDoubleMatrix :: (Monad m) => Text -> m (([] :*: []) Double) 
>parseDoubleMatrix = parseToMatrix floatLitP

>floatLitP (FloatNumber v) = return v
>floatLitP (Number v) = return (fromInteger v)
>floatLitP _ = fail "Matrix element is not a floating point number"

>parseNumberMatrix :: (Monad m) => Text -> m (([] :*: []) Integer)
>parseNumberMatrix = parseToMatrix numberP 

>instance Syntax.Lift ((Vector3 :*: Vector3) Integer) where
>   lift (Matrix x) = [| Matrix |] `TH.appE` [| x |]

>instance (Syntax.Lift a) => Syntax.Lift (Vector1 a) where
>   lift (Vector1 x) = [| Vector1 |] `TH.appE` [| x |]

>instance (Syntax.Lift a) => Syntax.Lift (Vector4 a) where
>   lift (Vector4 x y z t) = [| Vector4 |]
>      `TH.appE` [| x |] `TH.appE` [| y |] `TH.appE` [| z |]
>      `TH.appE` [| t |]

>instance (Syntax.Lift a) => Syntax.Lift (Vector3 a) where
>   lift (Vector3 x y z) = [| Vector3 |]
>      `TH.appE` [| x |] `TH.appE` [| y |] `TH.appE` [| z |]

>instance (Syntax.Lift a) => Syntax.Lift (Vector2 a) where
>   lift (Vector2 x y) = [| Vector2 |]
>     `TH.appE` [| x |] `TH.appE` [| y |]

>instance Syntax.Lift (([] :*: []) Integer) where
>   lift (Matrix f) = [| Matrix |] `TH.appE` [| f |]
>
>numberP (Number tok) = return tok
>numberP _ = fail "Matrix element is not a number"

>parseToMatrix :: (Monad m) => (Token -> ParseM a) -> Text -> m (([] :*: []) a)
>parseToMatrix f s = parseResultToMonad pr
>   where pr = runParseM (parseMatrix f) s emptyLineInfo

>parseMatrix :: (Token -> ParseM a) -> ParseM (([] :*: []) a)
>parseMatrix elemP = parseRow >>= \v -> eof >> return (Matrix v)
>  where parseCol = lexer $ \case
>           Linefeed -> return []
>           Semicolon -> return []
>           (WhiteSpace _) -> parseCol
>           Comma -> parseCol
>           tok -> do
>              v <- elemP tok
>              vr <- parseCol `mplus` return []
>              return (v:vr)
>        parseRow = do
>           lst <- parseCol
>           lst' <- parseRow `mplus` return []
>           return (lst:lst')
