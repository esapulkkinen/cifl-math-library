>{-# LANGUAGE OverloadedStrings #-}
>module Math.Graph.SVG where
>import qualified Data.Text as T
>import qualified Data.Text.IO as IO
>import Math.Number.Stream
>import qualified Math.Number.Stream as S
>import Control.Applicative
> 
>type Coord = T.Text
>type Stroke = T.Text
>type Fill = T.Text
>type FontFamily = T.Text
>type FontSize = T.Text

>data SVG = Line { x1 :: Coord, y1 :: Coord, x2 :: Coord, y2 :: Coord }
>         | Rect { x :: Coord, y :: Coord, width :: Coord, height :: Coord }
>         | Group { stroke :: Stroke, fill :: Fill, subelems :: [SVG] }
>         | Circle { cx :: Coord, cy :: Coord, r :: Coord }
>         | Ellipse { cx :: Coord, cy :: Coord, rx :: Coord, ry :: Coord }
>         | Text { x :: Coord, y :: Coord, fontfamily :: FontFamily,
>                  fontsize :: FontSize, fill :: Fill, text :: T.Text }
>         | SVG { subelems :: [SVG] }

>attr :: T.Text -> T.Text -> T.Text
>attr key val = key <> "=\"" <> val <> "\" "
>
>tag :: T.Text -> [T.Text] -> T.Text -> T.Text
>tag t attrs body = "<svg:" <> t <> " " <> T.concat attrs <> ">" <> body <> "</svg:" <> t <> ">\n"

>toHTML :: SVG -> T.Text
>toHTML (SVG elems) = tag "svg" [] $ T.concat (fmap toHTML elems)
>toHTML (Group s f elems) = tag "g" [attr "stroke" s, attr "fill" f] $
>   T.concat (fmap toHTML elems)
>toHTML (Text x y ff fs fi te) = tag "text" attrs te
>  where attrs = [attr "x" x, attr "y" y, attr "font-family" ff,
>                 attr "font-size" fs, attr "fill" fi]
>toHTML (Circle cx cy r) = tag "circle" [attr "cx" cx, attr "cy" cy, attr "r" r] ""
>toHTML (Ellipse cx cy rx ry) = tag "ellipse" [attr "cx" cx, attr "cy" cy, attr "rx" rx, attr "ry" ry] ""
>toHTML (Rect x y w h) = tag "rect" attrs "" 
>  where attrs = [attr "x" x, attr "y" y, attr "width" w, attr "height" h]                                 
>toHTML (Line x1 y1 x2 y2) = tag "line" attrs ""
>   where attrs = [attr "x1" x1, attr "y1" y1, attr "x2" x2, attr "y2" y2]

>htmlPrelude :: T.Text
>htmlPrelude = "<html xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\">\n<head></head>\n<body>\n<p><math:math><math:mfrac><math:mn>1</math:mn><math:mrow>1-5<math:mi>z</math:mi>-<math:msub><math:mi>z</math:mi><math:mn>2</math:mn></math:msub></math:mrow></math:mfrac></math:math></p><p><svg:svg width=\"25cm\" height=\"25cm\">"
>
>htmlSuffix :: T.Text
>htmlSuffix = "</svg:svg></p>\n</body>\n</html>\n"

>instance Show SVG where
>   show x = T.unpack $ htmlPrelude <> toHTML x <> htmlSuffix

>writeHTML :: FilePath -> SVG -> IO ()
>writeHTML fname svg = IO.writeFile fname (htmlPrelude <> toHTML svg <> htmlSuffix)

>source :: Stream (Stream Integer)
>source = fmap (fmap (fromIntegral . (`mod` 4))) (1 `div` (1 - 5*s_z - s_z2))

>source2 :: Stream (Integer, Stream (Integer,Integer))
>source2 = fmap (\ ~(i,s) -> (i,liftA2 (,) nonzero_naturals s)) (liftA2 (,) nonzero_naturals source)

>sourceSVG :: Stream (Stream SVG)
>sourceSVG = fmap (\ ~(j,row) -> fmap (\ ~(i,b) -> Circle (T.pack $ show (fromInteger i/3.0) ++ "cm")
>                (T.pack $ show (fromInteger j/3.0) ++ "cm") (b `seq` if b == 0 then "0.01cm"
>                                           else if b == 1 then "0.07cm"
>                                           else "0.15cm")) row) source2

>logoSVG :: SVG
>logoSVG = Group "black" "white" $ fmap (Group "black" "black") $ S.take 40 $ fmap (S.take 40) sourceSVG

>writeLogo :: IO ()
>writeLogo = writeHTML "logo2.svg" logoSVG
