>{-# LANGUAGE DataKinds #-}
>module Math.Matrix.Fractal where
>import Math.Number.Interface
>import Math.Matrix.Interface
>import Math.Matrix.Bitmap
>import Math.Matrix.SIMD
>import Data.Array
>import Data.Complex
>import Data.Word as Word

>mandelbrot_iter :: Complex Double -> Complex Double -> Complex Double
>mandelbrot_iter c x = x*x+c

>mandelbrot_iters :: Word.Word32 -> Complex Double -> Complex Double -> Word.Word32
>mandelbrot_iters 0 _ _ = 0
>mandelbrot_iters i c x | magnitude x > 2.0 = i
>                       | otherwise = mandelbrot_iters (pred i) c (mandelbrot_iter c x)

>mandelbrot_bitmap :: (Word.Word32, Word.Word32)
>    -> Complex Double -> Complex Double -> Word.Word32
>    -> Bitmap Word.Word32 Word.Word32 Word.Word32
>mandelbrot_bitmap (wid,hei) tl br maxiters = Matrix $ listArray (0,vertlen) $
>     [listArray (0,horizlen) [mandelbrot_iters maxiters (c:+ci) 0 | c <- horiz] | ci <- vert]
>  where step = ((realPart br - realPart tl) / fromIntegral wid)
>            :+ ((imagPart br - imagPart tl) / fromIntegral hei)
>        horiz = [realPart tl, realPart tl + realPart step .. realPart br]
>        vert  = [imagPart tl, imagPart tl + imagPart step .. imagPart br]
>        horizlen = fromIntegral (length horiz - 1)
>        vertlen = fromIntegral (length vert - 1)


>fullMandelbrot :: (Word.Word32,Word.Word32) -> Bitmap Word.Word32 Word.Word32 Word.Word8
>fullMandelbrot (xs,ys) = fmap fromIntegral $ mandelbrot_bitmap (xs,ys) ((-2.0) :+ (-2.0)) (2.0 :+ 2.0) 255

>writeMandelbrot :: (Word.Word32,Word.Word32) -> IO ()
>writeMandelbrot (xs,ys) = do
>  let bm = fullMandelbrot (xs,ys)
>  let ppm = bitmapToPPM default_colortable bm
>  writePPMFile "mandelbrot.ppm" ppm
