>{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses #-}
>module Math.Matrix.Bitmap where
>import qualified Data.Array as A
>import Data.Array (Array, Ix, array, range, bounds, ixmap, (!), indices)
>import Math.Matrix.Interface
>import qualified Math.Number.Stream as Stream
>import qualified Data.ByteString as ByteString
>import qualified Data.ByteString.Char8 as ByteStringChar8
>import qualified Data.Word as Word
>import qualified System.IO as SIO
>
>type Bitmap i j a = ((Array i :*: Array j) a)
>type Colortable = Array Word.Word8 ByteString.ByteString

>-- | PPM format <http://netpbm.sourceforge.net/doc/ppm.html>
>bitmapToPPM :: (Ix i, Ix j, Show j, Show i) => Colortable -> Bitmap i j Word.Word8
> -> ByteString.ByteString
>bitmapToPPM colortable (Matrix bm) = packedHeader `mappend` mconcat bitmapString
>   where bmbound = bounds bm
>         bitmapString = [color (bm ! i ! j)
>                        | i <- range bmbound,
>                          j <- range (bounds (bm ! i))]
>         header lst = "P6 " ++ show (snd $ bounds (bm ! fst bmbound))
>                      ++ " " ++ show (snd bmbound) ++ " 255\n"
>         packedHeader = ByteString.pack $!
>                map (toEnum . fromEnum) (header bitmapString)
>         color i = colortable ! i
>         black = ByteString.pack $! [0,0,0]

>writePPMFile :: FilePath -> ByteString.ByteString -> IO ()
>writePPMFile filename bm = SIO.withFile filename SIO.WriteMode $ \handle -> do
>   ByteStringChar8.hPutStrLn handle bm
>
>triangular_order_bitmap :: Bitmap Integer Integer Word.Word8
>triangular_order_bitmap = Matrix $ 
>  A.listArray (0,299) $ Stream.take 300 $
>   fmap (A.listArray (0,299) . Stream.take 300) $
>   cells (fmap (fromInteger . (`mod` 256)) Stream.triangular_order_matrix)

>default_colortable :: Colortable
>default_colortable = array (0,255) $!
>            [(i,ByteString.pack $! [(255-2*i),(255-2*i),i])
>            | i <- [0..254]]
>           ++ [(255,ByteString.pack $! [0,0,0])]
>
>instance (Ix i, Ix j) => Transposable (Array i) (Array j) where
>   transpose (Matrix m) = Matrix $ array yr $
>        [(j,array xr [(i, (m ! i) ! j) | i <- range xr]) | j <- range yr]
>     where xr@(x,_) = bounds m
>           yr = bounds (m ! x)

>transpose_ :: (Ix i, Ix j) => Bitmap i j a -> Bitmap j i a
>transpose_ m = (fromArray . ixmap bnds (\ (a,b) -> (b,a)) . toArray) m
>  where (x,x') = bounds (cells m)
>        (y,y') = bounds (cells m ! x)
>        bnds = ((y,x),(y',x'))
>    
>(#) :: (Ix i, Ix j) => Bitmap i j a -> (i,j) -> a
>(Matrix m) # (a,b) = (m A.! a) A.! b

>empty :: (Ix i, Ix j) => (i,i) -> (j,j) -> a -> Bitmap i j a
>empty xr yr v = Matrix $ array xr $
>    [(i, array yr [(j,v) | j <- range yr]) | i <- range xr]

>toArray :: (Ix i, Ix j) => Bitmap i j a -> Array (i,j) a
>toArray (Matrix m) = array ((x,y),(x',y')) $
>       [((i,j),(m ! i) ! j) | i <- range xr, j <- range yr]
>   where xr@(x,x') = bounds m
>         yr@(y,y') = bounds (m ! x)

>fromArray :: (Ix i, Ix j) => Array (i,j) a -> Bitmap i j a
>fromArray a = Matrix $ array (x,x') $
>      [(i,array (y,y') [(j, a ! (i,j)) | j <- range (y,y')])
>       | i <- range (x,x')]
>   where ((x,y),(x',y')) = bounds a

>zipWithBM :: (Ix i, Ix j) => (a -> b -> c)
>          -> Bitmap i j a -> Bitmap i j b -> Bitmap i j c
>zipWithBM f (Matrix m) (Matrix n)
>   | bounds m == bounds n = Matrix $
>           array (bounds m) [(i,array (bounds (m!i))
>                                 [(j,f ((m ! i) ! j) ((n ! i) ! j))
>                                 | j <- indices (m ! i)])
>                              | i <- indices m]
>   | otherwise = error "zipWithBM: Mismatching bounds"


