>{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses #-}
>module Math.Matrix.Bitmap where
>import qualified Data.Array as A
>import Data.Array
>import Math.Matrix.Interface

>type Bitmap i j a = ((Array i :*: Array j) a)

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

