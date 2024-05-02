>module Math.Matrix.FFT where
>import Data.Array
>import Data.Complex
>import Data.Ratio
>
>-- | FFT algorithm following Cooley-Tukey FFT algorithm
>-- <https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm>
>fft_impl :: (Ix i, Integral i, RealFloat e) => (i,i) -> i -> Array i (Complex e) -> Array i (Complex e)
>fft_impl (a,b) step ary = if n <= 0 then array (a,a) [(a,ary ! a)]
>                           else result
>  where n = b - a
>        middle = a + n `div` 2
>        s2 = 2*step
>        evenfft = fft_impl (a,middle) s2 ary
>        oddfft  = fft_impl (a+step, middle) s2 ary
>        pairfft = array (a,middle) [(k,(p+q, p-q)) | k <- [a..middle-1],
>                                                   let p = evenfft ! k,
>                                                   let q = (oddfft ! k) * root_of_unity k n]
>        result = array (a,b) [(k,if k >= middle then snd (pairfft ! (k - middle + a))
>                                               else fst (pairfft ! k))
>                             | k <- [a..b]]

>fft :: (Ix i, Integral i, RealFloat e) => Array i (Complex e) -> Array i (Complex e)
>fft ary = fft_impl (bounds ary) 1 ary

>root_of_unity :: (RealFloat e, Integral i) => i -> i -> Complex e
>root_of_unity k n = exp ((0 :+ (2*pi)) * fromRational (toInteger k % toInteger n))

>testdata :: Array Integer (Complex Double)
>testdata = array (0,10) [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
