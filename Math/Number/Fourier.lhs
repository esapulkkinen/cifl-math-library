>{-# LANGUAGE Safe #-}
>{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
>module Math.Number.Fourier where
>import safe Data.Complex
>import safe Data.Ratio
>import Math.Matrix.Interface
>import qualified Math.Number.Stream as Stream
>import Math.Number.Stream
> 
>-- | <https://en.wikipedia.org/wiki/Discrete_Fourier_transform>
>dft :: (RealFloat a) => [Complex a] -> [Complex a]
>dft (nums :: [Complex a]) = [func (fromIntegral k) | k <- [0.. pred len]]
>  where len = length nums
>        len' = fromIntegral len
>        coeff = 0 :+ (-2*pi)
>        facts = factors len'
>        func k = Prelude.sum $ do
>           ~(xn,factor) <- zip nums facts
>           return $ xn * exp (factor * k * coeff)

>factors :: (Fractional a) => Integer -> [a]
>factors len = [fromRational (fromIntegral k % fromIntegral len) | k <- [0..pred len]]

>inverse_dft :: (RealFloat a) => [Complex a] -> [Complex a]
>inverse_dft lst = factor %* (map swap $ dft $ map swap lst)
>   where swap (a:+b) = b:+a
>         factor = 1 / fromIntegral (length lst)

>root_of_unity :: (RealFloat a) => Int -> Complex a
>root_of_unity n = exp (0 :+ (-2*pi/fromIntegral n))

>dft_matrix :: (RealFloat a) => Int -> ([] :*: []) (Complex a)
>dft_matrix n = matrix (\i j -> rou ^ (i %. j)) [0..n-1] [0..n-1]
>  where rou = root_of_unity n
