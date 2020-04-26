>module Math.Matrix.Fractal where
>import Math.Matrix.SIMD

>mandelbrot :: FVec2 Double -> [FVec2 Double] -> [FVec2 Double]
>mandelbrot c lst = map (\x -> x*x+c) lst

>mandelbrot_iters :: Integer -> FVec2 Double -> [FVec2 Double] -> [FVec2 Double]
>mandelbrot_iters 0 c lst = lst
>mandelbrot_iters i c lst = mandelbrot c $ mandelbrot_iters (i-1) c lst
