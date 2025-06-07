>module Math.Matrix.Polynomial where
>import Data.List
> 
>data Poly a = Poly { coefficients :: [a] }

>instance (Show a, Num a, Eq a) => Show (Poly a) where
>  show (Poly lst) = intercalate " + " $
>    zipWith (\ a i -> show a ++ showPower i) lst [0..]
>   where showPower 0 = ""
>         showPower 1 = "x"
>         showPower i = "x^" ++ show i

>zipCoefficient :: (Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
>zipCoefficient f (c:d) (c':d') = f c c' : zipCoefficient f d d'
>zipCoefficient f (c:d) [] = f c 0 : zipCoefficient f d []
>zipCoefficient f [] (c':d') = f 0 c' : zipCoefficient f [] d'
>zipCoefficient _ [] [] = []

>evalPoly :: (Num a) => Poly a -> a -> a
>evalPoly (Poly (c:lst)) x = c + x * evalPoly (Poly lst) x
>evalPoly (Poly []) _ = 0


>instance (Num a) => Num (Poly a) where
>  (Poly lst) + (Poly lst') = Poly $ zipCoefficient (+) lst lst'
>  (Poly lst) - (Poly lst') = Poly $ zipCoefficient (-) lst lst'
>  negate (Poly lst) = Poly $ map negate lst
>  abs (Poly lst) = Poly $ map abs lst
>  signum (Poly lst) = Poly $ map signum lst
>  fromInteger i = Poly [fromInteger i]

