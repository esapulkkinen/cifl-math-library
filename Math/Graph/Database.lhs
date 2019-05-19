>module Math.Graph.Database where
>import Control.Monad
>import Data.Monoid

>data Table a = Table {
>   numberOfColumns :: Integer,
>   columnByName :: String -> Maybe Integer,
>   columns :: Integer -> Endo a }

>inRange :: Integer -> (Integer,Integer) -> Bool
>inRange k (a,b) = k >= a && k < b

>instance Semigroup (Table a) where
>   (<>) = mappend

>instance Monoid (Table a) where
>   mempty = Table 0 (const Nothing) (const $ Endo id)
>   mappend (Table i s f) (Table j t g) = let m = i+j in Table m colsByName cols
>     where m = i + j
>           cols k = if k `inRange` (0,i) then f k
>                  else if k `inRange` (i,m) then g (k-i)
>                  else Endo id
>           colsByName name = s name `mplus` (t name >>= return . (+ i))

>(!!!!) :: Table a -> String -> Maybe (Endo a)
>(!!!!) t name = do
>   i <- columnByName t name
>   return (t !!! i)

>(!!!) :: Table a -> Integer -> Endo a
>(Table icount _ f) !!! i
>  | i `inRange` (0,icount) = f i
>  | otherwise = Endo id
