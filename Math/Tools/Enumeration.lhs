>{-# OPTIONS_HADDOCK hide, prune #-}
>module Math.Tools.Enumeration where


>-- ^ This is a variant of idea presented in:
>-- <http://math.andrej.com/2008/11/21/a-haskell-monad-for-infinite-search-in-finite-time/>

which lists all elements of the equivalence class.

>newtype Enumerable a = Enumerable { enumerate :: (a -> Bool) -> [a] }

>elements :: Enumerable a -> (a -> Bool) -> [a]
>elements xs p = filter p (enumerate xs p)

>forsome :: Enumerable a -> (a -> Bool) -> Bool
>forsome e p = or $ map p $ enumerate e p

>forevery :: Enumerable a -> (a -> Bool) -> Bool
>forevery e p = not $ forsome e $ not . p

>optional :: a -> Enumerable a
>optional x = fiber [x]

>singleton :: a -> Enumerable a
>singleton x = domain [x]

>domain :: [a] -> Enumerable a
>domain x = Enumerable $ const x

>fiber :: [a] -> Enumerable a
>fiber lst = Enumerable $ \p -> filter p lst

>image :: (a -> b) -> Enumerable a -> Enumerable b
>image f e = Enumerable $ \p -> map f (enumerate e (p . f))

>bigUnion :: Enumerable (Enumerable a) -> Enumerable a
>bigUnion e = Enumerable $ \p -> concatMap (\e' -> enumerate e' p)
>                                (enumerate e (\q -> forsome q p))

>unionLst :: [Enumerable a] -> Enumerable a
>unionLst lst = bigUnion (fiber lst)

>union :: Enumerable a -> Enumerable a -> Enumerable a
>union x y = unionLst [x,y]


instance Monad Enumerable where
  return x = domain [x]
  xs >>= f = bigUnion (image f xs)

times :: Enumerable a -> Enumerable b -> Enumerable (a,b)
times xs ys = do x <- xs
                 y <- ys
                 return (x,y)

>truthValues :: Enumerable Bool
>truthValues = domain [True,False]

cantor :: Enumerable [Bool]
cantor = sequence $ repeat truthValues
