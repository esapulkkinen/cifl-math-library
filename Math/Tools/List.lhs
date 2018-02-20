>{-# LANGUAGE Safe #-}
>module Math.Tools.List where

>infixr 8 `interleave`

The interleave first pattern must not match both arguments,
otherwise infinite list interleaving doesn't work. Note also that
interleave swaps arguments after every element.

>interleave :: [a] -> [a] -> [a]
>interleave (c:cr) lst = (c:interleave lst cr)
>interleave [] lst = lst

>interleave2 :: [[a]] -> [a]
>interleave2 = foldr interleave []

>-- | shift rotates the first k elements (and leaves the rest of the list intact)

>shift :: Int -> [a] -> [a]
>shift _ []     = []
>shift k (c:cr) = take k cr ++ [c] ++ drop k cr


>-- | From discussion in Haskell-Cafe mailing list by Koen Claessen:
>-- <https://mail.haskell.org/pipermail/haskell-cafe/2002-June/003122.html>

>selections :: [a] -> [(a,[a])]
>selections []     = []
>selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

>twice_selections :: [a] -> [(a,a,[a])]
>twice_selections lst = [(x,y,r) | (x,xr) <- selections lst, (y,r)  <- selections xr]

>-- | From discussion in Haskell-Cafe mailing list by Koen Claessen:
>-- <https://mail.haskell.org/pipermail/haskell-cafe/2002-June/003122.html>

>permutations :: [a] -> [[a]]
>permutations [] = [[]]
>permutations xs = [ y : zs | (y,ys) <- selections xs,
>                             zs <- permutations ys ]
