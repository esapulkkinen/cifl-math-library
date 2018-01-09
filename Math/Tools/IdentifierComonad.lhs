>module Math.Tools.IdentifierComonad where
>import Math.Tools.CoMonad

>data IdentifierC a = IdentifierC (Integer,a)

>runIdentifierC :: (IdentifierC a -> b) -> a -> b
>runIdentifierC f a = f (IdentifierC (0,a))

>getNumber :: IdentifierC a -> Integer
>getNumber (IdentifierC (i,_)) = i

>instance Functor IdentifierC where
>   fmap f (IdentifierC (x,a)) = IdentifierC (x,f a)

>instance Comonad IdentifierC where
>   extract (IdentifierC (_,x)) = x
>   duplicate (IdentifierC (c,x)) = IdentifierC (c*2,IdentifierC (succ (c*2),x))
