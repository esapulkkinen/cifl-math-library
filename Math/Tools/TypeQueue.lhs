>{-# LANGUAGE PolyKinds, GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeFamilyDependencies, RankNTypes, TypeInType #-}
>module Math.Tools.TypeQueue where
>import Data.Proxy
>import Data.Kind
>
>data Q (t :: k) where
>  PushQ :: Q a -> b -> Q c -> Q (a,b,c)
>  EmptyQ :: Q ()
>{-# COMPLETE PushQ, EmptyQ #-}

>instance (Show (Q a),Show b, Show (Q c)) => Show (Q (a,b,c)) where
>  show (PushQ a b c) = show a ++ " " ++ show b ++ " " ++ show c

>instance Show (Q ()) where
>  show EmptyQ = show "()"

>push_proxy :: Proxy a -> Proxy (a',b',c') -> Proxy (Push a (a',b',c'))
>push_proxy Proxy Proxy = Proxy

>push_front :: a -> Q t -> Q (Push a t)
>push_front a (PushQ EmptyQ x q) = PushQ (PushQ EmptyQ a EmptyQ) x q
>push_front a (PushQ (PushQ b' x' q') x q)
> = PushQ (push_front a (PushQ b' x' q')) x q
>
>type family Push a q = r 
>type instance Push a ((),x,c) = (( (),a,() ), x, c)
>type instance Push a ((b',x',c'),x,c) = (Push a (b',x',c'),x,c)

>type family PushB q a
>type instance PushB (c,x,()) a = (c,x,( (), a, () ))
>type instance PushB (c,x,(c',x',b')) a = (c,x,PushB (c',x',b') a)

>push_back :: Q t -> a -> Q (PushB t a)
>push_back (PushQ q x EmptyQ) a = PushQ q x (PushQ EmptyQ a EmptyQ)
>push_back (PushQ q x (PushQ q' x' b')) a = PushQ q x (push_back (PushQ q' x' b') a)

>type family First t 
>type instance First ((),t,_) = t
>type instance First ((a,b,c),_,_)  = First (a,b,c)

>class QueueOps q where
>  show_first :: (Show (First t)) => q t -> String
>  show_last  :: (Show (Last t)) => q t -> String
>  firstQ :: q t -> t
>  lastQ  :: q t -> t

instance QueueOps Q where
   show_first (PushQ EmptyQ a _) = show a
   show_first (PushQ (PushQ x a' y) a b) = show_first (PushQ x a' y)
   show_last (PushQ _ a EmptyQ) = show a
   show_last (PushQ b a (PushQ x a' y)) = show_last (PushQ x a' y)
   firstQ (PushQ EmptyQ x _) = x
   firstQ (PushQ z@(PushQ _ _ _) _ _) = firstQ z
   firstQ EmptyQ = undefined
   lastQ (PushQ _ a EmptyQ) = a
   lastQ (PushQ _ a z@(PushQ _ _ _)) = lastQ z
   lastQ EmptyQ = undefined

>type family Last t
>type instance Last (a,t,()) = t
>type instance Last (a,t,(b,c,d))  = Last (b,c,d)

>type family Reverse a
>type instance Reverse (a,b,c) = (Reverse c, b, Reverse a)
>type instance Reverse () = ()

>reverseQ :: Q a -> Q (Reverse a)
>reverseQ (PushQ a b c) = PushQ (reverseQ c) b (reverseQ a)
>reverseQ EmptyQ = EmptyQ
