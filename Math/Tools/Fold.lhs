>{-# LANGUAGE RankNTypes, GADTs #-}
>{-# OPTIONS_HADDOCK hide, prune #-}
>-- | This is based on Wadler at
>-- <http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt>
>module Math.Tools.Fold where
>import Math.Tools.PrettyP

>data LFix f = LIn { unlin :: forall x. (f x -> x) -> x }
>data GFix f = forall x. GIn (x -> f x) x

>type Nat = LFix Maybe

>fold :: (f x -> x) -> LFix f -> x
>fold p (LIn fx) = fx p

>unfold :: (x -> f x) -> x -> GFix f
>unfold = GIn

>lin :: (Functor f) => f (LFix f) -> LFix f
>lin s = LIn (\k -> (k (fmap (fold k) s)))

>lout :: (Functor f) => LFix f -> f (LFix f)
>lout = fold (fmap lin)

>gout :: (Functor f) => GFix f -> f (GFix f)
>gout t = case t of { (GIn k x) -> fmap (unfold k) (k x) }

>gin :: (Functor f) => f (GFix f) -> GFix f
>gin = unfold (fmap gout)

>least_to_greatest :: (Functor f) => LFix f -> GFix f
>least_to_greatest = fold gin

>least_to_greatest' :: (Functor f) => LFix f -> GFix f
>least_to_greatest' = unfold lout


{-# RULES
    "fold/lin" fold lin = id
 #-}
{-# RULES
    "linout/reduce" lout . lin = id
 #-}

recurse :: (f (x,LFix f) -> x) -> LFix f -> (x,LFix f)
recurse g = fold (\ctx -> (g ctx, inj (fmap snd) ctx))
